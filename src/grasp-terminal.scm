(module-name grasp-terminal)
(module-compile-options main: #t)

(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-property))
(import (language define-type))
(import (language define-object))
(import (language define-cache))
(import (language define-parameter))
(import (language keyword-arguments))
(import (language examples))
(import (language assert))
(import (language infix))
(import (language match))
(import (language for))
(import (language while))
(import (language mapping))
(import (language fundamental))
(import (utils server))
(import (utils conversions))
(import (utils functions))
(import (utils print))

(import (editor interfaces painting))
(import (editor interfaces elements))
;;(import (editor types spaces))
(import (editor document cursor))
(import (editor types primitive))

;;(import (editor types extensions combinators))
(import (editor document parse))
(import (editor term))

(import (editor document document-operations))
(import (editor document editor-operations))
(import (editor document copy-paste))

(import (editor text-painter))
(import (editor types texts))
(import (editor types extensions extensions))
(import (editor types extensions widgets))
(import (editor types extensions quotations))
(import (editor types extensions testing))
(import (editor input input))
(import (editor input pane))
(import (editor input terminal-keymap))
(import (editor interfaces delayed))
(import (editor input touch-event-processor))
(import (editor types extensions visual-stepper))
(import (editor types extensions testing))
(import (editor input transforms))
(import (editor awt-clipboard))

(define-alias Thread java.lang.Thread)
(define-alias BlockingQueue java.util.concurrent.BlockingQueue)
(define-alias ThreadPool java.util.concurrent.ScheduledThreadPoolExecutor)
(define-alias Scheduler java.util.concurrent.ScheduledExecutorService)
(define-alias ScheduledTask java.util.concurrent.ScheduledFuture)
(define-alias TimeUnit java.util.concurrent.TimeUnit)

(define-alias InputStream java.io.InputStream)
(define ClassLoader ::java.lang.ClassLoader
  (java.lang.ClassLoader:getSystemClassLoader))

(define-alias ArrayBlockingQueue
  java.util.concurrent.ArrayBlockingQueue)
(define-alias System java.lang.System)

(define-syntax define-box
  (syntax-rules (::)
    ((_ (name)::type initial-value)
     (define-early-constant name
       (let* ((state ::type initial-value)
	      (getter (lambda () state)))
	 (set! (setter getter)
	       (lambda (value::type) (set! state value)))
	 getter)))

    ((_ (name) initial-value)
     (define-early-constant name
       (let* ((state initial-value)
	      (getter (lambda () state)))
	 (set! (setter getter) (lambda (value) (set! state value)))
	 getter)))
    ))

(define (load-resource path::String)::InputStream
  (or
   (ClassLoader:getResourceAsStream path)
   (ClassLoader:getResourceAsStream (string-drop path 1))))

(define-box (screen-up-to-date?)::boolean #f)

(define-cache (color red::int green::int blue::int)::Color
  (Color:RGB red green blue))

(define-parameter (the-text-style)::TextStyle
  (TextStyle:noneOf TextDecoration:class))

(define-parameter (the-text-color)::Color
  (color 255 255 255))

(define-parameter (the-background-color)::Color
  (color 0 0 0))

(define-parameter (the-comment-text-color)::Color
  (color 255 255 200))

(define-parameter (the-comment-background-color)::Color
  (color 0 0 0))

(define-parameter (the-odd-comment-color)::Color
  (color 76 76 76))

(define-parameter (the-even-comment-color)::Color
  (color 127 127 127))

(define-parameter (the-quoted-text-color)::Color
  (color 200 255 255))

(define-parameter (the-quoted-text-background-color)::Color
  (color 0 0 0))

(define-parameter (the-text-intensity)::float
  1.0)

(define-cache (blend fg::Color bg::Color intensity::float)::Color
  (let* ((rf ::int (fg:getRed))
	 (gf ::int (fg:getGreen))
	 (bf ::int (fg:getBlue))
	 (rb ::int (bg:getRed))
	 (gb ::int (bg:getGreen))
	 (bb ::int (bg:getBlue))
	 (r ::int (nearby-int (linear-interpolation
			       from: rb to: rf at: intensity)))
	 (g ::int (nearby-int (linear-interpolation
			       from: gb to: gf at: intensity)))
	 (b ::int (nearby-int (linear-interpolation
			       from: bb to: bf at: intensity))))
    (Color:RGB r g b)))

(define-cache (letter/cached character color::Color background::Color
			     style::TextStyle)
  ::Letter
  (Letter character color background style))
  
(define/kw (letter character
		   color: text-color::Color := (the-text-color)
		   background: background::Color
		   := (the-background-color)
		   style: style::TextStyle := (the-text-style))
  ::Letter
  (let ((color ::Color (blend text-color background
			      (the-text-intensity))))
    (letter/cached character color background style)))

(define system-clipboard
  (try-catch
   (let* ((toolkit ::java.awt.Toolkit
		   (java.awt.Toolkit:getDefaultToolkit))
	  (clipboard ::AWTClipboard 
		     (toolkit:getSystemClipboard)))
     (AWTSystemClipboard clipboard))
   (ex java.lang.Throwable
       (WARN "unable to obtain system clipboard: "ex)
       (the-system-clipboard))))

(set! (the-system-clipboard) system-clipboard)

(define (render io ::LanternaScreen)::void
  (parameterize ((the-system-clipboard system-clipboard))
    (let loop ()
      (synchronized screen-up-to-date?
	(while (screen-up-to-date?)
	  (invoke screen-up-to-date? 'wait))
	;; if - during rendering - the state of
	;; the screen changes (by the editing thread),
	;; then this value will be set to #f, and
	;; another iteration of the rendering loop
	;; will be forced.
	;; (This idea was inspired by Richard Stallman's
	;; 1981 paper "EMACS: The Extensible,
	;; Customizable Display Editor", section 13
	;; "The Display Processor")
	(set! (screen-up-to-date?) #t))
      (let* ((resize ::TerminalSize (io:doResizeIfNecessary))
	     (size (or resize (io:getTerminalSize))))
	(screen:set-size! (size:getColumns) (size:getRows))
	(painter:clear!)
	(screen:render!)
	;; swap front- and back-buffer
	(io:refresh (if resize
			LanternaScreen:RefreshType:COMPLETE
			LanternaScreen:RefreshType:DELTA))
	(loop)))))

(define-interface CancellableRunner (java.lang.Runnable
				     Postponed
				     Cancellable))

(define-object (EventRunner queue::BlockingQueue)
  ::CancellableRunner

  (define postponed-action ::(maps () to: boolean) never)

  (define thread-pool ::Scheduler (ThreadPool 1))

  (define scheduled-task ::ScheduledTask #!null)

  (define (cancel)::Cancellable
    (when scheduled-task
      (scheduled-task:cancel #f)
      (set! postponed-action never)
      (set! scheduled-task #!null))
    (this))

  (define (after time-ms::long action::procedure)
    ::Cancellable
    (set! postponed-action action)
    (set! scheduled-task
	  (thread-pool:schedule (this) time-ms
				TimeUnit:MILLISECONDS))
    (this))

  (define (run)::void
    (queue:put postponed-action)
    ))

(define (rewrite-events io::LanternaScreen queue::BlockingQueue)::void
  ;; although a thread rewriting stuff from one place
  ;; to another may not seem very useful, the point is
  ;; to expose a queue, so that things can be added to it
  ;; asynchronously, from a timer event
  (parameterize ((the-system-clipboard system-clipboard))
    (while #t
      (let ((event ::KeyStroke (io:readInput)))
	(queue:put event)))))

(define (edit io ::LanternaScreen queue::BlockingQueue)::void
  (let* ((postpone ::CancellableRunner (EventRunner queue))
	 (pointer ::TouchEventProcessor
		  (TouchEventProcessor 0 screen postpone
				       vicinity: 1)))
    (parameterize ((the-system-clipboard system-clipboard))
      (while #t
	(safely
	 (let ((input (queue:take)))
	   (if (procedure? input)
	       (input)
	       (let* ((key ::KeyStroke input)
		      (type ::KeyType (key:getKeyType))
		      (caret ::TerminalPosition (io:getCursorPosition)))
		 (match type

		   (,KeyType:MouseEvent
		    (let* ((action ::MouseAction
				   (as MouseAction key))
			   (position ::TerminalPosition
				     (action:getPosition))
			   (last-position ::Position
					  (last-known-pointer-position
					   0))
			   (left (position:getColumn))
			   (top (position:getRow)))
		      (cond
		       ((action:isMouseMove)
			(set! last-position:left left)
			(set! last-position:top top))
		       ((action:isMouseDown)

			(match (action:getButton)
			  (,MouseButton:Left
			   (pointer:press! left top
					   (System:currentTimeMillis)))
			  (,MouseButton:Right
 			   (set! last-position:left left)
			   (set! last-position:top top))
			  (,MouseButton:WheelUp
			   (set! last-position:left left)
			   (set! last-position:top top)
			   (screen:key-typed!
			    (special-key-code KeyType:PageUp)
			    '()))
			  (,MouseButton:WheelDown
			   (set! last-position:left left)
			   (set! last-position:top top)
			   (screen:key-typed!
			    (special-key-code KeyType:PageDown)
			    '()))
			  (_
			   (values))))
		       ((action:isMouseDrag)
			(pointer:move! left top
				       (System:currentTimeMillis)))

		       ((action:isMouseUp)
			(pointer:release! left top
					  (System:currentTimeMillis)))
		       )))

		   (_
		    (parameterize ((unicode-input (input-character
						   key)))
		      (screen:key-typed! (scancode key) '())))))))
	 (synchronized screen-up-to-date?
	   (set! (screen-up-to-date?) #f)
	   (invoke screen-up-to-date? 'notify)))))))

(define pending-animations
  ::java.util.Collection
  (java.util.concurrent.ConcurrentLinkedQueue))

(define-object (Pending animation::Animation)
  (define then ::long (current-time-ms))

  (define (apply0)
    (let* ((now ::long (current-time-ms))
	   (delta/ms ::long (- now then)))
      (unless (animation:advance! delta/ms)
        (pending-animations:remove (this)))
      (set! then now)))

  (gnu.mapping.Procedure0))

(define-object (TerminalPainter io::LanternaScreen
				queue::BlockingQueue)::Painter

  (define text-color-stack ::java.util.Stack (java.util.Stack))
  (define background-color-stack ::java.util.Stack (java.util.Stack))

  (define (put! c::char row::real col::real)::void
    (let ((screen ::Extent (screen:extent))
	  (x (+ shiftLeft (nearby-int
			   (* (slot-ref (this) 'horizontal-stretch)
			      col))))
          (y (+ shiftTop (nearby-int
			  (* (slot-ref (this) 'vertical-stretch)
			     row))))
	  (left (max 0 clipLeft))
	  (top (max 0 clipTop)))
      (when (and (is left <= x < (+ left clipWidth))
                 (is top <= y < (+ top clipHeight))
		 (is 0 <= x < screen:width)
		 (is 0 <= y < screen:height))
	(io:setCharacter x y (letter c)))))

  (define (mark-editor-cursor! +left::real +top::real
			       editor::WithCursor)
    ::void
    (invoke-special CharPainter (this)
		    'mark-editor-cursor! +left +top editor)
    (let* ((position ::Position (editor:cursor-position))
	   (x ::real position:left)
	   (y ::real position:top)
	   (screen ::Extent (screen:extent)))
      (when (and (is 0 <= x < screen:width)
		 (is 0 <= y < screen:height))
	(let ((letter (io:getBackCharacter x y)))
	  (letter:getCharacter))

	(io:setCursorPosition
	 (TerminalPosition x y)))))

  (define (enter-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'enter-selection-drawing-mode!)
    (let ((text-color (the-text-color)))
      (set! (the-text-color) (the-background-color))
      (set! (the-background-color) text-color)))

  (define (exit-selection-drawing-mode!)::void
    (let ((text-color (the-text-color)))
      (set! (the-text-color) (the-background-color))
      (set! (the-background-color) text-color))
    (invoke-special CharPainter (this)
		    'exit-selection-drawing-mode!))

  (define (get row::real col::real)::char
    (let ((x (+ shiftLeft
		(nearby-int (* (slot-ref (this)
					 'horizontal-stretch) col))))
	  (y (+ shiftTop
		(nearby-int (* (slot-ref (this)
					 'vertical-stretch) row))))
	  (screen ::Extent (screen:extent)))
      (if (and (is 0 <= x < screen:width)
	       (is 0 <= y < screen:height))
	  (let ((letter (io:getBackCharacter x y)))
	    (letter:getCharacter))
	  #\space)))

  (define (clear!)::void
    (io:clear))

  (define (current-width)::real
    (let ((size (io:getTerminalSize)))
      (size:getColumns)))

  (define (current-height)::real
    (let ((size (io:getTerminalSize)))
      (size:getRows)))

  (define (draw-quoted-text! s::CharSequence
			     context::Cursor)
    ::void
    (parameterize ((the-text-color (the-quoted-text-color))
		   (the-background-color (the-quoted-text-background-color)))
      (invoke-special CharPainter (this) 'draw-quoted-text!
		      s context)))

  (define (draw-line-comment! text::CharSequence context::Cursor)::void
    (parameterize ((the-text-color (the-comment-text-color))
		   (the-background-color (the-comment-background-color)))
      (invoke-special CharPainter (this) 'draw-line-comment!
		      text context)))

  (define (draw-block-comment! text::CharSequence context::Cursor)::void
    (parameterize ((the-text-color (the-comment-text-color))
		   (the-background-color (the-comment-background-color)))
      (invoke-special CharPainter (this) 'draw-block-comment!
		      text context)))

  (define (enter-comment-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'enter-comment-drawing-mode!)
    (text-color-stack:push (the-text-color))
    (set! (the-text-color)
	  (if (even? (as int (slot-ref
			      (this)
			      'current-comment-level)))
	      (the-even-comment-color)
	      (the-odd-comment-color))))

  (define (exit-comment-drawing-mode!)::void
    (set! (the-text-color) (text-color-stack:pop))
    (invoke-special CharPainter (this)
		    'exit-comment-drawing-mode!))

  (define (draw-point! left::real top::real color-rgb::int)
    ::void
    (let* ((red ::int (byte-ref color-rgb 2))
	   (green ::int (byte-ref color-rgb 1))
	   (blue ::int (byte-ref color-rgb 0))
	   (color ::Color (Color:Indexed:fromRGB red green blue))
	   (foreground ::Color (if (is (+ red green blue) > 384)
				   Color:ANSI:BLACK
				   Color:ANSI:WHITE)))
      (parameterize ((the-text-color foreground)
		     (the-background-color color))
	(put! #\⦿ top left))))

  (define (play! animation::Animation)::void
    (unless (any (lambda (pending::Pending)
		   (eq? pending:animation animation))
		 pending-animations)
      (pending-animations:add (Pending animation))))

  (define (with-intensity i::float action::(maps () to: void))::void
    (parameterize ((the-text-intensity i))
      (action)))

  (define thread-pool ::Scheduler (ThreadPool 1))

  (define animating ::ScheduledTask #!null)

  (define (start-animating!)::void
    (set! animating
	  (thread-pool:scheduleAtFixedRate
	   (lambda ()
	     (for playing in pending-animations
	          (queue:put playing)))
	   40 40 TimeUnit:MILLISECONDS)))

  (CharPainter)
  (start-animating!)
  )



(define (run-in-terminal
	 #!optional
	 (io :: LanternaScreen (make-terminal-screen
				background: (letter #\space))))
  ::void
  (cond
   ((and-let* ((`(,command "-p" ,port) (command-line))
	       (port (string->number port))
	       ((integer? port)))
      port) =>
      (lambda (port::integer)
	(WARN "Starting debug server on port "port)
	(let ((tcp-server (tcp-output-server port)))
	  (set! (current-output-port) tcp-server)
	  (set! (current-error-port) tcp-server)
	  (WARN "Debug server started on port "port))))
   (else
    (set! (current-display-procedure) nothing)
    (set! (current-message-handler) (ignoring-message-handler))))
  (initialize-keymap)
  (safely
   (let* ((input (gnu.kawa.io.InPort
		  (java.io.InputStreamReader
		   (load-resource "/assets/init.scm"))))
	  (init-script (read-all input)))
     (for expression in init-script
       (eval expression))))
  (let ((event-queue ::BlockingQueue (ArrayBlockingQueue 16)))
    (with ((painter (TerminalPainter io event-queue)))
      (io:startScreen)
      (io:clear)
      (io:refresh)
      (let* ((preprocessing (future (rewrite-events
				     io event-queue)))
	     (editing (future (edit io event-queue)))
	     (rendering (future (render io))))
	;; we want the rendering thread to have a lower
	;; priority than the editing thread
	(invoke rendering 'setPriority Thread:MIN_PRIORITY)
	(force editing)))))

(run-in-terminal)
