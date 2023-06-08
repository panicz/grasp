(module-name grasp-terminal)
(module-compile-options main: #t)

(import (define-syntax-rule))
(import (define-interface))
(import (define-property))
(import (define-type))
(import (define-object))
(import (define-cache))
(import (default-value))
(import (define-parameter))
(import (server))
(import (extent))
(import (fundamental))
(import (conversions))
(import (indexable))
;;(import (space))
(import (cursor))
(import (primitive))
(import (extent))
;;(import (combinators))
(import (parse))
(import (examples))
(import (assert))
(import (infix))
(import (match))
(import (term))
(import (functions))
(import (print))
(import (painter))
(import (for))
(import (while))
(import (document-operations))
(import (editor-operations))
(import (text-painter))
(import (extension))
(import (button))
(import (quotations))
(import (input))
(import (mapping))
(import (pane))
(import (terminal-keymap))
(import (postponed))
(import (touch-event-processor))


(define-alias Thread java.lang.Thread)
(define-alias TimerTask java.util.TimerTask)
(define-alias BlockingQueue java.util.concurrent.BlockingQueue)
(define-alias ThreadPool java.util.concurrent.ScheduledThreadPoolExecutor)
(define-alias Scheduler java.util.concurrent.ScheduledExecutorService)
(define-alias ScheduledTask java.util.concurrent.ScheduledFuture)
(define-alias TimeUnit java.util.concurrent.TimeUnit)

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

(define-box (screen-up-to-date?)::boolean #f)

(define-parameter (the-text-style)::TextStyle
  (TextStyle:noneOf TextDecoration:class))

(define-parameter (the-text-color)::Color
  Color:ANSI:DEFAULT)

(define-parameter (the-background-color)::Color
  Color:ANSI:DEFAULT)

(define-parameter (the-selected-text-color)::Color
  Color:ANSI:BLACK)

(define-parameter (the-selected-background-color)::Color
  Color:ANSI:YELLOW)

(define-parameter (the-comment-text-color)::Color
  Color:ANSI:WHITE)

(define-parameter (the-comment-background-color)::Color
  Color:ANSI:DEFAULT)

(define-parameter (the-selected-comment-text-color)::Color
  Color:ANSI:BLACK)

(define-parameter (the-selected-comment-background-color)::Color
  Color:ANSI:WHITE)

(define-parameter (the-odd-comment-color)::Color
  (Color:RGB 76 76 76))
(define-parameter (the-even-comment-color)::Color
  (Color:RGB 127 127 127))

(define-cache (letter character
		      color: color::Color := (the-text-color)
		      background: background::Color
		      := (the-background-color)
		      style: style::TextStyle := (the-text-style))
  ::Letter
  (Letter character color background style))

(define (render io :: LanternaScreen)::void
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
	   (size (or resize (io:getTerminalSize)))
	   (painter (the-painter)))
      (screen:set-size! (size:getColumns) (size:getRows))
      (painter:clear!)
      (screen:draw!)
      ;; swap front- and back-buffer
      (io:refresh (if resize
		      LanternaScreen:RefreshType:COMPLETE
		      LanternaScreen:RefreshType:DELTA))
      (loop))))

(define-interface CancellableRunner (java.lang.Runnable Postponed Cancellable))

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
	  (thread-pool:schedule (this) time-ms TimeUnit:MILLISECONDS))
    (this))

  (define (run)::void
    (queue:put postponed-action)))

(define (rewrite-events io::LanternaScreen queue::BlockingQueue)::void
  ;; although a thread rewriting stuff from one place
  ;; to another may not seem very useful, the point is
  ;; to expose a queue, so that things can be added to it
  ;; asynchronously, from a timer event
  (while #t
    (let ((event ::KeyStroke (io:readInput)))
      (queue:put event))))

(define (edit io ::LanternaScreen queue::BlockingQueue)::void
  (let* ((postpone ::CancellableRunner (EventRunner queue))
	 (pointer ::TouchEventProcessor
		  (TouchEventProcessor 0 screen postpone
				       vicinity: 1)))
    (while #t
      (safely
       (let ((input (queue:take)))
	 (if (procedure? input)
	     (input)
	     (let* ((key ::KeyStroke input)
		    (type ::KeyType (key:getKeyType))
		    (caret ::TerminalPosition (io:getCursorPosition)))
	       (match type
		 #;(,KeyType:Character 
		 (parameterize ((unicode-input (input-character key)))
		 (invoke (the-screen) 'key-typed! (scancode key))))

		 (,KeyType:MouseEvent
		  (let* ((action ::MouseAction
				 (as MouseAction key))
			 (position ::TerminalPosition
				   (action:getPosition))
			 (left (position:getColumn))
			 (top (position:getRow)))
		    (cond
		     ((action:isMouseMove)
		      (values))
		     ((action:isMouseDown)
		      
		      (match (action:getButton)
			(,MouseButton:Left
			 (pointer:press! left top
					 (System:currentTimeMillis)))
			(,MouseButton:Right
			 (values))
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
		    (screen:key-typed! (scancode key))))))))       
       (synchronized screen-up-to-date?
	 (set! (screen-up-to-date?) #f)
	 (invoke screen-up-to-date? 'notify))))))

(define-object (TerminalPainter screen::LanternaScreen)::Painter
  
  (define io::LanternaScreen screen)

  (define text-color-stack ::java.util.Stack (java.util.Stack))
  (define background-color-stack ::java.util.Stack (java.util.Stack))
  
  (define (put! c::char row::real col::real)::void
    (let ((x (+ col shiftLeft))
          (y (+ row shiftTop))
	  (left (max 0 clipLeft))
	  (top (max 0 clipTop)))
      (when (and (is left <= x < (+ left clipWidth))
                 (is top <= y < (+ top clipHeight)))
	(io:setCharacter x y (letter c)))))

  (define (mark-cursor! +left::real +top::real)::void
    (invoke-special CharPainter (this)
		    'mark-cursor! +left +top)
    (io:setCursorPosition
     (TerminalPosition markedCursorPosition:left
		       markedCursorPosition:top)))
  
  (define (enter-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'enter-selection-drawing-mode!)
    (text-color-stack:push (the-text-color))
    (background-color-stack:push (the-background-color))
    (set! (the-text-color) (the-selected-text-color))
    (set! (the-background-color) (the-selected-background-color)))

  (define (exit-selection-drawing-mode!)::void
    (set! (the-text-color) (text-color-stack:pop))
    (set! (the-background-color) (background-color-stack:pop))
    (invoke-special CharPainter (this)
		    'exit-selection-drawing-mode!))
  
  (define (get row::real col::real)::char
    (let ((letter (io:getBackCharacter col row)))
      (letter:getCharacter)))

  (define (clear!)::void
    (io:clear))

  (define (current-width)::real
    (let ((size (io:getTerminalSize)))
      (size:getColumns)))

  (define (current-height)::real
    (let ((size (io:getTerminalSize)))
      (size:getRows)))

  (define (draw-line-comment! text::CharSequence context::Cursor)::void
    (parameterize ((the-text-color (the-comment-text-color))
		   (the-background-color
		    (the-comment-background-color))
		   (the-selected-text-color
		    (the-selected-comment-text-color))
		   (the-selected-background-color
		    (the-selected-comment-background-color)))
      (invoke-special CharPainter (this) 'draw-line-comment!
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
  
  (define (draw-point! left::real top::real color-rgb::int)::void
    (let* ((red ::int (byte-ref color-rgb 2))
	   (green ::int (byte-ref color-rgb 1))
	   (blue ::int (byte-ref color-rgb 0))
	   (color ::Color (Color:Indexed:fromRGB red green blue))
	   (foreground ::Color (if (is (+ red green blue) > 384)
				   Color:ANSI:BLACK
				   Color:ANSI:WHITE)))
      (io:setCharacter left top
		       (letter #\⦿
			       color: foreground
			       background: color))))
  (CharPainter))

(define (run-in-terminal
	 #!optional
	 (io :: LanternaScreen (make-terminal-screen)))
  ::void
  (cond
   ((and-let* ((`(,command "-p" ,port) (command-line))
	       (port (string->number port))
	       ((integer? port)))
      port) =>
      (lambda (port::integer)
	(WARN "Starting debug server on port "port)
	(let ((tcp-server (tcp-output-server 12345)))
	  (set! (current-output-port) tcp-server)
	  (set! (current-error-port) tcp-server)
	  (WARN "Debug server started on port "port))))
   (else
     (set! (current-display-procedure) nothing)
     (set! (current-message-handler) (ignoring-message-handler))))
  (initialize-keymap)     
  (parameterize ((the-painter (TerminalPainter io)))
    (safely
     (load "assets/init.scm"))
    (io:startScreen)
    (let* ((event-queue ::BlockingQueue (ArrayBlockingQueue 16))
	   (preprocessing (future (rewrite-events io event-queue)))
	   (editing (future (edit io event-queue)))
	   (rendering (future (render io))))
      ;; we want the rendering thread to have a lower
      ;; priority than the editing thread
      (invoke rendering 'setPriority Thread:MIN_PRIORITY)
      (force editing))))

(run-in-terminal)
