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
;;(import (extension))
;;(import (button))
(import (input))
(import (mapping))
(import (pane))

(import (terminal-keymap))

(define-alias Thread java.lang.Thread)

;; OK, wyglada na to, ze klienta terminalowego
;; bedziemy rozwijac na telefonie.

;; nie zmienia to jednak faktu, ze chcemy
;; sie teraz zajac klikaniem

;; plan zatem taki:
;; - dodajemy metode cursor-under* do interfejsu Element
;; - tworzymy implementacje tej metody dla:
;;   - spacji
;;   - textu
;;   - atomow
;;   - par/sekwencji
;;   - kombinacji
;;   - innych instancji?
;; - tworzymy duzo testow jednostkowych
;;   (moze byc w test-painter)

;; dalej: poki siedzimy na telefonie, powinnismy
;; rozbudowac klienta terminalowego
;; (tak zeby finalnie usunac primitive-terminal-client)
;;
;; fajnie by tez bylo zaimplementowac protokol
;; kitty do rysowania w terminalu
;; albo terminal ze wsparciem dla grafiki wektorowej
;;
;; ale teraz to bez znaczenia
;;
;; 

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

(define-cache (letter character
		      color: color::Color := (the-text-color)
		      background: background::Color
		      := (the-background-color)
		      style: style::TextStyle := (the-text-style))
  ::Letter
  (Letter character color background style))

(define (render io :: LanternaScreen)::void
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
	 (extent ::Extent (the-screen-extent))
	 (painter (the-painter)))
    (set! extent:width (size:getColumns))
    (set! extent:height (size:getRows))
    (painter:clear!)
    (invoke (the-screen) 'draw! '())
    (overlay:draw!)
    ;; swap front- and back-buffer
    (io:refresh (if resize
		    LanternaScreen:RefreshType:COMPLETE
		    LanternaScreen:RefreshType:DELTA))
    (render io)))

(define previous-mouse ::Position (Position))

(define (edit io :: LanternaScreen)::void
  (let loop ()
    (let* ((key ::KeyStroke (io:readInput))
	   (type ::KeyType (key:getKeyType))
	   (caret ::TerminalPosition (io:getCursorPosition)))
      (match type
	(,KeyType:Character 
	 (parameterize ((unicode-input (input-character key)))
	   (invoke (the-screen) 'key-typed! (scancode key))))
	
	(,KeyType:EOF
	 (io:stopScreen)
	 (exit))

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
		(invoke (the-screen) 'press! 0 #;at left top)
		(set! previous-mouse:left left)
		(set! previous-mouse:top top))
	       (,MouseButton:Right
		(values))
	       (_
		(values))))
	    ((action:isMouseDrag)
	     (invoke (the-screen)
		     'move! 0 left top
		     (- left previous-mouse:left)
		     (- top previous-mouse:top))
	     (set! previous-mouse:left left)
	     (set! previous-mouse:top top))

	    ((action:isMouseUp)
	     (invoke (the-screen)
		     'release! 0 left top
		     (- left previous-mouse:left)
		     (- top previous-mouse:top))
	     (set! previous-mouse:left left)
	     (set! previous-mouse:top top)))))
	
	(_
	 (invoke (the-screen) 'key-typed! (scancode key)))))
    
    (synchronized screen-up-to-date?
      (set! (screen-up-to-date?) #f)
      (invoke screen-up-to-date? 'notify))
    (loop)))

(define-object (TerminalPainter screen::LanternaScreen)::Painter
  
  (define io::LanternaScreen screen)

  (define text-color ::Color Color:ANSI:DEFAULT)
  (define background-color ::Color Color:ANSI:DEFAULT)
  
  (define (put! c::char row::real col::real)::void
    (let ((x (+ col shiftLeft))
          (y (+ row shiftTop))
	  (left (max 0 clipLeft))
	  (top (max 0 clipTop)))
      (when (and (is left <= x < (+ left clipWidth))
                 (is top <= y < (+ top clipHeight)))
	(io:setCharacter x y (letter c color: text-color
				     background: background-color)))))

  (define (mark-cursor! +left::real +top::real)::void
    (invoke-special CharPainter (this)
		    'mark-cursor! +left +top)
    (io:setCursorPosition
     (TerminalPosition markedCursorPosition:left
		       markedCursorPosition:top)))
  
  (define (enter-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'enter-selection-drawing-mode!)
    (set! text-color Color:ANSI:BLACK)
    (set! background-color Color:ANSI:YELLOW))

  (define (exit-selection-drawing-mode!)::void
    (invoke-special CharPainter (this)
		    'exit-selection-drawing-mode!)
    (set! text-color Color:ANSI:DEFAULT)
    (set! background-color Color:ANSI:DEFAULT))
  
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

  (define (draw-point! left::real top::real color-rgb::int)::void
    (let* ((red ::int (bitwise-and #xff (bitwise-arithmetic-shift
					color-rgb -16)))
	   (green ::int (bitwise-and #xff (bitwise-arithmetic-shift
					   color-rgb -8)))
	   (blue ::int (bitwise-and #xff color-rgb))
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
  :: void
  (initialize-keymap)  
  (parameterize ((the-painter (TerminalPainter io)))
    (safely
     (load "assets/init.scm"))
    
    (io:startScreen)
    (let* ((editing (future (edit io)))
	   (rendering (future (render io))))
      ;; we want the rendering thread to have a lower
      ;; priority than the editing thread
      (invoke rendering 'setPriority Thread:MIN_PRIORITY)
      (force editing))))

(run-in-terminal)
