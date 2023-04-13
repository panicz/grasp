(module-name grasp-desktop)
(module-compile-options main: #t)

(import (srfi :11))
(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (define-parameter))
(import (default-value))
(import (mapping))
(import (fundamental))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (pane))
(import (indexable))
(import (painter))
(import (print))
(import (primitive))
(import (cursor))
(import (input))
(import (extent))
(import (conversions))
(import (parse))
(import (editor-operations))
(import (history))
(import (desktop-keymap))

(define-alias Font java.awt.Font)
(define-alias FontMetrics java.awt.FontMetrics)
(define-alias File java.io.File)
(define-alias InputStream java.io.InputStream)
(define ClassLoader ::java.lang.ClassLoader
  (java.lang.ClassLoader:getSystemClassLoader))

(define-alias FocusEvent java.awt.event.FocusEvent)
;;(define-alias KeyEvent java.awt.event.KeyEvent)
(define-alias ComponentEvent java.awt.event.ComponentEvent)

(define-alias MouseEvent java.awt.event.MouseEvent)

(define-alias MouseWheelListener java.awt.event.MouseWheelListener)
(define-alias MouseWheelEvent java.awt.event.MouseWheelEvent)

(define-alias WindowAdapter java.awt.event.WindowAdapter)
(define-alias WindowEvent java.awt.event.WindowEvent)

(define-alias Graphics java.awt.Graphics)
(define-alias Graphics2D java.awt.Graphics2D)
(define-alias RenderingHints java.awt.RenderingHints)
(define-alias Shape java.awt.Shape)
(define-alias Path2D java.awt.geom.Path2D)

(define-alias Rectangle java.awt.Rectangle)
(define-alias AffineTransform java.awt.geom.AffineTransform)

(define-alias Color java.awt.Color)

(define-cache (color aRGB::int)::Color
  (let* ((alpha ::int (- 255
			 (bitwise-and #xff
				      (bitwise-arithmetic-shift
				       aRGB -24))))
	 (red ::int (bitwise-and #xff (bitwise-arithmetic-shift
				       aRGB -16)))
	 (green ::int (bitwise-and #xff (bitwise-arithmetic-shift
					 aRGB -8)))
	 (blue ::int (bitwise-and #xff aRGB)))
    (Color red green blue alpha)))

(define-parameter (parenthesis-color) ::Color
  (color #xcccccc))

(define-parameter (focused-parenthesis-color) ::Color
  Color:DARK_GRAY)

(define-parameter (matching-parenthesis-color) ::Color
  (color #x888888))

(define-early-constant graphics-environment
  ::java.awt.GraphicsEnvironment
  (invoke-static
   java.awt.GraphicsEnvironment
   'getLocalGraphicsEnvironment))

(define (load-font path::String #!key (size ::float 12.0))
  (let* ((font-source ::InputStream
		      (ClassLoader:getResourceAsStream path))
	 (font ::Font (Font:createFont
		       Font:TRUETYPE_FONT
		       font-source)))
    (graphics-environment:registerFont font)
    (font:deriveFont size)))

(define-constant Basic-Regular
  (load-font "assets/Basic-Regular.otf" size: 20))

(define-constant LobsterTwo-Regular
  (load-font "assets/LobsterTwo-Regular.otf" size: 28))

(define-constant Oswald-Regular
  (load-font "assets/Oswald-Regular.ttf" size: 22))

(define-constant GloriaHallelujah
  (load-font "assets/GloriaHallelujah.ttf" size: 16))

(define-constant NotoSerif-Regular
  (load-font "assets/NotoSerif-Regular.ttf" size: 16))

(define-parameter+ (the-atom-font) ::Font
  #;Basic-Regular LobsterTwo-Regular)

(define-parameter+ (the-string-font) ::Font
  Basic-Regular #;LobsterTwo-Regular)

(define-parameter+ (the-comment-font) ::Font
  GloriaHallelujah)

(define-parameter+ (the-block-comment-font) ::Font
  NotoSerif-Regular)

(define-parameter+ (the-block-comment-margin) ::real
  10)

(define-parameter (the-cursor-offset)::Position
  (Position left: 0 top: 16))

(define-parameter (the-cursor-extent)::Extent
  (Extent width: 2 height: 16))

(define-syntax-rule (Path (command args ...) ...)
  (let ((path ::Path2D (Path2D:Float)))
    (invoke path 'command (as float args) ...)
    ...
    path))

(define-constant top-left-paren ::Path2D
  (Path
   (moveTo 10 0)
   (quadTo 2.5 0 0 25)
   (lineTo 5 25)
   (quadTo 5 15 10 15)
   (closePath)))

(define-constant top-left-bounds ::Rectangle
  (top-left-paren:getBounds))

(define-constant bottom-left-paren ::Path2D
  (Path
   (moveTo 10 25)
   (quadTo 2.5 25 0 0)
   (lineTo 5 0)
   (quadTo 5 10 10 10)
   (closePath)))

(define-constant bottom-left-bounds ::Rectangle
  (bottom-left-paren:getBounds))

(define-constant top-right-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 7.5 0 10 25)
   (lineTo 5 25)
   (quadTo 5 15 0 15)
   (closePath)))

(define-constant top-right-bounds ::Rectangle
  (top-right-paren:getBounds))

(define-constant bottom-right-paren ::Path2D
  (Path
   (moveTo 0 25)
   (quadTo 7.5 25 10 0)
   (lineTo 5 0)
   (quadTo 5 10 0 10)
   (closePath)))

(define-constant bottom-right-bounds ::Rectangle
  (bottom-right-paren:getBounds))

(define-constant transparent ::Color (Color 0.0 0.0 0.0 0.0))

(define-interface InputListener
  (java.awt.event.KeyListener
   java.awt.event.FocusListener
   java.awt.event.ComponentListener
   java.awt.event.MouseMotionListener
   java.awt.event.MouseListener))

(define-interface Application (Painter InputListener))

(define-object (InputHandler)::InputListener
  (define (mouseEntered event::MouseEvent)::void
    (values))

  (define (mouseExited event::MouseEvent)::void
    (values))

  (define (mouseClicked event::MouseEvent)::void
    (values))

  (define (mousePressed event::MouseEvent)::void
    (values))

  (define (mouseReleased event::MouseEvent)::void
    (values))

  (define (mouseDragged event::MouseEvent)::void
    (values))

  (define (mouseMoved event::MouseEvent)::void
    (values))

  (define (focusGained event::FocusEvent)::void
    (values))

  (define (keyTyped event::KeyEvent)::void
    (values))

  (define (keyReleased event::KeyEvent)::void
    (values))

  (define (keyPressed event::KeyEvent)::void
    (values))

  (define (focusLost event::FocusEvent)::void
    (invoke (invoke event 'getComponent) 'requestFocus))

  (define (componentHidden event::ComponentEvent)::void
    (values))

  (define (componentShown event::ComponentEvent)::void
    (values))
  
  (define (componentMoved event::ComponentEvent)::void
    (values))

  (define (componentResized event::ComponentEvent)::void
    (values))

  (javax.swing.JComponent)
  (addKeyListener (this))
  (addFocusListener (this))
  (addComponentListener (this))
  (addMouseListener (this))
  (addMouseMotionListener (this))
  )

(define-object (GRASP)::Application
  (define graphics ::Graphics2D)
  
  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (graphics:setClip left top width height))
  
  (define (current-clip-width)::real
    (let ((clip-area ::Rectangle
		     (graphics:getClipBounds)))
      clip-area:width))
    
  (define (current-clip-height)::real
    (let ((clip-area ::Rectangle
		     (graphics:getClipBounds)))
      clip-area:height))
  
  (define (current-clip-left)::real
    (let ((clip-area ::Rectangle
		     (graphics:getClipBounds)))
      clip-area:x))
  
  (define (current-clip-top)::real
    (let ((clip-area ::Rectangle
		     (graphics:getClipBounds)))
      clip-area:y))

  (define (translate! x::real y::real)::void
    (graphics:translate (as double x) (as double y)))
	    
  (define (current-translation-left)::real
    (let ((transform ::AffineTransform
		     (graphics:getTransform)))
      (transform:getTranslateX)))
    
  (define (current-translation-top)::real
    (let ((transform ::AffineTransform
		     (graphics:getTransform)))
      (transform:getTranslateY)))

  (define rendering-hints ::RenderingHints
    (RenderingHints RenderingHints:KEY_TEXT_ANTIALIASING
		    RenderingHints:VALUE_TEXT_ANTIALIAS_ON))
    
  (define (open-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height))))
      (graphics:setColor color)
      (graphics:fill top-left-paren)
      (graphics:fillRect 0 top-left-bounds:height
			 5 line-height)
      (with-translation (0 (+ top-left-bounds:height
			      line-height))
	  (graphics:fill bottom-left-paren))))

  (define (close-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 top-right-bounds:height
				 bottom-right-bounds:height))))
      (graphics:setColor color)
      (graphics:fill top-right-paren)
      (graphics:fillRect (- top-right-bounds:width 5)
			 top-right-bounds:height 5 line-height)

      (with-translation (0 (+ top-right-bounds:height
			      line-height))
	  (graphics:fill bottom-right-paren))))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (let ((cursor (the-cursor)))
      (open-paren! height
		   (match cursor
		     (`(#\[ . ,,context)
		      (focused-parenthesis-color))
		     (`(#\] . ,,context)
		      (matching-parenthesis-color))
		     (_
		      (parenthesis-color))))
      (with-translation ((- width (paren-width)) 0)
	  (close-paren! height
			(match cursor
			  (`(#\] . ,,context)
			   (focused-parenthesis-color))
			  (`(#\[ . ,,context)
			   (matching-parenthesis-color))
			  (_
			   (parenthesis-color)))))))
  
  (define (space-width)::real 8)
  
  (define (paren-width)::real
    top-left-bounds:width)

  (define (min-box-height)::real
    (max (invoke (the-atom-font) 'getSize2D)
	 (+ top-left-bounds:height bottom-left-bounds:height)
	 (+ top-right-bounds:height bottom-right-bounds:height)))
  
  (define (min-line-height)::real
    (invoke (the-atom-font) 'getSize2D))

  (define (draw-rounded-rectangle! width::real height::real)::void
    (graphics:drawRoundRect 0 0 (as int width) (as int height) 5 5))

  (define (draw-rectangle! width::real height::real)::void
    (graphics:drawRect 0 0 (as int width) (as int height)))
  
  (define marked-cursor-position ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)::void
    (let ((cursor-extent (the-cursor-extent))
	  (cursor-offset (the-cursor-offset)))
      (set! marked-cursor-position:left (+ (current-translation-left)
					   +left))
      (set! marked-cursor-position:top (+ (current-translation-top)
					  +top))
      (graphics:fillRect (+ +left cursor-offset:left)
			 (+ +top cursor-offset:top)
			 cursor-extent:width
			 cursor-extent:height)))
  
  (define (cursor-position)::Position
    marked-cursor-position)

  (define (cursor-height)::real
    (let ((offset ::Position (the-cursor-offset))
	  (extent ::Extent (the-cursor-extent)))
      (+ offset:top extent:height)))
  
  (define text-color ::Color Color:DARK_GRAY)

  (define background-color ::Color transparent)
  
  (define selection-drawing-mode? ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #t)
    (set! text-color Color:WHITE)
    (set! background-color Color:DARK_GRAY))

  (define (exit-selection-drawing-mode!)::void
    (set! selection-drawing-mode? #f)
    (set! text-color Color:DARK_GRAY)
    (set! background-color transparent))
  
  (define (in-selection-drawing-mode?)::boolean
    selection-drawing-mode?)

  (define current-comment-level ::int 0)
  
  (define (enter-comment-drawing-mode!)::void
    (set! text-color (color #xdddddd))
    (set! (parenthesis-color) (color #xe5e5e5))
    (set! atom-frame-color (color #xeaeaea))
    (set! current-comment-level (+ current-comment-level 1)))
  
  (define (exit-comment-drawing-mode!)::void
    (set! current-comment-level (- current-comment-level 1))
    (when (is current-comment-level <= 0)
      (set! text-color (color #x555555))
      (set! atom-frame-color (color #xdddddd))
      (set! (parenthesis-color) (color #xcccccc))))

  (define (in-comment-drawing-mode?)::boolean
    (is current-comment-level > 0))
  
  (define (vertical-bar-width)::real 5)
  
  (define (horizontal-bar-height)::real 5)
  
  (define (draw-horizontal-bar! width::real)::void
    (graphics:fillRect 0 0 width (horizontal-bar-height)))
    
  (define (draw-vertical-bar! height::real)::void
    (graphics:fillRect 0 0 (vertical-bar-width) height))

  (define (horizontal-line-height)::real 20)
  
  (define (vertical-line-width)::real 20)
  
  (define (draw-horizontal-line! top::real)::void
    (graphics:fillRect (max 0 (current-clip-left)) top
		       (current-clip-width) (horizontal-line-height)))
    
  (define (draw-vertical-line! left::real)::void
    (graphics:fillRect left (max 0 (current-clip-top)) 
		       (vertical-line-width) (current-clip-height)))

  (define (draw-line! x0::real y0::real x1::real y1::real)
    ::void
    (graphics:drawLine (as int (round x0))
		       (as int (round y0))
		       (as int (round x1))
		       (as int (round y1))))
  
  (define (draw-text! text::CharSequence
		      font::Font
		      context::Cursor)
    ::void
    (let-values (((selection-start selection-end) (the-selection)))
      (let* ((focused? (and (pair? (the-cursor))
			    (equal? context (cdr (the-cursor)))))
	     (enters-selection-drawing-mode?
	      (and (pair? selection-start)
		   (equal? (tail selection-start) context)))
	     (exits-selection-drawing-mode?
	      (and (pair? selection-end)
		   (equal? (tail selection-end) context)))
	     (metrics ::FontMetrics (graphics:getFontMetrics font))
	     (segment-start 0)
	     (left ::float 0)
	     (lines 1)
	     (height ::float (font:getSize))
	     (string-end (text:length)))
	(parameterize ((the-cursor-extent (Extent width: 2
						  height: height)))
	  (define (render-fragment! segment-end::int)
	    (let* ((fragment (text:subSequence segment-start
					       segment-end))
		   (width (metrics:stringWidth fragment)))
	      (graphics:setColor background-color)
	      (graphics:fillRect left (* (- lines 1) height)
				 width height)
	      (graphics:setColor text-color)
	      (graphics:drawString fragment left (* lines height))
	      (set! left (+ left width))))
	  
	  (graphics:setFont font)
	  (for i from 0 below string-end
	       (when (and focused? (eqv? (head (the-cursor)) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (mark-cursor! left (* (- lines 1) height)))
	       
	       (when (and enters-selection-drawing-mode?
			  (eqv? (head selection-start) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (enter-selection-drawing-mode!))
	       
	       (when (and exits-selection-drawing-mode?
			  (eqv? (head selection-end) i))
		 (render-fragment! i)
		 (set! segment-start i)
		 (exit-selection-drawing-mode!))
	       
	       (when (eq? (text:charAt i) #\newline)
		 (render-fragment! i)
		 (set! left 0)
		 (set! lines (+ lines 1))
		 (set! segment-start (+ i 1))))
	  (render-fragment! string-end)
	  (when (and focused? (eqv? (head (the-cursor)) string-end))
	    (mark-cursor! left (* (- lines 1) height)))))))
  
  (define (draw-string! text::CharSequence context::Cursor)::void
    (draw-text! text (the-string-font) context))

  (define quoted-text-cursor-offset::Position
    (Position left: -1 top: 2))
  
  (define (draw-quoted-text! text::CharSequence context::Cursor)::void
    (parameterize ((the-cursor-offset quoted-text-cursor-offset))
      (draw-string! text context)))

  (define (text-extent text::CharSequence font::Font)::Extent
    (let* ((metrics ::FontMetrics (graphics:getFontMetrics font))
	   (line-start 0)
	   (lines 1)
	   (line-height (metrics:getHeight))
	   (max-width 0)
	   (string-end (text:length)))
      (for i from 0 below string-end
	   (when (eq? (text:charAt i) #\newline)
	     (set! max-width
		   (max max-width
			(metrics:stringWidth (text:subSequence
					      line-start i))))
	     (set! lines (+ lines 1))
	     (set! line-start (+ i 1))))
      (set! max-width
	    (max max-width
		 (metrics:stringWidth
		  (text:subSequence line-start string-end))))
      (Extent width: max-width
	      height: (* lines (metrics:getHeight)))))

  (define (text-character-index-under x::real y::real
				      text::CharSequence
				      font::Font)
    ::int
    (let* ((metrics ::FontMetrics (graphics:getFontMetrics font))
	   (line-height (metrics:getHeight))
	   (string-end (text:length)))
      (let loop ((i 0)
		 (left 0)
		 (top 0))
	(if (is i >= string-end)
	    (max 0 (- i 1))
	    (let ((c (text:charAt i)))
	      (match c
		(#\newline
		 (if (is top <= y < (+ top line-height))
		     i
		     (loop (+ i 1) 0 (+ top line-height))))
		(_
		 (let ((width (metrics:charWidth c)))
		   (if (and (is top <= y < (+ top line-height))
			    (is left <= x < (+ left width)))
		       i
		       (loop (+ i 1) (+ left width) top))))))))))
  
  (define (atom-extent text::CharSequence)::Extent
    (let ((inner (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (+ inner:height 16))))

  (define atom-cursor-offset::Position (Position left: 0 top: 4))

  (define atom-frame-color ::Color (Color #xdddddd))
  
  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((extent (atom-extent text))
	   (font (the-atom-font)))
      (graphics:setColor atom-frame-color)
      (graphics:fillRoundRect 0 14
			      extent:width (- extent:height 28)
			      12 12)
      (with-translation (4 8)
	  (parameterize ((the-cursor-offset atom-cursor-offset))
	    (draw-text! text font context)))))

  (define (atom-character-index-under x::real y::real
				      text::CharSequence)
    ::int
    (text-character-index-under x y text (the-atom-font)))
  
  (define (quoted-text-extent text::CharSequence)::Extent
    (text-extent text (the-string-font)))

  (define (quoted-text-character-index-under x::real y::real
					     text::CharSequence)
    ::int
    (text-character-index-under x y text (the-string-font)))

  (define line-comment-cursor-offset::Position
    (Position left: 0 top: 0))

  (define (draw-line-comment! text::CharSequence context::Cursor)
    ::void
    (parameterize ((the-cursor-offset line-comment-cursor-offset))
      (draw-text! text (the-comment-font) context)))
  
  (define (line-comment-extent text::CharSequence)
    ::Extent
    (text-extent text (the-string-font)))
  
  (define (line-comment-character-index-under x::real y::real
					      text::CharSequence)
    ::int
    (text-character-index-under x y text (the-comment-font)))

  (define (draw-block-comment! text::CharSequence context::Cursor)
    ::void
    (let* ((font ::Font (the-block-comment-font))
	   (font-size ::real (font:getSize))
	   (outer ::Extent (block-comment-extent text))
	   (margin ::real (the-block-comment-margin)))
      (draw-rectangle! outer:width (- outer:height 5))
      (with-translation (margin (* 0.5 font-size))
	  (draw-text! text font context))))
  
  (define (block-comment-extent text::CharSequence)::Extent
    (let* ((font ::Font (the-block-comment-font))
	   (font-size ::real (font:getSize))
	   (inner ::Extent (text-extent text font))
	   (margin ::real (the-block-comment-margin)))
      (Extent width: (+ inner:width margin margin)
	      height: (+ inner:height font-size))))
  
  (define (block-comment-character-index-under x::real y::real
					       text::CharSequence)
    ::int
    (let* ((font ::Font (the-block-comment-font))
	   (font-size ::real (font:getSize))
	   (margin ::real (the-block-comment-margin)))
      (text-character-index-under (- x margin) (- y (* 0.5 font-size))
				  text font)))
  
  (define (draw-point! left::real top::real aRGB::int)::void
    (graphics:setColor (color aRGB))
    (graphics:fillOval (as int (- left 4))
		       (as int (- top 4))
		       (as int 9)
		       (as int 9)))

  (define (clear!)::void
    (error "
The `clear!' method is not implemented for the AWT,
because the screen is cleared automatically 
by the AWT framework."))
  
  (define (paint g::Graphics)::void
    (invoke-special javax.swing.JComponent (this) 'paint g)
    (set! graphics (as Graphics2D g))
    (set! (the-painter) (this))
    ;; cf. https://docs.oracle.com/javase/tutorial/2d/advanced/quality.html
    (graphics:setRenderingHints rendering-hints)
    (invoke (the-screen) 'draw! '())
    (the-overlay:draw!))

  (define (x event::MouseEvent)::real
    (event:getX))

  (define (y event::MouseEvent)::real
    (event:getY))
  
  (define (mouseClicked event::MouseEvent)::void
    (when (invoke (the-screen) 'tap! 0 #;at (x event) (y event))
      (repaint)))
  
  (define previous-x ::real 0)
  (define previous-y ::real 0)
  
  (define (mousePressed event::MouseEvent)::void
    (let ((x (x event))
	  (y (y event)))
      (when (invoke (the-screen) 'press! 0 #;at x y)
	(repaint))
      (set! previous-x x)
      (set! previous-y y)))
  
  (define (mouseDragged event::MouseEvent)::void
    (let ((x (x event))
	  (y (y event)))
      (when (invoke (the-screen) 'move!
		    0 x y (- x previous-x) (- y previous-y))
	(repaint))
      (set! previous-x x)
      (set! previous-y y)))

  (define (mouseReleased event::MouseEvent)::void
    (let ((x (x event))
	  (y (y event)))
      (when (invoke (the-screen) 'release!
		    0 x y (- x previous-x) (- y previous-y))
	(repaint))))

  (define (keyPressed event::KeyEvent)::void
    (let ((typed (event:getKeyChar)))
      (parameterize ((unicode-input (if (eqv? KeyEvent:CHAR_UNDEFINED
					      typed)
					#\null
					(integer->char typed))))
	(invoke (the-screen) 'key-typed!
		(as long (bitwise-ior
			  (as long (event:getKeyCode))
			  (if (event:control-down?) CTRL_MASK 0)
			  (if (event:alt-down?) ALT_MASK 0)
			  (if (event:shift-down?) SHIFT_MASK 0))))
	(repaint))))

  (define (componentResized event::ComponentEvent)::void
    (slot-set! (the-screen-extent) 'width
	       (invoke (this) 'getWidth))
    (slot-set! (the-screen-extent) 'height
	       (invoke (this) 'getHeight)))
  
  (InputHandler)
  (rendering-hints:put RenderingHints:KEY_ANTIALIASING
		       RenderingHints:VALUE_ANTIALIAS_ON)
  (rendering-hints:put RenderingHints:KEY_RENDERING
		       RenderingHints:VALUE_RENDER_QUALITY))

(define-parameter (ctrl-pressed?) ::boolean #f)
(define-parameter (shift-pressed?) ::boolean #f)
(define-parameter (alt-pressed?) ::boolean #f)
(define-parameter (meta-pressed?) ::boolean #f)

(define (run-in-AWT-window)::void
  (let ((application ::GRASP (GRASP)))
    (set! (the-painter) application)
    (initialize-keymap)
    (safely (load "assets/init.scm"))
    (let ((window ::javax.swing.JFrame
		  (javax.swing.JFrame title: "GRASP"
				      content-pane: application)))
      (window:addKeyListener application)
      (window:setSize 640 480)
      (window:setDefaultCloseOperation
       javax.swing.JFrame:EXIT_ON_CLOSE)
      (window:setVisible #t))))

(run-in-AWT-window)
