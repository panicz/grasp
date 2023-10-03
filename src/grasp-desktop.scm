(module-name grasp-desktop)
(module-compile-options main: #t)

(import (srfi :11))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-property))
(import (language define-cache))
(import (language define-parameter))
(import (language mapping))
(import (language fundamental))
(import (language infix))
(import (language match))
(import (utils functions))
(import (language for))
(import (language while))
(import (indexable))
(import (interactive))
(import (painter))
(import (print))
(import (primitive))
(import (cursor))
(import (input))
(import (extent))
(import (utils conversions))
(import (parse))
(import (editor-operations))
(import (history))
(import (desktop-keymap))
(import (extension))
(import (transform))
(import (pane))
(import (button))
(import (postponed))
(import (touch-event-processor))
(import (stepper))

(define-alias Font java.awt.Font)
(define-alias FontMetrics java.awt.FontMetrics)
;;(define-alias File java.io.File)
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

(define-alias System java.lang.System)

(define-alias Color java.awt.Color)

(define-constant IdentityTransform ::AffineTransform
  (AffineTransform))

(define-alias SVGLoader
  com.github.weisj.jsvg.parser.SVGLoader)

(define-alias SVGDocument
  com.github.weisj.jsvg.SVGDocument)

(define-alias FloatSize
  com.github.weisj.jsvg.geometry.size.FloatSize)

(define-alias ViewBox
  com.github.weisj.jsvg.attributes.ViewBox)

(define-cache (color aRGB::int)::Color
  (let* ((alpha ::int (- 255
			 (bitwise-and
			  #xff
			  (bitwise-arithmetic-shift
			   aRGB -24))))
	 (red ::int (bitwise-and
		     #xff
		     (bitwise-arithmetic-shift
		      aRGB -16)))
	 (green ::int (bitwise-and
		       #xff
		       (bitwise-arithmetic-shift
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

(define (load-resource path::String)::InputStream
  (or
   (ClassLoader:getResourceAsStream path)
   (ClassLoader:getResourceAsStream (string-drop path 1))))

(define (load-font path::String #!key (size ::float 12.0))::Font
  (let* ((data ::InputStream (load-resource path))
	 (font ::Font (Font:createFont
		       Font:TRUETYPE_FONT
		       data)))
    (graphics-environment:registerFont font)
    (font:deriveFont size)))

(define svg-loader ::SVGLoader (SVGLoader))

(define (load-svg path::String)::SVGDocument
  (let ((data ::InputStream (load-resource path)))
    (svg-loader:load data)))

(define (svg-extent svg::SVGDocument)::Extent
  (let ((size ::FloatSize (svg:size)))
    (Extent width: size:width
	    height: size:height)))

(define-constant Iosevka ::Font
  (load-font "/assets/iosevka-fixed-semibold.ttf" size: 16))

(define-constant BarlowCondensed ::Font
  (load-font "/assets/BarlowCondensed-Medium.ttf" size: 28))

(define-constant Basic-Regular ::Font
  (load-font "/assets/Basic-Regular.otf" size: 20))

(define-constant Oswald-Regular ::Font
  (load-font "/assets/Oswald-Regular.ttf" size: 22))

(define-constant M+1p ::Font
  (load-font "/assets/MPLUS1p-Medium.ttf" size: 22))

(define-constant GloriaHallelujah ::Font
  (load-font "/assets/GloriaHallelujah.ttf" size: 16))

(define-constant BasicRegular
  (load-font "/assets/Basic-Regular.otf" size: 21))

(define-constant NotoSerif-Regular ::Font
  (load-font "/assets/NotoSerif-Regular.ttf" size: 16))

(define-constant directory-icon ::SVGDocument
  (load-svg "/assets/directory.svg"))

(define-constant file-icon ::SVGDocument
  (load-svg "/assets/file.svg"))

(define-parameter+ (the-atom-font) ::Font
  BarlowCondensed)

(define-parameter+ (the-string-font) ::Font
  Iosevka)

(define-parameter+ (the-comment-font) ::Font
  GloriaHallelujah)

(define-parameter+ (the-menu-font) ::Font
  Oswald-Regular)

(define-parameter+ (the-caption-font) ::Font
  M+1p)

(define-parameter+ (the-text-input-font) ::Font
  NotoSerif-Regular)

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
   (moveTo 0 25)
   (lineTo 0 10)
   (quadTo 0 0 10 0)
   (lineTo 10 5)
   (quadTo 5 5 5 10)
   (lineTo 5 25)
   (closePath)))

(define-constant top-left-bounds ::Rectangle
  (top-left-paren:getBounds))

(define-constant bottom-left-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 0 15)
   (quadTo 0 25 10 25)
   (lineTo 10 20)
   (quadTo 5 20 5 15)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-left-bounds ::Rectangle
  (bottom-left-paren:getBounds))

(define-constant top-right-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 10 0 10 10)
   (lineTo 10 25)
   (lineTo 5 25)
   (lineTo 5 10)
   (quadTo 5 5 0 5)
   (closePath)))

(define-constant top-right-bounds ::Rectangle
  (top-right-paren:getBounds))

(define-constant bottom-right-paren ::Path2D
  (Path
   (moveTo 10 0)
   (lineTo 10 15)
   (quadTo 10 25 0 25)
   (lineTo 0 20)
   (quadTo 5 20 5 15)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-right-bounds ::Rectangle
  (bottom-right-paren:getBounds))

  ;; quote

(define-constant top-left-quote-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 10 0)
   (lineTo 10 5)
   (lineTo 5 5)
   (lineTo 5 25)
   (lineTo 0 25)
   (closePath)))

(define-constant top-left-quote-bounds ::Rectangle
  (top-left-quote-paren:getBounds))

(define-constant bottom-left-quote-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 0 25)
   (lineTo 10 25)
   (lineTo 10 20)
   (lineTo 5 20)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-left-quote-bounds ::Rectangle
  (bottom-left-quote-paren:getBounds))

(define-constant top-right-quote-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 10 0)
   (lineTo 10 25)
   (lineTo 5 25)
   (lineTo 5 5)
   (lineTo 0 5)
   (closePath)))

(define-constant top-right-quote-bounds ::Rectangle
  (top-right-quote-paren:getBounds))

(define-constant bottom-right-quote-paren ::Path2D
  (Path
   (moveTo 10 0)
   (lineTo 10 25)
   (lineTo 0 25)
   (lineTo 0 20)
   (lineTo 5 20)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-right-quote-bounds ::Rectangle
  (bottom-right-quote-paren:getBounds))

(define-constant quote-marker ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 5 0)
   (lineTo 5 25)
   (closePath)))

(define-constant quote-marker-bounds ::Rectangle
  (quote-marker:getBounds))

(define-constant single-quote ::Path2D
  (Path
   (moveTo 3.75 0)
   (quadTo 7.5 0 7.5 3.75)
   (quadTo 7.5 7.5 3.75 7.5)
   (quadTo 0 7.5 0 3.75)
   (quadTo 0 0 3.75 0)
   (closePath)

   (moveTo (+ 3.75 (* 0.5 3.75 (sqrt 2)))
	   (- 3.75 (* 0.5 3.75 (sqrt 2))))
   (quadTo 11.25 11.25 0 15)
   (quadTo 3.75 15 3.75 7.5)
   (closePath)
   ))

(define-constant single-quote-extent ::Rectangle
  (single-quote:getBounds))


(define-constant transparent ::Color (Color 0.0 0.0 0.0 0.0))

(define-interface InputListener
  (java.awt.event.KeyListener
   java.awt.event.FocusListener
   java.awt.event.ComponentListener
   java.awt.event.MouseMotionListener
   java.awt.event.MouseListener))

(define-interface Application (Painter
			       InputListener
			       java.awt.event.ActionListener))

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

(define-interface CancellableRunner
  (Postponed
   Cancellable
   java.awt.event.ActionListener))

(define-object (EventRunner target::java.awt.Component)
  ::CancellableRunner

  (define postponed-action ::(maps () to: boolean) never)

  (define (actionPerformed event::java.awt.event.ActionEvent)::void
    (when (postponed-action)
      (target:repaint)))

  (define timer ::javax.swing.Timer
    (let ((timer ::javax.swing.Timer
		 (javax.swing.Timer 1 (this))))
      (timer:stop)
      (timer:setRepeats #f)
      timer))

  (define (cancel)::Cancellable
    (timer:stop)
    (set! postponed-action never)
    (this))

  (define (after time-ms::long action::procedure)
    ::Cancellable
    (timer:setInitialDelay time-ms)
    (set! postponed-action action)
    (timer:start)
    (this)))

(define-object (GRASP)::Application
  (define graphics ::Graphics2D)

  (define intensity ::float 1.0)

  (define (set-color! c::Color)::void
    (let* ((A ::int (c:getAlpha))
	   (a ::int (clamp 0 (nearby-int (- 255 (* intensity A)))
			   255))
	   (aRGB ::int (bitwise-ior
			(bitwise-arithmetic-shift (c:getBlue) 0)
			(bitwise-arithmetic-shift (c:getGreen) 8)
			(bitwise-arithmetic-shift (c:getRed) 16)
			(bitwise-arithmetic-shift a 24)))
	   (c* ::Color (color aRGB)))
      (graphics:setColor c*)))

  (define (with-intensity i::float action::(maps () to: void))::void
    (let ((previous ::float intensity))
      (set! intensity i)
      (try-finally
       (action)
       (set! intensity previous))))

  (define (with-stretch horizontal::float vertical::float
			action::(maps () to: void))
    ::void
    (graphics:scale horizontal vertical)
    (try-finally
     (action)
     (graphics:scale (/ horizontal) (/ vertical))))
  
  (define directory-box ::ViewBox
    (let* ((box ::FloatSize (directory-icon:size))
	   (w/h ::float (/ box:width box:height))
	   (height ::float 24.0)
	   (width (* w/h height)))
      (ViewBox width height)))

  (define file-box ::ViewBox
    (let* ((box ::FloatSize (file-icon:size))
	   (w/h ::float (/ box:width box:height))
	   (height ::float 24.0)
	   (width (* w/h height)))
      (ViewBox width height)))

  (define icon-size ::Extent
    (Extent width: (+ 8 (max directory-box:width
			     file-box:width))
	    height: (max directory-box:height
			 file-box:height)))

  (define (icon-extent)::Extent
    icon-size)

  (define (draw-directory-icon!)::void
    (directory-icon:render (this) graphics directory-box))

  (define (draw-file-icon!)::void
    (file-icon:render (this) graphics file-box))

  (define (with-clip w::real h::real
		     action::(maps () to: void))
    ::void
    (let* ((previous-clip (graphics:getClip))
           (transform ::AffineTransform
 		      (graphics:getTransform))
           #;(x (transform:getTranslateX))
	   #;(y (transform:getTranslateY)))
      (graphics:clipRect 0 0 w h)
      (try-finally
       (action)
       (graphics:setClip previous-clip))))

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

  (define rotation ::real 0.0)
  
  (define (rotate! angle ::real)::void
    (set! rotation (+ rotation angle))
    (graphics:rotate angle))

  (define (current-rotation-angle)::real
    rotation)

  (define (scale! factor ::real)::void
    (graphics:scale factor factor))

  (define (current-scale)::real
    (let ((transform ::AffineTransform
		     (graphics:getTransform)))
      (transform:getScaleX)))
  
  (define rendering-hints ::RenderingHints
    (RenderingHints RenderingHints:KEY_TEXT_ANTIALIASING
		    RenderingHints:VALUE_TEXT_ANTIALIAS_ON))

  (define (opening-parenthesis-color context::Cursor)::Color
    (match (the-cursor)
      (`(#\[ . ,,context)
       (mark-cursor! 0 0)
       (focused-parenthesis-color))
      (`(#\] . ,,context)
       (mark-cursor! 0 0)
       (matching-parenthesis-color))
      (_
       (parenthesis-color))))

  (define (closing-parenthesis-color context::Cursor)::Color
    (match (the-cursor)
      (`(#\] . ,,context)
       (mark-cursor! 0 0)
       (focused-parenthesis-color))
      (`(#\[ . ,,context)
       (mark-cursor! 0 0)
       (matching-parenthesis-color))
      (_
       (parenthesis-color))))

  (define (open-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height))))
      (set-color! color)
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
      (set-color! color)
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
		   (opening-parenthesis-color context))
      
      (with-translation ((- width (paren-width)) 0)
	  (close-paren! height
			(closing-parenthesis-color context)))))

  (define (draw-border! width::real height::real)::void
    (graphics:fillRect 3 3 (- width 6) 4)
    (graphics:fillRect 3 3 4 (- height 6))
    (graphics:fillRect (- width 7) 3 4 (- height 6))
    (graphics:fillRect 3 (- height 7) (- width 6) 4))

  (define (border-size)::real 10)
  
  (define (space-width)::real 8)

  (define (paren-width)::real
    top-left-bounds:width)

  (define (line-simplification-resolution)::real 20)
  
  (define (open-quote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height))))
      (set-color! color)
      (graphics:fill top-left-quote-paren)
      (graphics:fillRect 0 top-left-quote-bounds:height
		       5 line-height)
      (with-translation (0 (+ top-left-quote-bounds:height
			      line-height))
	  (graphics:fill bottom-left-quote-paren))))

  (define (close-quote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 top-right-bounds:height
				 bottom-right-bounds:height))))
      (set-color! color)
      (graphics:fill top-right-quote-paren)
      (graphics:fillRect (- top-right-quote-bounds:width 5)
			 top-right-quote-bounds:height
			 5
			 line-height)

      (with-translation (0 (+ top-right-quote-bounds:height
			      line-height))
	  (graphics:fill bottom-right-quote-paren))))

  (define (draw-quote-box! width::real
			   height::real
			   context::Cursor)
    ::void
    (open-quote-paren! height (opening-parenthesis-color context))
    (with-translation ((- width (quote-paren-width)) 0)
      (close-quote-paren! height
			  (closing-parenthesis-color context))))

  (define (quote-paren-width)::real
    (+ 1 top-left-quote-bounds:width))

  (define (draw-quote-markers! width::real
			       height::real
			       context::Cursor)
    ::void
    (set-color! (opening-parenthesis-color context))
    (graphics:fill quote-marker))

  (define (quote-marker-width)::real
    (+ 1 top-left-quote-bounds:width))

  (define (open-quasiquote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height top-left-bounds:height))))
      (set-color! color)
      (graphics:fill top-left-quote-paren)
      (graphics:fillRect 0 top-left-quote-bounds:height
		       5 line-height)))

  (define (close-quasiquote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height top-right-bounds:height))))
      (set-color! color)
      (graphics:fill top-right-quote-paren)
      (graphics:fillRect (- top-right-quote-bounds:width 5)
			 top-right-quote-bounds:height
			 5 line-height)))

  (define (draw-quasiquote-box! width::real
				height::real
				context::Cursor)
    ::void
    (open-quasiquote-paren! height
			    (opening-parenthesis-color context))
    (with-translation ((- width (quasiquote-paren-width)) 0)
      (close-quasiquote-paren! height
			       (closing-parenthesis-color context))))

  (define (quasiquote-paren-width)::real
    (+ 1 top-left-quote-bounds:width))

  (define (draw-quasiquote-markers! width::real
				    height::real
				    context::Cursor)
    ::void
    (set-color! (opening-parenthesis-color context))
    (graphics:fill top-left-quote-paren)
    (with-translation ((+ width (quasiquote-marker-width)) 0)
      (set-color! (closing-parenthesis-color context))
      (graphics:fill top-right-quote-paren)))

  (define (quasiquote-marker-width)::real
    (+ 1 quote-marker-bounds:width))

  (define (open-unquote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 bottom-left-quote-bounds:height))))
      (set-color! color)
      (graphics:fillRect 0 0 5 line-height)
      (with-translation (0 line-height)
	(graphics:fill bottom-left-quote-paren))
      ))

  (define (close-unquote-paren! height::real color::Color)::void
    (let ((line-height (max 0 (- height
				 bottom-right-quote-bounds:height))))
      (set-color! color)
      (graphics:fillRect (- bottom-right-quote-bounds:width 5) 0
			 5 line-height)
      (with-translation (0 line-height)
	(graphics:fill bottom-right-quote-paren))))

  (define (draw-unquote-box! width::real
			     height::real
			     context::Cursor)
    ::void
    (open-unquote-paren! height (opening-parenthesis-color context))
    (with-translation ((- width (quasiquote-paren-width)) 0)
      (close-unquote-paren! height
			    (closing-parenthesis-color context))))

  (define (unquote-paren-width)::real
    (+ 1 bottom-left-quote-bounds:width))

  (define (draw-unquote-markers! width::real
				 height::real
				 context::Cursor)
    ::void
    (with-translation (0 (- height bottom-left-quote-bounds:height))
      (set-color! (opening-parenthesis-color context))
      (graphics:fill bottom-left-quote-paren)
      (with-translation ((+ width (quasiquote-marker-width)) 0)
	(set-color! (closing-parenthesis-color context))
	(graphics:fill bottom-right-quote-paren))))

  (define (unquote-marker-width)::real
    (+ 1 bottom-left-quote-bounds:width))

  (define (open-unquote-splicing-paren! height::real color::Color)
    ::void
    (set-color! color)
    (graphics:fillRect 0 5 1 5)
    (graphics:fillRect 3 5 3 5)
    (with-translation (5 0)
      (open-unquote-paren! height color)))

  (define (close-unquote-splicing-paren! height::real color::Color)
    ::void
    (set-color! color)
    (close-unquote-paren! height color)
    (graphics:fillRect 10 5 3 5)
    (graphics:fillRect 14 5 1 5))

  (define (draw-unquote-splicing-box!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (open-unquote-splicing-paren!
     height (opening-parenthesis-color context))
    (with-translation ((- width (unquote-splicing-paren-width)) 0)
      (close-unquote-splicing-paren!
       height (closing-parenthesis-color context))))

  (define (unquote-splicing-paren-width)::real
    (+ (unquote-paren-width) 5))

  (define (draw-unquote-splicing-markers!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (with-translation (0 (- height bottom-left-quote-bounds:height))
      (set-color! (opening-parenthesis-color context))
      (graphics:fillRect 0 5 1 10)
      (graphics:fillRect 3 5 5 10)
      (with-translation (5 0)
	(graphics:fill bottom-left-quote-paren)
	(with-translation ((+ width (quasiquote-marker-width)) 0)
	  (set-color! (closing-parenthesis-color context))
	  (graphics:fill bottom-right-quote-paren)
	  (graphics:fillRect 10 5 13 10)
	  (graphics:fillRect 14 5 15 10)))))

  (define (unquote-splicing-marker-width)::real
    (+ (unquote-marker-width) 10))

  (define (min-box-height)::real
    (max (invoke (the-atom-font) 'getSize2D)
	 (+ top-left-bounds:height bottom-left-bounds:height)
	 (+ top-right-bounds:height bottom-right-bounds:height)))

  (define (min-line-height)::real
    (invoke (the-atom-font) 'getSize2D))

  (define (fill-background! width::real height::real)::void
    (set-color! (color #xffffffff))
    (graphics:fillRect 0 0 (as int width) (as int height)))

  (define (draw-popup! width::real height::real)::void
    (graphics:setColor (color #x77AAAAAA))
    (graphics:fillRoundRect 0 0 (as int width) (as int height)
			    12 12))

  (define (horizontal-popup-margin)::real 2)
  (define (vertical-popup-margin)::real 20)

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

  (define (horizontal-split-height)::real 20)

  (define (vertical-split-width)::real 20)

  (define (draw-horizontal-split! top::real)::void
    (graphics:fillRect (max 0 (current-clip-left)) top
		       (current-clip-width)
		       (horizontal-split-height)))

  (define (draw-vertical-split! left::real)::void
    (graphics:fillRect left (max 0 (current-clip-top))
		       (vertical-split-width) (current-clip-height)))

  (define (grid-border)::real 10)

  (define (draw-horizontal-grid! width::real)::void
    (set-color! text-color)
    (graphics:fillRect 4 4 (- width 8) 2))

  (define (draw-vertical-grid! height::real)::void
    (set-color! text-color)
    (graphics:fillRect 4 4 2 (- height 8)))

  (define (fill-grid-cell! width::real height::real)::void
    (set-color! Color:WHITE)
    (graphics:fillRect 5 5 (- width 10) (- height 10)))

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
	      (set-color! background-color)
	      (graphics:fillRect left (* (- lines 1) height)
				 width height)
	      (set-color! text-color)
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
      (graphics:fill single-quote)
      (with-translation (single-quote-extent:width 0)
	(graphics:fill single-quote)
	(with-translation (single-quote-extent:width
			   single-quote-extent:height)
	  (draw-string! text context)
      	  (let ((extent ::Extent (text-extent text
					      (the-string-font))))
	    (with-translation (extent:width extent:height)
	      (graphics:fill single-quote)
	      (with-translation (single-quote-extent:width 0)
		(graphics:fill single-quote))))))))

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

  (define (draw-text-input! text::CharSequence
			    context::Cursor)
    ::void
    (draw-text! text (the-text-input-font) context))

  (define (text-input-extent text::CharSequence)::Extent
    (text-extent text (the-text-input-font)))

  (define (text-input-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (text-character-index-under x y text (the-text-input-font)))

  (define (draw-caption! caption::CharSequence)::void
    (draw-text! caption (the-caption-font) #!null))

  (define (caption-extent caption::CharSequence)::Extent
    (text-extent caption (the-caption-font)))

  (define (caption-margin-top)::real 6)

  (define (caption-margin-bottom)::real 0)

  (define (caption-horizontal-margin)::real
    (space-width))

  (define (atom-extent text::CharSequence)::Extent
    (let ((inner (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (+ inner:height 16))))

  (define atom-cursor-offset::Position (Position left: 0 top: 4))

  (define atom-frame-color ::Color (Color #xdddddd))

  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((extent (atom-extent text))
	   (font (the-atom-font)))
      (set-color! atom-frame-color)
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
    (let ((inner ::Extent (text-extent text (the-string-font))))
      (Extent width: (+ inner:width
			(* 4 single-quote-extent:width))
	      height: (+ inner:height
			 (* 2 single-quote-extent:height)))))

  (define (quoted-text-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (text-character-index-under
     (- x (* 2 single-quote-extent:width))
     (- y single-quote-extent:height)
     text (the-string-font)))

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
    (screen:draw!))

  (define pointer ::TouchEventProcessor
    (TouchEventProcessor 0 screen (EventRunner (this)) vicinity: 1))

  (define (invalidating result::boolean)::boolean
    (when result
      (repaint))
    result)

  (define (mousePressed event::MouseEvent)::void
    (invalidating
     (pointer:press! (event:getX) (event:getY)
		     (System:currentTimeMillis))))

  (define (mouseDragged event::MouseEvent)::void
    (invalidating
     (pointer:move! (event:getX) (event:getY)
		    (System:currentTimeMillis))))

  (define (mouseReleased event::MouseEvent)::void
    (invalidating
     (pointer:release! (event:getX) (event:getY)
		       (System:currentTimeMillis))))

  (define (keyPressed event::KeyEvent)::void
    (let ((typed (event:getKeyChar)))
      (parameterize ((unicode-input (if (eqv? KeyEvent:CHAR_UNDEFINED
					      typed)
					#\null
					(integer->char typed))))
	(screen:key-typed!
		(as long (bitwise-ior
			  (as long (event:getKeyCode))
			  (if (event:control-down?) CTRL_MASK 0)
			  (if (event:alt-down?) ALT_MASK 0)
			  (if (event:shift-down?) SHIFT_MASK 0)))
		'())
	(repaint))))

  (define (componentResized event::ComponentEvent)::void
    (screen:set-size! (invoke (this) 'getWidth)
		      (invoke (this) 'getHeight)))

  (define pending-animations
    ::java.util.Collection
    (java.util.concurrent.ConcurrentLinkedQueue))

  (define last-animation-event-time-ms ::long 0)
  
  (define (actionPerformed event::java.awt.event.ActionEvent)::void
    (unless (pending-animations:isEmpty)
      (let* ((now ::long (current-time-ms))
	     (delta ::long (- now last-animation-event-time-ms)))
	(for animation::Animation in pending-animations
	  (unless (animation:advance! delta)
	    (pending-animations:remove animation)))
	(set! last-animation-event-time-ms now)
	(repaint)
	(when (pending-animations:isEmpty)
	  (animator:stop)))))
  
  (define animator ::javax.swing.Timer
    (let ((timer ::javax.swing.Timer
		 (javax.swing.Timer 40 (this))))
      (timer:stop)
      (timer:setRepeats #t)
      timer))
  
  (define (play! animation::Animation)::void
    (unless (any (is _ eq? animation) pending-animations)
      (let ((was-empty? ::boolean (pending-animations:isEmpty)))
	(pending-animations:add animation)
	(when was-empty?
	  (set! last-animation-event-time-ms (current-time-ms))
	  (animator:start)))))

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
  (set! (default-transform) (lambda () (Isogonal)))
  (let ((application ::GRASP (GRASP)))
    (set! (the-painter) application)
    (initialize-keymap)
    (safely (load "./assets/init.scm"))
    (let ((window ::javax.swing.JFrame
		  (javax.swing.JFrame title: "GRASP"
				      content-pane: application)))
      (window:addKeyListener application)
      (window:setSize 640 480)
      (window:setDefaultCloseOperation
       javax.swing.JFrame:EXIT_ON_CLOSE)
      (window:setFocusTraversalKeysEnabled #f)
      (window:setVisible #t))))

(set! *print-base* 16)

(run-in-AWT-window)
