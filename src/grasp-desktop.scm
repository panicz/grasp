(module-name grasp-desktop)
(module-compile-options main: #t)

(import (srfi :11))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language attributes))
(import (language define-cache))
(import (language define-parameter))
(import (language mapping))
(import (language fundamental))
(import (language infix))
(import (language match))
(import (language while))
(import (language for))

(import (utils functions))
(import (utils print))
(import (utils conversions))

(import (editor interfaces painting))
(import (editor interfaces elements))

(import (editor types primitive))
(import (editor document cursor))
(import (editor input input))

(import (editor document parse))
(import (editor document editor-operations))
(import (editor document history-tracking))
(import (editor document copy-paste))

(import (editor input desktop-keymap))
(import (editor types extensions extensions))
(import (editor input transforms))
(import (editor input screen))
(import (editor input splits))
(import (editor input document-editor))
(import (editor types extensions widgets))
(import (editor interfaces delayed))
(import (editor input touch-event-processor))
(import (editor types extensions visual-stepper))
(import (editor types extensions testing))
(import (editor types extensions canvas))
(import (editor types extensions physics))
(import (editor types extensions module-viewer))
(import (editor types extensions gutenbergson))

(import (utils server))

(import (editor input gestures))
(import (editor awt-clipboard))

(import (utils reflection))

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
(define-alias LineDecoration java.awt.Stroke)
(define-alias BasicLineDecoration java.awt.BasicStroke)

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
  (load-font "/assets/iosevka-fixed-semibold.ttf" size: 14))

(define-constant Roboto-Medium ::Font
  (load-font "/assets/Roboto-Medium.ttf" size: 16))

(define-constant Basic-Regular ::Font
  (load-font "/assets/Basic-Regular.otf" size: 20))

(define-constant Oswald-Regular ::Font
  (load-font "/assets/Oswald-Regular.ttf" size: 22))

(define-constant M+1p ::Font
  (load-font "/assets/MPLUS1p-Medium.ttf" size: 22))

(define-constant GloriaHallelujah ::Font
  (load-font "/assets/GloriaHallelujah.ttf" size: 16))

(define-constant BasicRegular ::Font
  (load-font "/assets/Basic-Regular.otf" size: 21))

(define-constant NotoSerif-Regular ::Font
  (load-font "/assets/NotoSerif-Regular.ttf" size: 12))

(define-constant Yrsa-Regular ::Font
  (load-font "/assets/Yrsa-Regular.ttf" size: 18))

(define-constant Yrsa-Bold ::Font
  (load-font "/assets/Yrsa-Bold.ttf" size: 18))

(define-constant Yrsa-Italic ::Font
  (load-font "/assets/Yrsa-Italic.ttf" size: 18))

(define-constant Yrsa-BoldItalic ::Font
  (load-font "/assets/Yrsa-BoldItalic.ttf" size: 18))

;; large
(define-constant Iosevka-Large ::Font
  (load-font "/assets/iosevka-fixed-semibold.ttf" size: 19))

(define-constant Yrsa-Large ::Font
  (load-font "/assets/Yrsa-Regular.ttf" size: 24))

(define-constant Yrsa-LargeBold ::Font
  (load-font "/assets/Yrsa-Bold.ttf" size: 24))

(define-constant Yrsa-LargeItalic ::Font
  (load-font "/assets/Yrsa-Italic.ttf" size: 24))

(define-constant Yrsa-LargeBoldItalic ::Font
  (load-font "/assets/Yrsa-BoldItalic.ttf" size: 24))

;; extra
(define-constant Iosevka-Extra ::Font
  (load-font "/assets/iosevka-fixed-semibold.ttf" size: 23))

(define-constant Yrsa-Extra ::Font
  (load-font "/assets/Yrsa-Regular.ttf" size: 30))

(define-constant Yrsa-ExtraBold ::Font
  (load-font "/assets/Yrsa-Bold.ttf" size: 30))

(define-constant Yrsa-ExtraItalic ::Font
  (load-font "/assets/Yrsa-Italic.ttf" size: 30))

(define-constant Yrsa-ExtraBoldItalic ::Font
  (load-font "/assets/Yrsa-BoldItalic.ttf" size: 30))

;; extra
(define-constant Iosevka-ExtraLarge ::Font
  (load-font "/assets/iosevka-fixed-semibold.ttf" size: 28))

(define-constant Yrsa-ExtraLarge ::Font
  (load-font "/assets/Yrsa-Regular.ttf" size: 36))

(define-constant Yrsa-ExtraLargeBold ::Font
  (load-font "/assets/Yrsa-Bold.ttf" size: 36))

(define-constant Yrsa-ExtraLargeItalic ::Font
  (load-font "/assets/Yrsa-Italic.ttf" size: 36))

(define-constant Yrsa-ExtraLargeBoldItalic ::Font
  (load-font "/assets/Yrsa-BoldItalic.ttf" size: 36))


(define-constant directory-icon ::SVGDocument
  (load-svg "/assets/directory.svg"))

(define-constant file-icon ::SVGDocument
  (load-svg "/assets/file.svg"))

(define-constant press-mark ::SVGDocument
  (load-svg "/assets/press.svg"))

(define-constant release-mark ::SVGDocument
  (load-svg "/assets/release.svg"))

(define-parameter+ (the-atom-font) ::Font
  Roboto-Medium)

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

(define-parameter+ (the-regular-text-font)::Font
  Yrsa-Regular)

(define-parameter+ (the-monospace-font) ::Font
  Iosevka)

(define-parameter+ (the-bold-text-font)::Font
  Yrsa-Bold)

(define-parameter+ (the-italic-text-font)::Font
  Yrsa-Italic)

(define-parameter+ (the-bold-italic-text-font)::Font
  Yrsa-BoldItalic)

;; large
(define-parameter+ (the-large-text-font)::Font
  Yrsa-Large)

(define-parameter+ (the-large-monospace-font) ::Font
  Iosevka-Large)

(define-parameter+ (the-large-bold-text-font)::Font
  Yrsa-LargeBold)

(define-parameter+ (the-large-italic-text-font)::Font
  Yrsa-LargeItalic)

(define-parameter+ (the-large-bold-italic-text-font)::Font
  Yrsa-LargeBoldItalic)

;; extra
(define-parameter+ (the-extra-text-font)::Font
  Yrsa-Extra)

(define-parameter+ (the-extra-monospace-font) ::Font
  Iosevka-Extra)

(define-parameter+ (the-extra-bold-text-font)::Font
  Yrsa-ExtraBold)

(define-parameter+ (the-extra-italic-text-font)::Font
  Yrsa-ExtraItalic)

(define-parameter+ (the-extra-bold-italic-text-font)::Font
  Yrsa-ExtraBoldItalic)

;; extra large
(define-parameter+ (the-extra-large-text-font)::Font
  Yrsa-ExtraLarge)

(define-parameter+ (the-extra-large-monospace-font) ::Font
  Iosevka-ExtraLarge)

(define-parameter+ (the-extra-large-bold-text-font)::Font
  Yrsa-ExtraLargeBold)

(define-parameter+ (the-extra-large-italic-text-font)::Font
  Yrsa-ExtraLargeItalic)

(define-parameter+ (the-extra-large-bold-italic-text-font)::Font
  Yrsa-ExtraLargeBoldItalic)

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
   (moveTo 0 10)
   (quadTo 0 0 10 0)
   (lineTo 10 5)
   (quadTo 5 5 5 10)
   (closePath)))

(define-constant top-left-bounds ::Rectangle
  (top-left-paren:getBounds))

(define-constant bottom-left-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 0 10 10 10)
   (lineTo 10 5)
   (quadTo 5 5 5 0)
   (closePath)))

(define-constant bottom-left-bounds ::Rectangle
  (bottom-left-paren:getBounds))

(define-constant top-right-paren ::Path2D
  (Path
   (moveTo 0 0)
   (quadTo 10 0 10 10)
   (lineTo 5 10)
   (quadTo 5 5 0 5)
   (closePath)))

(define-constant top-right-bounds ::Rectangle
  (top-right-paren:getBounds))

(define-constant bottom-right-paren ::Path2D
  (Path
   (moveTo 10 0)
   (quadTo 10 10 0 10)
   (lineTo 0 5)
   (quadTo 5 5 5 0)
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
   (lineTo 5 10)
   (lineTo 0 10)
   (closePath)))

(define-constant top-left-quote-bounds ::Rectangle
  (top-left-quote-paren:getBounds))

(define-constant bottom-left-quote-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 0 10)
   (lineTo 10 10)
   (lineTo 10 5)
   (lineTo 5 5)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-left-quote-bounds ::Rectangle
  (bottom-left-quote-paren:getBounds))

(define-constant top-right-quote-paren ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 10 0)
   (lineTo 10 10)
   (lineTo 5 10)
   (lineTo 5 5)
   (lineTo 0 5)
   (closePath)))

(define-constant top-right-quote-bounds ::Rectangle
  (top-right-quote-paren:getBounds))

(define-constant bottom-right-quote-paren ::Path2D
  (Path
   (moveTo 10 0)
   (lineTo 10 10)
   (lineTo 0 10)
   (lineTo 0 5)
   (lineTo 5 5)
   (lineTo 5 0)
   (closePath)))

(define-constant bottom-right-quote-bounds ::Rectangle
  (bottom-right-quote-paren:getBounds))

(define-constant quote-marker ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 5 0)
   (lineTo 5 10)
   (closePath)))

(define-constant quote-marker-bounds ::Rectangle
  (quote-marker:getBounds))

(define-constant single-quote ::Path2D
  (Path
   (moveTo 0 0)
   (lineTo 5 0)
   (lineTo 5 2.5)
   (lineTo 0 2.5)
   (closePath)

   (moveTo 5 2.5)
   (lineTo 0 5)
   (lineTo 2.5 2.5)
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
   java.awt.event.MouseWheelListener
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

  (define (mouseWheelMoved event::MouseWheelEvent)::void
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
  (addMouseWheelListener (this))
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

  (define clipboard ::Clipboard
    (let* ((toolkit ::java.awt.Toolkit
		    (java.awt.Toolkit:getDefaultToolkit))
	   (clipboard ::AWTClipboard 
		      (toolkit:getSystemClipboard)))
      (AWTSystemClipboard clipboard)))
  
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

  (define press-mark-size ::Extent
    (let ((mark ::FloatSize (press-mark:size)))
      (Extent width: mark:width
	      height: mark:height)))
  
  (define (press/release-mark-extent)::Extent
    press-mark-size)
  
  (define (draw-press-mark! left::real top::real)::void
    (let ((mark ::FloatSize (press-mark:size)))
      (with-translation ((- left (/ mark:width 2))
			 (- top (/ mark:height 2)))
	  (press-mark:render (this) graphics))))

  (define (draw-release-mark! left::real top::real)::void
    (let ((mark ::FloatSize (release-mark:size)))
      (with-translation ((- left (/ mark:width 2))
			 (- top (/ mark:height 2)))
	  (release-mark:render (this) graphics))))
  
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

  (define rotation ::real 0.0)
  
  (define (rotate! angle ::real)::void
    (set! rotation (+ rotation angle))
    (graphics:rotate angle))

  (define (scale! factor ::real)::void
    (graphics:scale factor factor))
  
  (define rendering-hints ::RenderingHints
    (RenderingHints RenderingHints:KEY_TEXT_ANTIALIASING
		    RenderingHints:VALUE_TEXT_ANTIALIAS_ON))

  (define (draw-custom-box!
	   draw-left-paren!::(maps (real) to: void)
	   draw-right-paren!::(maps (real) to: void)
	   paren-width::real
	   width::real height::real context::Cursor)
    ::void
    (let ((left-color ::Color (parenthesis-color))
	  (right-color ::Color (parenthesis-color))
	  (t ::Traversal (the-traversal)))
      (match (the-cursor)
	(`(#\[ . ,,context) 
	 (set! left-color (focused-parenthesis-color))
	 (set! right-color (matching-parenthesis-color))
	 (set! t:parent-left (+ t:parent-left t:left))
	 (set! t:parent-top (+ t:parent-top t:top))
	 (mark-cursor! (quotient paren-width 2) 0)
	 (set! t:parent-top (- t:parent-top t:top))
	 (set! t:parent-left (- t:parent-left t:left)))
	(`(#\] . ,,context)
	 (set! left-color (matching-parenthesis-color))
	 (set! right-color (focused-parenthesis-color))
	 (set! t:parent-left (+ t:parent-left t:left))
	 (set! t:parent-top (+ t:parent-top t:top))
	 (mark-cursor! (- width (quotient paren-width 2)) 0)
	 (set! t:parent-top (- t:parent-top t:top))
	 (set! t:parent-left (- t:parent-left t:left)))
	(_
	 (values)))
      (set-color! left-color)
      (draw-left-paren! height)
      (with-translation ((- width paren-width) 0)
	(set-color! right-color)
	(draw-right-paren! height))))
 
  (define (open-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height))))
      (graphics:fill top-left-paren)
      (graphics:fillRect 0 top-left-bounds:height
			 5 line-height)
      (with-translation (0 (+ top-left-bounds:height
			      line-height))
	  (graphics:fill bottom-left-paren))))

  (define (close-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-bounds:height
				 bottom-right-bounds:height))))
      (graphics:fill top-right-paren)
      (graphics:fillRect (- top-right-bounds:width 5)
			 top-right-bounds:height 5 line-height)

      (with-translation (0 (+ top-right-bounds:height
			      line-height))
	  (graphics:fill bottom-right-paren))))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-paren! height))
     (lambda (width::real)::void (close-paren! height))
     (paren-width)
     width height context))

  (define (open-quote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-left-bounds:height
				 bottom-left-bounds:height))))
      (graphics:fill top-left-quote-paren)
      (graphics:fillRect 0 top-left-quote-bounds:height
		       5 line-height)
      (with-translation (0 (+ top-left-quote-bounds:height
			      line-height))
	  (graphics:fill bottom-left-quote-paren))))

  (define (close-quote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-bounds:height
				 bottom-right-bounds:height))))
      (graphics:fill top-right-quote-paren)
      (graphics:fillRect (- top-right-quote-bounds:width 5)
			 top-right-quote-bounds:height
			 5
			 line-height)

      (with-translation (0 (+ top-right-quote-bounds:height
			      line-height))
	  (graphics:fill bottom-right-quote-paren))))

  (define (quote-paren-width)::real
    (+ 1 top-left-quote-bounds:width))
  
  (define (draw-quote-box! width::real
			   height::real
			   context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void
	     (open-quote-paren! height))
     (lambda (width::real)::void
	     (close-quote-paren! height))
     (quote-paren-width)
     width height context))

  (define (open-quasiquote-paren! height::real)::void
    (let ((line-height (max 0 (- height top-left-bounds:height))))
      (graphics:fill top-left-quote-paren)
      (graphics:fillRect 0 top-left-quote-bounds:height
		       5 line-height)))

  (define (close-quasiquote-paren! height::real)::void
    (let ((line-height (max 0 (- height top-right-bounds:height))))
      (graphics:fill top-right-quote-paren)
      (graphics:fillRect (- top-right-quote-bounds:width 5)
			 top-right-quote-bounds:height
			 5 line-height)))

  (define (quasiquote-paren-width)::real
    (+ 1 top-left-quote-bounds:width))
  
  (define (draw-quasiquote-box! width::real
				height::real
				context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void
	     (invoke (this) 'open-quasiquote-paren! height))
     (lambda (width::real)::void
	     (invoke (this) 'close-quasiquote-paren! height))
     (quasiquote-paren-width)
     width height context))

  (define (open-unquote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 bottom-left-quote-bounds:height))))
      (graphics:fillRect 0 0 5 line-height)
      (with-translation (0 line-height)
	(graphics:fill bottom-left-quote-paren))
      ))

  (define (close-unquote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 bottom-right-quote-bounds:height))))
      (graphics:fillRect (- bottom-right-quote-bounds:width 5) 0
			 5 line-height)
      (with-translation (0 line-height)
	(graphics:fill bottom-right-quote-paren))))

  (define (unquote-paren-width)::real
    (+ 1 bottom-left-quote-bounds:width))

  (define (draw-unquote-box! width::real
			     height::real
			     context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-unquote-paren! height))
     (lambda (width::real)::void (close-unquote-paren! height))
     (unquote-paren-width)
     width height context))

  (define (open-unquote-splicing-paren! height::real)
    ::void
    (graphics:fillRect 0 5 1 5)
    (graphics:fillRect 3 5 3 5)
    (with-translation (5 0)
      (open-unquote-paren! height)))

  (define (close-unquote-splicing-paren! height::real)
    ::void
    (close-unquote-paren! height)
    (graphics:fillRect 10 5 3 5)
    (graphics:fillRect 14 5 1 5))

  (define (unquote-splicing-paren-width)::real
    (+ (unquote-paren-width) 5))

  (define (draw-unquote-splicing-box! width::real
				      height::real
				      context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void
	     (open-unquote-splicing-paren! height))
     (lambda (width::real)::void
	     (close-unquote-splicing-paren! height))
     (unquote-splicing-paren-width)
     width height context))

  (define (open-quote-marker! height::real)
    ::void
    (graphics:fill quote-marker))

  (define (quote-marker-width)::real
    (+ 1 top-left-quote-bounds:width))

  (define (draw-quote-markers! width::real
			       height::real
			       context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-quote-marker! height))
     (lambda (width::real)::void (values))
     (quote-marker-width)
     width height context))

  (define (open-quasiquote-marker! height::real)
    ::void
    (graphics:fill top-left-quote-paren))

  (define (close-quasiquote-marker! height::real)
    ::void
    (graphics:fill top-right-quote-paren))

  (define (quasiquote-marker-width)::real
    (+ 1 quote-marker-bounds:width))
  
  (define (draw-quasiquote-markers! width::real
				    height::real
				    context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-quasiquote-marker! height))
     (lambda (width::real)::void (close-quasiquote-marker! height))
     (quasiquote-marker-width)
     width height context))

  (define (open-unquote-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-left-quote-bounds:height))
      (graphics:fill bottom-left-quote-paren)))

  (define (close-unquote-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-right-quote-bounds:height))
      (graphics:fill bottom-right-quote-paren)))
  
  (define (unquote-marker-width)::real
    (+ 1 bottom-left-quote-bounds:width))
  
  (define (draw-unquote-markers! width::real
				 height::real
				 context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-unquote-marker! height))
     (lambda (width::real)::void (close-unquote-marker! height))
     (unquote-marker-width)
     width height context))

  (define (open-unquote-splicing-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-left-quote-bounds:height))
      (graphics:fillRect 0 0 1 5)
      (graphics:fillRect 3 0 5 5)
      (with-translation (5 0)
	(graphics:fill bottom-left-quote-paren))))

  (define (close-unquote-splicing-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-left-quote-bounds:height))
      (graphics:fill bottom-right-quote-paren)
      (graphics:fillRect 10 0 3 5)
      (graphics:fillRect 14 0 1 5)
      ))

  (define (unquote-splicing-marker-width)::real
    (+ (unquote-marker-width) 10))

  (define (draw-unquote-splicing-markers! width::real
				 height::real
				 context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void
	     (open-unquote-splicing-marker! height))
     (lambda (width::real)::void
	     (close-unquote-splicing-marker! height))
     (unquote-splicing-marker-width)
     width height context))
  
  (define (draw-border! width::real
			height::real
			highlight::(subtype-of
				    gnu.text.Char
				    (either
				     #\[
				     #\]
				     #\t
				     #\null)))
    ::void
    (set-color!
     (match highlight
       (#\[
	(focused-parenthesis-color))
       (#\]
	(matching-parenthesis-color))
       (#\null
	(parenthesis-color))
       (_
	text-color)))
    (graphics:fillRect 3 3 (- width 6) 4)
    (graphics:fillRect 3 3 4 (- height 6))
    (set-color!
     (match highlight
       (#\[
	(matching-parenthesis-color))
       (#\]
	(focused-parenthesis-color))
       (#\null
	(parenthesis-color))
       (_
	text-color)))
    (graphics:fillRect (- width 7) 3 4 (- height 6))
    (graphics:fillRect 3 (- height 7) (- width 6) 4))

  (define (border-size)::real 15)

  (define (height/width-ratio)::real 1)

  (define (module-view-interline)::real 20)

  (define (module-view-interspace)::real 15)
  
  (define (space-width)::real 8)

  (define (paren-width)::real
    top-left-bounds:width)

  (define (line-simplification-resolution)::real 20)
  
  (define (min-box-height)::real
    (max (invoke (the-atom-font) 'getSize2D)
	 (+ top-left-bounds:height bottom-left-bounds:height)
	 (+ top-right-bounds:height bottom-right-bounds:height)))

  (define (min-line-height)::real
    (invoke (the-atom-font) 'getSize2D))

  (define (fill-background! width::real height::real)::void
    (set-color! (color #xffffff))
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

  (define (draw-dashed-rectangle! width::real height::real)::void
    (let ((s ::LineDecoration (graphics:getStroke)))
      (graphics:setStroke dashed)
      (graphics:drawRect 0 0 (as int width) (as int height))
      (graphics:setStroke s)))
  
  (define (mark-editor-cursor! +left::real +top::real
			       editor::WithCursor)
    ::void
    (let ((cursor-extent ::Extent (the-cursor-extent))
	  (cursor-offset ::Position (the-cursor-offset))
	  (traversal ::Traversal (the-traversal)))
      (editor:mark-cursor! (as int (+ traversal:parent-left +left))
			   (as int (+ traversal:parent-top +top)))
      (graphics:fillRect (+ +left cursor-offset:left)
			 (+ +top cursor-offset:top)
			 cursor-extent:width
			 cursor-extent:height)))

  (define (mark-cursor! +left::real +top::real)::void
    (mark-editor-cursor! +left +top (the-editor)))
  
  (define (editor-cursor-position editor::WithCursor)::Position
    (editor:marked-cursor-position))

  (define (marked-cursor-position)::Position
    (editor-cursor-position (the-editor)))

  (define (cursor-height)::real
    (let ((offset ::Position (the-cursor-offset))
	  (extent ::Extent (the-cursor-extent)))
      (+ offset:top extent:height)))

  (define text-color ::Color Color:DARK_GRAY)

  (define background-color ::Color transparent)

  (define highlight-count::(array-of byte)
    ((array-of byte) length: (length (HighlightType:values))))

  (define (set-highlight-color!)
    (cond
     ((is (highlight-count
	   (HighlightType:CurrentFinding:ordinal)) > 0)
      (set! text-color Color:WHITE)
      (set! background-color Color:ORANGE))
     ((is (highlight-count
	   (HighlightType:OtherFinding:ordinal)) > 0)
      (set! text-color Color:WHITE)
      (set! background-color Color:YELLOW))
     ((is (highlight-count
	   (HighlightType:Selection:ordinal)) > 0)
      (set! text-color Color:WHITE)
      (set! background-color Color:DARK_GRAY))
     ((is (highlight-count
	   (HighlightType:CurrentChoice:ordinal)) > 0)
      (set! text-color Color:BLACK)
      (set! background-color transparent))
     (else
      (set! text-color Color:DARK_GRAY)
      (set! background-color transparent))
     ))

  (define (begin-highlight! type::HighlightType)::void
    (let ((type-index (type:ordinal)))
      (set! (highlight-count type-index)
	    (+ (highlight-count type-index) 1))
      (set-highlight-color!)))

  (define (end-highlight! type::HighlightType)::void
    (let ((type-index (type:ordinal)))
      (set! (highlight-count type-index)
	    (- (highlight-count type-index) 1))
      (set-highlight-color!)))
  
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

  (define (draw-horizontal-bar! width::real
				highlighted?::boolean)
    ::void
    (if highlighted?
	(set-color! (focused-parenthesis-color))
	(set-color! (parenthesis-color)))
    (graphics:fillRect 0 0 width (horizontal-bar-height)))

  (define (draw-vertical-bar! height::real
			      highlighted?::boolean)::void
    (if highlighted?
	(set-color! (focused-parenthesis-color))
	(set-color! (parenthesis-color)))
    (graphics:fillRect 0 0 (vertical-bar-width) height))

  (define (horizontal-split-height)::real 10)

  (define (vertical-split-width)::real 10)

  (define (draw-horizontal-split! top::real)::void
    (set-color! text-color)
    (graphics:fillRect (max 0 (current-clip-left)) top
		       (current-clip-width)
		       (horizontal-split-height)))

  (define (draw-vertical-split! left::real)::void
    (set-color! text-color)
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

  (define thick ::LineDecoration
    (BasicLineDecoration 4))
  
  (define (draw-thick-line! x0::real y0::real
			    x1::real y1::real)
    ::void
    (graphics:setStroke thick)
    (graphics:drawLine (as int (round x0))
		       (as int (round y0))
		       (as int (round x1))
		       (as int (round y1))))

  (define thin ::LineDecoration
    (BasicLineDecoration 1))
  
  (define (draw-thin-line! x0::real y0::real
			   x1::real y1::real)
    ::void
    (graphics:setStroke thin)
    (graphics:drawLine (as int (round x0))
		       (as int (round y0))
		       (as int (round x1))
		       (as int (round y1))))

  (define (precise-resolution-right)::ubyte 1)
  (define (precise-resolution-down)::ubyte 1)

  (define (precise-inside-out px::real py::real)
    ::(Values real real)
    (values px py))
  
  (define (precise-outside-in x::real y::real)
    ::(Values real real)
    (values x y))

  (define (precise-draw-circle! px0::real py0::real r::real
				c::uint)
    ::void
    (graphics:setStroke thick)
    (set-color! (color c))
    (let ((2r (+ r r)))
      (graphics:drawOval (- px0 r) (- py0 r) 2r 2r)))
  
  (define (precise-fill-circle! px0::real py0::real r::real
				c::uint)
    ::void
    (set-color! (color c))
    (let ((2r (+ r r)))
      (graphics:fillOval (- px0 r) (- py0 r) 2r 2r)))

  (define (precise-fill-rectangle! left::real top::real
				   right::real bottom::real
				   rgb::uint)
    ::void
    (set-color! (color (bitwise-xor #xff000000 rgb)))
    (graphics:fillRect (as int left) (as int top)
		       (as int (- right left))
		       (as int (- bottom top))))
  
  (define (precise-draw-line! px0::real py0::real
			      px1::real py1::real
			      c::uint)
    ::void
    (set-color! (color c))
    (draw-thin-line! px0 py0 px1 py1))
  
  (define (measure-text-index-position-into!
	   target::Position text::CharSequence index::int
	   font::Font)
    ::Position
    (let* ((segment-start 0)
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (height ::float (metrics:getHeight))
	   (string-end (text:length)))
      (escape-with break
	(for i from 0 below string-end
	     (when (eq? (text:charAt i) #\newline)
	       (set! target:top (+ target:top height))
	       (set! segment-start i))
	     (when (= i index)
	       (let ((line (text:subSequence segment-start i)))
		 (set! target:left (+ target:left
				      (metrics:stringWidth line))))
	       (break))))
      target))

  (define (draw-text! text::CharSequence
		      font::Font
		      context::Cursor)
    ::void
    (let* ((focused? (and (pair? (the-cursor))
			  (equal? context
				  (cdr (the-cursor)))))
	   (highlights (the-highlights))
	   (highlight-starts
	    ::(list-of Highlight)
	    (only (lambda (highlight::Highlight)
		    (and-let* ((`(,_ . ,,context)
				highlight:start))))
		  highlights))
	   (highlight-ends
	    ::(list-of Highlight)
	    (only (lambda (highlight::Highlight)
		    (and-let* ((`(,_ . ,,context)
				highlight:end))))
		  highlights))
	   (metrics ::FontMetrics
		    (graphics:getFontMetrics font))
	   (parent ::Traversal (the-traversal))
	   (height ::float (metrics:getHeight))
	   (traversal ::Traversal
		      (Traversal
		       max-line-height: height
		       parent-left: (+ parent:parent-left
				       parent:left)
		       parent-top: (+ parent:parent-top
				      parent:top)
		       parent: parent))	     
	   (segment-start 0)
	   (string-end (text:length)))
      (parameterize ((the-cursor-extent
		      (Extent width: 2
			      height: height))
		     (the-traversal traversal))
	(define (render-fragment! segment-end::int)
	  (let* ((fragment (text:subSequence
			    segment-start
			    segment-end))
		 (width (metrics:stringWidth fragment)))
	    (set-color! background-color)
	    (graphics:fillRect traversal:left
			       traversal:top
			       width height)
	    (set-color! text-color)
	    (graphics:drawString fragment
				 (as float
				     traversal:left)
				 (as float
				     (+ traversal:top
					height)))
	    (traversal:expand-by! width)))

	(graphics:setFont font)
	(for i from 0 below string-end
	     (when (and focused? (eqv? (head (the-cursor)) i))
	       (render-fragment! i)
	       (set! segment-start i)
	       (mark-cursor! traversal:left traversal:top))

	     (when (any (is (car _:start) eqv? i)
			highlight-starts)
	       (render-fragment! i)
	       (set! segment-start i)
	       (for highlight::Highlight in highlight-starts
		 (when (eqv? (car highlight:start) i)
		   (begin-highlight! highlight:type))))

	     (when (any (is (car _:end) eqv? i)
			highlight-ends)
	       (render-fragment! i)
	       (set! segment-start i)
	       (for highlight::Highlight in highlight-ends
		 (when (eqv? (car highlight:end) i)
		   (end-highlight! highlight:type))))

	     (when (eq? (text:charAt i) #\newline)
	       (render-fragment! i)
	       (traversal:on-end-line #t)
	       (traversal:new-line!)
	       (set! traversal:max-line-height height)
	       (set! segment-start (+ i 1))))
	(render-fragment! string-end)
	
	(for highlight::Highlight in highlight-starts
	  (when (eqv? (car highlight:start) string-end)
	    (begin-highlight! highlight:type)))
	
	(for highlight::Highlight in highlight-ends
	  (when (eqv? (car highlight:end) string-end)
	    (end-highlight! highlight:type)))
	
	(when (and focused? (eqv? (head (the-cursor))
				  string-end))
	  (mark-cursor! traversal:left traversal:top))
	(traversal:on-end-line #f))))

  (define (decorated-font style::TextDecoration)::Font
    (cond
     ((style:contains TextStyle:Monospace)
      (cond
       ((and (is TextStyle:Extra in style)
	     (is TextStyle:Large in style))
	(the-extra-large-monospace-font))
       ((is TextStyle:Extra in style)
	(the-extra-monospace-font))
       ((is TextStyle:Large in style)
	(the-large-monospace-font))
       (else
	(the-monospace-font))))
     ((and (style:contains TextStyle:Bold)
	   (style:contains TextStyle:Italic))
      (cond
       ((and (is TextStyle:Extra in style)
	     (is TextStyle:Large in style))
	(the-extra-large-bold-italic-text-font))
       ((is TextStyle:Extra in style)
	(the-extra-bold-italic-text-font))
       ((is TextStyle:Large in style)
	(the-large-bold-italic-text-font))
       (else
	(the-bold-italic-text-font))))
     ((style:contains TextStyle:Bold)
      (cond
       ((and (is TextStyle:Extra in style)
	     (is TextStyle:Large in style))
	(the-extra-large-bold-text-font))
       ((is TextStyle:Extra in style)
	(the-extra-bold-text-font))
       ((is TextStyle:Large in style)
	(the-large-bold-text-font))
       (else
	(the-bold-text-font))))
     ((style:contains TextStyle:Italic)
      (cond
       ((and (is TextStyle:Extra in style)
	     (is TextStyle:Large in style))
	(the-extra-large-italic-text-font))
       ((is TextStyle:Extra in style)
	(the-extra-italic-text-font))
       ((is TextStyle:Large in style)
	(the-large-italic-text-font))
       (else
	(the-italic-text-font))))
     (else
      (cond
       ((and (is TextStyle:Extra in style)
	     (is TextStyle:Large in style))
	(the-extra-large-text-font))
       ((is TextStyle:Extra in style)
	(the-extra-text-font))
       ((is TextStyle:Large in style)
	(the-large-text-font))
       (else
	(the-regular-text-font))))))
  
  (define (styled-text-width text::Word style::TextDecoration)::real
    (let* ((font ::Font (decorated-font style))
	   (metrics ::FontMetrics (graphics:getFontMetrics font)))
      (metrics:stringWidth text)))

  (define (styled-text-height style::TextDecoration)::real
    (let ((font ::Font (cond
			((and (is TextStyle:Extra in style)
			      (is TextStyle:Large in style))
			 (the-extra-large-text-font))
			((is TextStyle:Extra in style)
			 (the-extra-text-font))
			((is TextStyle:Large in style)
			 (the-large-text-font))
			(else
			 (the-regular-text-font)))))
      (font:getSize)))
  
  (define (draw-styled-text! left::real top::real
			     text::CharSequence style::TextDecoration)
    ::void
    (let* ((font ::Font (decorated-font style))
	   (mono ::Font (cond
			 ((and (is TextStyle:Extra in style)
			       (is TextStyle:Large in style))
			  (the-extra-large-monospace-font))
			 ((is TextStyle:Extra in style)
			  (the-extra-monospace-font))
			 ((is TextStyle:Large in style)
			  (the-large-monospace-font))
			 (else
			  (the-monospace-font))))
	   (height ::float (mono:getSize)))
      (set-color! text-color)
      (graphics:setFont font)
      (graphics:drawString text (as float left) (as float (+ top height)))))
  
  (define (draw-string! text::CharSequence context::Cursor)::void
    (draw-text! text (the-string-font) context))

  (define (measure-string-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (measure-text-index-position-into! target
				       text
				       index
				       (the-string-font)))
  
  (define quoted-text-cursor-offset::Position
    (Position left: -1 top: 2))
  
  (define dashed ::LineDecoration
    (BasicLineDecoration
     1 BasicLineDecoration:CAP_BUTT
     BasicLineDecoration:JOIN_BEVEL
     0 ((array-of float) 9) 0))

  (define (measure-quoted-text-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (set! target:left (+ target:left
			 (* 2 single-quote-extent:width)))
    (set! target:top (+ target:top
			single-quote-extent:height))
    (measure-string-index-position-into! target text index))
  
  (define (draw-quoted-text! text::CharSequence context::Cursor)::void
    (let* ((e ::Extent (text-extent text
				    (the-string-font)))
	   (w ::real single-quote-extent:width)
	   (h ::real single-quote-extent:height)
	   (2w ::int (* 2 w))
	   (h/2 ::int (quotient h 2))
	   (r ::real (min 2w h))
	   (r/2 ::real (quotient r 2))
	   (s ::LineDecoration (graphics:getStroke)))
      (graphics:setStroke dashed)
      (graphics:drawLine 2w h/2 (as int (+ 2w e:width)) h/2)
      (graphics:drawLine w h w (as int (+ h e:height)))
      (graphics:drawLine 2w (as int (+ h e:height h/2))
			 (as int (+ 2w e:width))
			 (as int (+ h e:height h/2)))
      (graphics:drawLine (as int (+ 2w e:width w)) h
			 (as int (+ 2w e:width w))
			 (as int (+ h e:height)))
      (graphics:setStroke s)
      (parameterize ((the-cursor-offset quoted-text-cursor-offset))
	(graphics:fill single-quote)
	(with-translation (w 0)
	  (graphics:fill single-quote)
	  (with-translation (w (/ h 2))
	    (let ((t ::Traversal (the-traversal)))
	      (set! t:left (+ t:left 2w))
	      (set! t:top (+ t:top h))
	      (draw-string! text context)  
	      (set! t:left (- t:left w))
	      (set! t:top (- t:top h)))
	    (with-translation (e:width e:height)
	      (graphics:fill single-quote)
	      (with-translation (w 0)
		(graphics:fill single-quote)))
      	    )))))

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
	      height: (* lines line-height))))

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

  (define (measure-text-input-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (measure-text-index-position-into! target
				       text
				       index
				       (the-text-input-font)))
  
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

  (define (caption-margin-top)::real 0)

  (define (caption-margin-bottom)::real 16)

  (define (caption-horizontal-margin)::real
    (space-width))

  (define (atom-extent text::CharSequence)::Extent
    (let ((inner (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (+ inner:height 16))))

  (define atom-cursor-offset::Position (Position left: 0 top: 4))

  (define atom-frame-color ::Color (Color #xdddddd))

  (define (measure-atom-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (set! target:left (+ target:left 4))
    (set! target:top (+ target:top 8))
    (measure-text-index-position-into! target
				       text
				       index
				       (the-atom-font)))
  
  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((extent (atom-extent text))
	   (font (the-atom-font)))
      (set-color! atom-frame-color)
      (graphics:fillRoundRect 0 10
			      extent:width (- extent:height 16)
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
			(* 4 single-quote-extent:width) 1)
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

  (define (measure-line-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (measure-text-index-position-into! target
				       text
				       index
				       (the-comment-font)))
  
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

  (define (measure-block-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (let* ((font ::Font (the-block-comment-font))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (font-size ::real (metrics:getHeight)))
      (set! target:left (+ target:left
			   (the-block-comment-margin)))
      (set! target:top (+ target:top (/ font-size 4)))
      (measure-text-index-position-into! target text index
					 font)))
  
  (define (draw-block-comment! text::CharSequence context::Cursor)
    ::void
    (let* ((font ::Font (the-block-comment-font))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (font-size ::real (metrics:getHeight))
	   (outer ::Extent (block-comment-extent text))
	   (margin ::real (the-block-comment-margin)))
      (draw-rectangle! outer:width (- outer:height 5))
      (with-translation (margin (/ font-size 4))
	  (draw-text! text font context))))

  (define (block-comment-extent text::CharSequence)::Extent
    (let* ((font ::Font (the-block-comment-font))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (font-size ::real (metrics:getHeight))
	   (inner ::Extent (text-extent text font))
	   (margin ::real (the-block-comment-margin)))
      (Extent width: (+ inner:width margin margin)
	      height: (+ inner:height font-size))))

  (define (block-comment-character-index-under x::real y::real
					       text::CharSequence)
    ::int
    (let* ((font ::Font (the-block-comment-font))
	   (metrics ::FontMetrics (graphics:getFontMetrics font))
	   (font-size ::real (metrics:getHeight))
	   (margin ::real (the-block-comment-margin)))
      (text-character-index-under (- x margin) (- y (* 0.5 font-size))
				  text font)))

  (define (draw-point! left::real top::real aRGB::int)::void
    (graphics:setColor (color aRGB))
    (graphics:fillOval (as int (- left 4))
		       (as int (- top 4))
		       (as int 9)
		       (as int 9)))

  (define (request-redraw!)::void
    (repaint))
  
  (define (clear!)::void
    (error "
The `clear!' method is not implemented for the AWT,
because the screen is cleared automatically
by the AWT framework."))

  (define (paint g::Graphics)::void
    (invoke-special javax.swing.JComponent (this) 'paint g)
    (set! graphics (as Graphics2D g))
    (set! painter (this))
    ;; cf. https://docs.oracle.com/javase/tutorial/2d/advanced/quality.html
    (graphics:setRenderingHints rendering-hints)
    (set-color! (color #x7f7f7f))
    (graphics:fillRect 0 0 (invoke (this) 'getWidth)
		       (invoke (this) 'getHeight))
    (screen:render!))

  (define pointer ::TouchEventProcessor
    (TouchEventProcessor 0 screen (EventRunner (this)) vicinity: 1))

  (define (invalidating result::boolean)::boolean
    (when result
      (repaint))
    result)

  (define (mousePressed event::MouseEvent)::void
    (parameterize ((the-system-clipboard clipboard))
      (invalidating
       (pointer:press! (event:getX) (event:getY)
		       (System:currentTimeMillis)))))

  (define (mouseWheelMoved event::MouseWheelEvent)::void
    (parameterize ((the-system-clipboard clipboard))
      (let ((direction ::real (+ (event:getWheelRotation)
				 (event:getPreciseWheelRotation)))
	    (pointer ::Position (last-known-pointer-position 0)))
	(set! pointer:left (event:getX))
	(set! pointer:top (event:getY))
	(screen:key-typed!
	 (as long
	     (bitwise-ior
	      (if (is direction < 0)
		  KeyEvent:WHEEL_UP
		  KeyEvent:WHEEL_DOWN)
	      (if (event:control-down?) CTRL_MASK 0)
	      (if (event:alt-down?) ALT_MASK 0)
	      (if (event:shift-down?) SHIFT_MASK 0)))
	 '()))
      (repaint)))
  
  (define (mouseDragged event::MouseEvent)::void
    (parameterize ((the-system-clipboard clipboard))
      (invalidating
       (pointer:move! (event:getX) (event:getY)
		      (System:currentTimeMillis)))))

  (define (mouseReleased event::MouseEvent)::void
    (parameterize ((the-system-clipboard clipboard))
      (invalidating
       (pointer:release! (event:getX) (event:getY)
			 (System:currentTimeMillis)))))

  (define (keyPressed event::KeyEvent)::void
    (let ((typed (event:getKeyChar)))
      (parameterize ((unicode-input (if (or (eqv? KeyEvent:CHAR_UNDEFINED
						  typed)
					    (and (java.lang.Character:isISOControl typed)
						 (isnt typed eqv? #\return)
						 (isnt typed eqv? #\newline)))
					#\null
					(integer->char typed)))
		     (the-system-clipboard clipboard))
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
		      (invoke (this) 'getHeight)
		      (screen:resize-anchor (invoke (this) 'getHeight)))
    (repaint))

  (define pending-animations
    ::java.util.Collection
    (java.util.concurrent.ConcurrentLinkedQueue))

  (define last-animation-event-time-ms ::long 0)
  
  (define (actionPerformed event::java.awt.event.ActionEvent)::void
    (unless (pending-animations:isEmpty)
      (let* ((now ::long (current-time-ms))
	     (delta ::long (- now last-animation-event-time-ms)))
	(for animation::Animation in pending-animations
	  (unless (safely (animation:advance! delta))
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

  (define (stop-playing! animation::Animation)::void
    (pending-animations:remove animation))

  (define (playing? animation::Animation)::boolean
    (any (is _ eq? animation) pending-animations))
  
  (define (play! animation::Animation)::void
    (unless (any (is _ eq? animation) pending-animations)
      (let ((was-empty? ::boolean (pending-animations:isEmpty)))
	(pending-animations:add animation)
	(when was-empty?
	  (set! last-animation-event-time-ms (current-time-ms))
	  (animator:start)))))

  (InputHandler)
  
  (java.awt.EventQueue:invokeAndWait
   (lambda ()
     (set! (default-transform) (lambda () (Isogonal)))))

  (rendering-hints:put RenderingHints:KEY_ANTIALIASING
		       RenderingHints:VALUE_ANTIALIAS_ON)
  (rendering-hints:put RenderingHints:KEY_RENDERING
		       RenderingHints:VALUE_RENDER_QUALITY)
  (rendering-hints:put RenderingHints:KEY_FRACTIONALMETRICS
		       RenderingHints:VALUE_FRACTIONALMETRICS_ON)
  
  )

(define-parameter (ctrl-pressed?) ::boolean #f)
(define-parameter (shift-pressed?) ::boolean #f)
(define-parameter (alt-pressed?) ::boolean #f)
(define-parameter (meta-pressed?) ::boolean #f)

(set! (default-transform) (lambda () (Isogonal)))

(define save-state ::procedure
  (lambda () (values)))

(define (before-possible-exit action ::procedure)::void
  (set! save-state action))

(define (open-asset file-name ::string)::gnu.kawa.io.InPort
  (gnu.kawa.io.InPort
   (java.io.InputStreamReader
    (load-resource (string-append "/assets/" file-name)))))

(define (run-in-AWT-window)::void
  (let* ((application ::GRASP (GRASP))
	 (runtime ::java.lang.Runtime
		  (java.lang.Runtime:getRuntime))
	 (window ::javax.swing.JFrame
		 (javax.swing.JFrame title: "GRASP"
				     content-pane: application))
	 (input (gnu.kawa.io.InPort
		 (java.io.InputStreamReader
		  (load-resource "/assets/init.scm"))))
	 (init-script (read-all input))
	 (scheme kawa.standard.Scheme:instance)
	 (env (scheme:getEnvironment)))
    
    (runtime:addShutdownHook (object (java.lang.Thread)
			       ((run)::void
				(save-state))))
    
    (set! (the-system-clipboard) application:clipboard)
    
    (set! painter application)
    (initialize-keymap)

    (window:addKeyListener application)
    (window:setSize 640 480)
    (window:setDefaultCloseOperation
     javax.swing.JFrame:EXIT_ON_CLOSE)
    (window:setFocusTraversalKeysEnabled #f)
    (window:setVisible #t)
    (window:setMaximizedBounds (graphics-environment:getMaximumWindowBounds))
    
    (env:define 'input-files #!null (cdr (command-line)))

    (env:define 'ask #!null
		(lambda question ::string
			;;(WARN "speech recognition unavailable")
			#!null))
    (env:define 'application-directory #!null
		(lambda ()::string
			(invoke-static
			 java.lang.System
			 'getProperty "user.dir")))

    (env:define 'projects-directory #!null
		(lambda ()::string
			(invoke-static
			 java.lang.System
			 'getProperty "user.home")))
    
    (env:define 'set-window-title! #!null
		(lambda (title::string)::void
			(window:setTitle title)))

    (env:define 'maximize/unmaximize! #!null
		(lambda ()
		  (let ((state (window:getExtendedState))
			(maximize javax.swing.JFrame:MAXIMIZED_BOTH))
		    (if (isnt (bitwise-and state maximize) = 0)
			(window:setExtendedState (bitwise-and (bitwise-not maximize)
							      state))
			(window:setExtendedState (bitwise-ior state maximize))))))
    
    (env:define 'show-keyboard! #!null
		(lambda ()::void
			(values)))
    
    (env:define 'say #!null
		(lambda words ::void
			;;(WARN "speech synthesis umavailable")
			(values)))

    (env:define 'before-possible-exit #!null
		(lambda (action::procedure)::void
			(set! save-state action)))
    
    (env:define 'open-asset #!null open-asset)

    (for expression in init-script
      (safely (eval expression)))
    (screen:set-size! (window:getWidth) (window:getHeight)
		      (screen:resize-anchor (window:getHeight)))
    (window:repaint)))

;;(set! *print-base* 16)

(run-in-AWT-window)
