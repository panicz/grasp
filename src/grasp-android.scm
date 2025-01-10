(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language attributes))
(import (language define-cache))
(import (language define-parameter))
(import (language keyword-arguments))

(import (language fundamental))
(import (language infix))
(import (language match))
(import (language while))
(import (language for))

(import (utils hash-table))
(import (utils functions))
(import (utils print))
(import (utils conversions))

(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor interfaces delayed))

(import (editor input transforms))
(import (editor input screen))
(import (editor input splits))
(import (editor input document-editor))
(import (editor types primitive))
(import (editor types spaces))

(import (editor document parse))
(import (editor document cursor))
(import (editor document editor-operations))
(import (editor document copy-paste))

(import (editor input input))
(import (editor input android-keymap))

(import (editor input touch-event-processor))
(import (editor document history-tracking))
(import (editor types extensions extensions))
(import (editor types extensions widgets))
(import (editor types extensions visual-stepper))
(import (editor types extensions testing))
(import (editor types extensions canvas))

(import (editor types texts))
(import (editor input evaluation))
(import (editor input gestures))

(define-alias BlockingQueue java.util.concurrent.BlockingQueue)
(define-alias ArrayBlockingQueue
  java.util.concurrent.ArrayBlockingQueue)

(define-alias Bundle android.os.Bundle)
;;(define-alias KeyEvent android.view.KeyEvent)
(define-alias MotionEvent android.view.MotionEvent)
(define-alias Canvas android.graphics.Canvas)
(define-alias AndroidActivity android.app.Activity)
(define-alias AndroidView android.view.View)
(define-alias Paint android.graphics.Paint)
(define-alias ViewTreeObserver
  android.view.ViewTreeObserver)

(define-alias AndroidClipboard android.content.ClipboardManager)

(define-alias AndroidClipData android.content.ClipData)

(define-alias Typeface android.graphics.Typeface)
(define-alias InputMethodManager
  android.view.inputmethod.InputMethodManager)
(define-alias Path2D android.graphics.Path)
(define-alias PathEffect android.graphics.PathEffect)
(define-alias DashPathEffect
  android.graphics.DashPathEffect)

(define-alias RectF android.graphics.RectF)
(define-alias Rect android.graphics.Rect)

(define-alias AssetManager
  android.content.res.AssetManager)

(define-alias WindowManager
   android.view.WindowManager)

(define-alias PackageManager
  android.content.pm.PackageManager)

(define-alias Manifest android.Manifest)

(define-alias Intent android.content.Intent)
(define-alias RecognizerIntent android.speech.RecognizerIntent)
(define-alias Ear android.speech.SpeechRecognizer)
(define-alias Mouth android.speech.tts.TextToSpeech)
(define-alias AudioManager android.media.AudioManager)
(define-alias Uri android.net.Uri)

(define-type (Named thing: Object name: string)
  extending Base with
  ((getName)::String name))

(define-alias ContentResolver
  android.content.ContentResolver)

(define-alias DbCursor android.database.Cursor)
(define-alias DbColumn android.provider.OpenableColumns)

(define-alias DisplayMetrics
  android.util.DisplayMetrics)
(define-alias Color android.graphics.Color)

(define-alias AndroidResources
  android.content.res.Resources)

(define-alias AndroidConfiguration
  android.content.res.Configuration)

(define-alias AndroidWindow
  android.view.Window)

(define-alias WindowInsets
  android.view.WindowInsets)

(define-alias SensorListener
  android.hardware.SensorListener)

(define-alias PreserveAspectRatio
  com.caverock.androidsvg.PreserveAspectRatio)

(define-alias SVG com.caverock.androidsvg.SVG)

(define-syntax-rule (Path (command args ...) ...)
  (let ((path ::Path2D (Path2D)))
    (invoke path 'command (as float args) ...)
    ...
    path))

(define (path-extent path::Path2D)::Extent
  (let ((rect ::RectF (RectF)))
    (path:computeBounds rect #t)
    (Extent width: (rect:width)
	    height: (rect:height))))

(define-type (Font face: Typeface
		   size: real))

(define (load-font name::string activity::AndroidActivity)
  ::Typeface
   (Typeface:createFromAsset (activity:getAssets) name))

(define/kw (load-svg name::string
		     activity::AndroidActivity
		     width: width ::real := +nan.0
		     height: height ::real := +nan.0)
  ::SVG
  (let* ((svg ::SVG (SVG:getFromAsset (activity:getAssets)
				      name))
	 (preserve ::PreserveAspectRatio
		   PreserveAspectRatio:START))
    (when (and (or (isnt width nan?) (isnt height nan?))
	       (or (is width nan?) (is height nan?)))
      (svg:setDocumentPreserveAspectRatio preserve))
    (when (isnt width nan?)
      (svg:setDocumentWidth (as float width)))
    (when (isnt height nan?)
      (svg:setDocumentHeight (as float height)))
    svg))

(define the-view ::AndroidView #!null)

(define (recognize-speech prompt::String)
  ::Intent
  (let ((intent ::Intent
		(Intent
		 RecognizerIntent:ACTION_RECOGNIZE_SPEECH)))
    (intent:putExtra RecognizerIntent:EXTRA_LANGUAGE_MODEL
		     RecognizerIntent:LANGUAGE_MODEL_FREE_FORM)
    (when prompt
      (intent:putExtra RecognizerIntent:EXTRA_PROMPT
		       prompt))
    intent))
   
(define-object (EventCanceller action::java.lang.Runnable
			       sync::android.os.Handler)
  ::Cancellable
  (define (cancel)::Cancellable
    (sync:removeCallbacks action)
    cancellable-nothing))

(define-object (EventRunner sync::android.os.Handler
			    view::AndroidView)
  ::Postponed
  (define (after time-ms::long action::procedure)
    ::Cancellable
    (let ((action* ::java.lang.Runnable
		   (object (java.lang.Runnable)
		     ((run)::void
		      (when (action)
			(view:postInvalidate))))))
      (sync:postDelayed action* time-ms)
      (EventCanceller action* sync))))

(define-syntax define-initializer
  (syntax-rules (::)
    ((define-initializer (initializer-name . args)
       (definition name etc ... initialization)
       ...)
     (begin
       (definition name etc ... #!null)
       ...
       (define (initializer-name . args)
	 ::void
	 (set! name initialization)
	 ...)))))

(define-initializer (initialize-activity activity::GRASP)
  (define the-activity ::AndroidActivity activity)
  
  (define Iosevka ::Typeface
    (load-font "iosevka-fixed-semibold.ttf" activity))
    
  (define Basic-Regular
    (load-font "Basic-Regular.otf" activity))
  
  (define Roboto-Medium
    (load-font "Roboto-Medium.ttf" activity))
  
  (define Oswald-Regular ::Typeface
    (load-font "Oswald-Regular.ttf" activity))

  (define GloriaHallelujah ::Typeface
    (load-font "GloriaHallelujah.ttf" activity))

  (define NotoSerif-Regular ::Typeface
    (load-font "NotoSerif-Regular.ttf" activity))

  (define M+1p ::Typeface
    (load-font "MPLUS1p-Medium.ttf" activity))
  
  (define file-icon ::SVG
    (load-svg "file.svg" activity width: 48 height: 48))

  (define directory-icon ::SVG
    (load-svg "directory.svg" activity width: 48 height: 48))

  (define press-mark ::SVG
    (load-svg "press.svg" activity width: 64 height: 64))

  (define release-mark ::SVG
    (load-svg "release.svg" activity width: 64 height: 64))

  (define assets ((activity:getAssets):list ""))

  (define init-script
    (let* ((assets ::AssetManager (activity:getAssets))
	   (input (gnu.kawa.io.InPort
		   (java.io.InputStreamReader
		    (assets:open "init.scm")))))
      (safely (read-all input))))

  (define the-atom-font ::(parameter-of Font)
    (make-parameter
     (Font face: Roboto-Medium
	   size: 36)))

  (define the-string-font ::(parameter-of Font)
    (make-parameter
     (Font face: Iosevka #;HackRegular #;PragmataProMonoRegular
	   size: 32)))

  (define the-comment-font ::(parameter-of Font)
    (make-parameter
     (Font face: GloriaHallelujah
	   size: 28)))

  (define the-caption-font ::(parameter-of Font)
    (make-parameter
     (Font face: M+1p #;Oswald-Regular
	   size: 34)))

  (define the-text-input-font ::(parameter-of Font)
    (make-parameter
     (Font face: NotoSerif-Regular
	   size: 36)))
  
  (define the-block-comment-font ::(parameter-of Font)
    (make-parameter
     (Font face: NotoSerif-Regular
	   size: 28)))

  (define the-block-comment-margin ::(parameter-of real)
    (make-parameter 6))

  (define the-log-font ::(parameter-of Font)
    (make-parameter
     (Font face: Oswald-Regular
	   size: 16)))

  (define the-cursor-offset ::(parameter-of Position)
    (make-parameter (Position left: 0 top: 32)))

  (define the-cursor-extent ::(parameter-of Extent)
    (make-parameter (Extent width: 2 height: 32)))

  (define parenthesis-color ::(parameter-of integer)
    (make-parameter #xffcccccc))

  (define focused-parenthesis-color ::(parameter-of integer)
    (make-parameter #xff555555))

  (define matching-parenthesis-color ::(parameter-of integer)
    (make-parameter #xff888888))

  (define top-left-paren ::Path2D
    (Path
     (moveTo 0 20)
     (quadTo 0 0 20 0)
     (lineTo 20 10)
     (quadTo 10 10 10 20)
     (close)))

  (define top-left-extent ::Extent
    (path-extent top-left-paren))

  (define bottom-left-paren ::Path2D
    (Path
     (moveTo 0 0)
     (quadTo 0 20 20 20)
     (lineTo 20 10)
     (quadTo 10 10 10 0)
     (close)))

  (define bottom-left-extent ::Extent
    (path-extent bottom-left-paren))

  (define top-right-paren ::Path2D
    (Path
     (moveTo 0 0)
     (quadTo 20 0 20 20)
     (lineTo 10 20)
     (quadTo 10 10 0 10)
     (close)))

  (define top-right-extent ::Extent
    (path-extent top-right-paren))

  (define bottom-right-paren ::Path2D
    (Path
     (moveTo 20 0)
     (quadTo 20 20 0 20)
     (lineTo 0 10)
     (quadTo 10 10 10 0)
     (close)))

  (define bottom-right-extent ::Extent
    (path-extent bottom-right-paren))

  ;; quote

  (define top-left-quote-paren ::Path2D
    (Path
     (lineTo 20 0)
     (lineTo 20 10)
     (lineTo 10 10)
     (lineTo 10 20)
     (lineTo 0 20)
     (close)))

  (define top-left-quote-extent ::Extent
    (path-extent top-left-quote-paren))

  (define bottom-left-quote-paren ::Path2D
    (Path
     (lineTo 0 20)
     (lineTo 20 20)
     (lineTo 20 10)
     (lineTo 10 10)
     (lineTo 10 0)
     (close)))

  (define bottom-left-quote-extent ::Extent
    (path-extent bottom-left-quote-paren))

  (define top-right-quote-paren ::Path2D
    (Path
     (lineTo 20 0)
     (lineTo 20 20)
     (lineTo 10 20)
     (lineTo 10 10)
     (lineTo 0 10)
     (close)))

  (define top-right-quote-extent ::Extent
    (path-extent top-right-quote-paren))

  (define bottom-right-quote-paren ::Path2D
    (Path
     (moveTo 20 0)
     (lineTo 20 20)
     (lineTo 0 20)
     (lineTo 0 10)
     (lineTo 10 10)
     (lineTo 10 0)
     (close)))

  (define bottom-right-quote-extent ::Extent
    (path-extent bottom-right-quote-paren))

  (define quote-marker ::Path2D
    (Path
     (lineTo 10 0)
     (lineTo 10 20)
     (close)))

  (define quote-marker-extent ::Extent
    (path-extent quote-marker))

  (define single-quote ::Path2D
    (Path
     (lineTo 10 0)
     (lineTo 10 5)
     (lineTo 0 5)
     (close)

     (moveTo 10 5)
     (lineTo 0 10)
     (lineTo 5 5)
     (close)
     ))

  (define single-quote-extent ::Extent
    (let ((e ::Extent (path-extent single-quote)))
      (set! e:width (+ e:width 2))
      e))

  (define dashed-line ::PathEffect
    (DashPathEffect
     ((array-of float) 10 20) 0))

  (define external-open-file ::(maps (byte Editor)
				     to: (maps _ to: void))
    #!null)
  
  (define external-save-file ::(maps (byte Editor)
				     to: (maps _ to: void))
    #!null)
  
  )

(define (INFO . messages)
  (let ((result ::java.lang.StringBuilder
		(java.lang.StringBuilder)))
    (for message in messages
	 (result:append message))
  (android.util.Log:i "grasp-android" (result:toString))))

(define-early-constant paint ::Paint (Paint))

(define-early-constant transparent ::long #x00000000)

(define-early-constant ruler ::Paint (Paint))

(define (text-width text::CharSequence font::Font)::real
  (ruler:setTypeface font:face)
  (ruler:setTextSize font:size)
  (ruler:measureText text))

(define-object (ScreenLogger size)::MessageHandler

  (define (add-message message::list)::void
    (invoke-special logger (this) 'add-message message)
    (when the-view
      (the-view:invalidate)))

  (define (display-messages output::Object)::void
    (let* ((canvas ::Canvas (as Canvas output))
	   (font ::Font (the-log-font))
	   (screen-extent ::Extent (screen:extent))
	   (top ::float  font:size #;(- screen-extent:height
		(* 4 font:size))))
      (paint:setColor #xff555555)
      (paint:setTypeface font:face)
      (paint:setTextSize font:size)
      (for message in messages
	   (canvas:drawText message 0 top paint)
	   (set! top (+ top font:size)))))

  (logger size))

(define-object (AndroidSystemClipboard clipboard::AndroidClipboard)
  ::Clipboard

  (define own-content ::list '())
  (define own-clip-data ::AndroidClipData #!null)
  
  (define (try-parse item ::AndroidClipData:Item)::Element
    (let ((input (item:getText)))
      (with-input-from-string input
	(lambda ()
	  (let*-values (((expression preceding-space) (read-list 1))
			((following-space) (read-spaces))
			((next) (peek-char)))
	    (if (eof-object? next)
		(or (and-let* ((`(,inside) expression))
		      inside)
		    expression)
		(text input)))))))
  
  (define (upload! new-content ::pair)::void
    (and-let* ((`(,head . ,tail) new-content)
	       (text (show->string head))
	       (clip ::AndroidClipData (AndroidClipData:newPlainText
				   "label" text)))
      (let rewrite ((input tail))
	(and-let* ((`(,head . ,tail) input))
	  (clip:addItem (AndroidClipData:Item
			 (show->string head)))
	  (rewrite tail)))
      (clipboard:setPrimaryClip clip)
      (set! own-clip-data clip)
      (set! own-content new-content)))
  
  (define (content)::list
    (let ((clip ::AndroidClipData (clipboard:getPrimaryClip)))
      (if (eq? clip own-clip-data)
	  (copy own-content)
	  (let ((n ::int (clip:getItemCount)))
	    (if (is n <= 0)
		'()
		(let* ((items ::list (cons (try-parse (clip:getItemAt 0))
					   '()))
		       (end ::list items))
		  (for i from 1 below n
		       (let ((item (try-parse (clip:getItemAt i))))
			 (set! (cdr end) (cons item '()))
			 (set! end (cdr end))))
		  items))))))
  )


(define-object (View source::AndroidActivity
		     sync::android.os.Handler)::Painter

  (define canvas ::Canvas)

  (define icon-size ::Extent #!null)

  (define (icon-extent)::Extent
    (when (and (eq? icon-size #!null)
	       (isnt file-icon eq? #!null)
	       (isnt directory-icon eq? #!null))
      (set! icon-size
	    (Extent width:
		    (+ 12
		       (max
			(directory-icon:getDocumentWidth)
			(file-icon:getDocumentWidth)))
		    height:
		    (max
		     (directory-icon:getDocumentHeight)
		     (file-icon:getDocumentHeight)))))
    icon-size)

  (define (draw-directory-icon!)::void
    (directory-icon:renderToCanvas canvas))

  (define (draw-file-icon!)::void
    (file-icon:renderToCanvas canvas))

  (define press-mark-size ::Extent
    (let ((mark-width ::real (press-mark:getDocumentWidth))
	  (mark-height ::real (press-mark:getDocumentHeight)))
      (Extent width: mark-width
	      height: mark-height)))

  (define (press/release-mark-extent)::Extent
    press-mark-size)
  
  (define (draw-press-mark! left::real top::real)::void
    (let ((mark-width ::real (press-mark:getDocumentWidth))
	  (mark-height ::real (press-mark:getDocumentHeight)))
      (with-translation ((- left (/ mark-width 2))
			 (- top (/ mark-height 2)))
	  (press-mark:renderToCanvas canvas))))

  (define (draw-release-mark! left::real top::real)::void
    (let ((mark-width ::real (release-mark:getDocumentWidth))
	  (mark-height ::real (release-mark:getDocumentHeight)))
      (with-translation ((- left (/ mark-width 2))
			 (- top (/ mark-height 2)))
	  (release-mark:renderToCanvas canvas))))
  
  (define activity ::AndroidActivity source)

  (define (showKeyboard)::void
    (when (requestFocus)
      (let ((imm ::InputMethodManager
		 (as InputMethodManager
		     (activity:getSystemService
		      android.content.Context:INPUT_METHOD_SERVICE))))
	(imm:showSoftInput (this)
			   InputMethodManager:SHOW_IMPLICIT))))

  (define clipLeft ::real 0)
  (define clipTop ::real 0)
  (define clipWidth ::real +inf.0)
  (define clipHeight ::real +inf.0)

  (define (with-clip w::real h::real action::(maps () to: void))::void
    (canvas:save)
    (canvas:clipRect (as float 0) (as float 0)
		     (as float w) (as float h))
    (try-finally
     (action)
     (canvas:restore)))

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (canvas:clipRect (as float left) (as float top)
		     (as float (+ left width))
		     (as float (+ top height)))
    (set! clipLeft left)
    (set! clipTop top)
    (set! clipWidth width)
    (set! clipHeight height))

  (define (current-clip-width)::real
    clipWidth)

  (define (current-clip-height)::real
    clipHeight)

  (define (current-clip-left)::real
    clipLeft)

  (define (current-clip-top)::real
    clipTop)

  (define matrix-points ::(array-of float)
    ((array-of float) length: 9))
  
  (define (translate! x ::real y ::real)::void
    (canvas:translate x y))

  (define rotation ::real 0.0)

  (define (rotate! angle ::real)::void
    (canvas:rotate (java.lang.Math:toDegrees angle))
    (set! rotation (+ rotation angle)))

  (define scale ::real 1.0)

  (define (scale! factor ::real)::void
    (set! scale (* scale factor))
    (canvas:scale factor factor))

  (define (with-stretch horizontal::float vertical::float
			action::(maps () to: void))
    ::void
    (canvas:scale horizontal vertical)
    (try-finally
     (action)
     (canvas:scale (/ horizontal) (/ vertical))))
  
  (define (horizontal-split-height)::real 30)

  (define (vertical-split-width)::real 30)

  (define text-color ::long #xff555555)

  (define background-color ::long transparent)

  (define intensity ::float 1.0)

  (define (with-intensity i::float action::(maps () to: void))::void
    (let ((previous ::float intensity))
      (set! intensity i)
      (try-finally
       (action)
       (set! intensity previous))))
  
  (define (set-color! c::integer)::void
    (let* ((c ::ulong (as ulong c))
	   (RGB ::ulong (bitwise-and c #xffffff))
	   (a ::ulong (bitwise-and #xff
				   (bitwise-arithmetic-shift
				    c -24)))
	   
	   (a* ::ulong (clamp 0 (nearby-int (* intensity a)) 255))
	   (c* ::ulong (bitwise-ior RGB
				    (bitwise-arithmetic-shift
				     a* 24))))
      (paint:setColor c*)))
  
  (define (draw-horizontal-split! top::real)::void
    (let* ((left ::float (max 0 (current-clip-left)))
	   (bottom ::float (+ top (horizontal-split-height)))
	   (right ::float (min screen:size:width
			       (+ left (current-clip-width)))))
      (paint:setColor text-color)
      (canvas:drawRect left (as float top) right bottom
		       paint)))

  (define (draw-vertical-split! left::real)::void
    (let* ((top ::float (max 0 (current-clip-top)))
	   (right ::float (+ left (vertical-split-width)))
	   (bottom ::float (min screen:size:height
				(+ top (current-clip-height)))))
      (paint:setColor text-color)
      (canvas:drawRect (as float left) top right bottom
		       paint)))

  (define (grid-border)::real 20)

  (define (draw-horizontal-grid! width::real)::void
    (set-color! text-color)
    (canvas:drawRect 8 8 (- width 8) 12 paint))

  (define (draw-vertical-grid! height::real)::void
    (set-color! text-color)
    (canvas:drawRect 8 8 12 (- height 8) paint))

  (define (fill-grid-cell! width::real height::real)::void
    (set-color! Color:WHITE)
    (canvas:drawRect 10 10 (- width 10) (- height 10) paint))

  (define (draw-thick-line! x0::real y0::real x1::real y1::real)
    ::void
    (set-color! Color:LTGRAY)
    (paint:setStrokeWidth 4)
    (canvas:drawLine x0 y0 x1 y1 paint)
    (paint:setStrokeWidth 1))

  (define (draw-thin-line! x0::real y0::real x1::real y1::real)
    ::void
    (set-color! Color:LTGRAY)
    (paint:setStrokeWidth 1)
    (canvas:drawLine x0 y0 x1 y1 paint))

  (define (draw-circle! x0::real y0::real r::real c::uint)::void
    ::void
    (set-color! c)
    (paint:setStyle Paint:Style:STROKE)
    (paint:setStrokeWidth 4)
    (canvas:drawCircle x0 y0 r paint)
    (paint:setStyle Paint:Style:FILL))

  (define (fill-circle! x0::real y0::real r::real c::uint)::void
    ::void
    (set-color! c)
    (canvas:drawCircle x0 y0 r paint))
  
  (define (draw-stroke! x0::real y0::real x1::real y1::real)
    ::void
    (draw-thick-line! x0 y0 x1 y1))

  (define (mark-editor-cursor! +left::real +top::real
			       editor::WithCursor)
    ::void
    (let* ((cursor-extent ::Extent (the-cursor-extent))
	   (cursor-offset ::Position (the-cursor-offset))
	   (left ::real (+ +left cursor-offset:left))
	   (top ::real (+ +top cursor-offset:top))
	   (traversal ::Traversal (the-traversal)))
      (editor:mark-cursor! (+ traversal:parent-left +left)
			   (+ traversal:parent-top +top))
      (set-color! text-color)
      (canvas:drawRect left top
		       (+ left cursor-extent:width)
		       (+ top cursor-extent:height)
		       paint)))

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

  (define highlight-count::(array-of byte)
    ((array-of byte) length: (length (HighlightType:values))))

  (define (set-highlight-color!)
    (cond
     ((is (highlight-count (HighlightType:CurrentFinding:ordinal)) > 0)
      (set! text-color #xffffffff)
      (set! background-color #xffff7f27))
     ((is (highlight-count (HighlightType:OtherFinding:ordinal)) > 0)
      (set! text-color #xffffffff)
      (set! background-color #xfffff200))
     ((is (highlight-count (HighlightType:Selection:ordinal)) > 0)
      (set! text-color #xffffffff)
      (set! background-color #xff555555))
     (else
      (set! text-color #xff555555)
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
    (set! text-color #xffdddddd)
    (set! (parenthesis-color) #xffeeeeee)
    (set! atom-frame-color #xffeeeeee)
    (set! current-comment-level (+ current-comment-level 1)))

  (define (exit-comment-drawing-mode!)::void
    (set! current-comment-level (- current-comment-level 1))
    (when (is current-comment-level <= 0)
      (set! text-color #xff555555)
      (set! atom-frame-color #xffdddddd)
      (set! (parenthesis-color) #xffcccccc)))

  (define (in-comment-drawing-mode?)::boolean
    (is current-comment-level > 0))

  (define (clear!)::void
    (canvas:drawRGB 127 127 127))

  (define (request-redraw!)::void
    (invalidate))
  
  (define (vertical-bar-width)::real 10)

  (define (horizontal-bar-height)::real 10)

  (define (draw-horizontal-bar! width::real
				highlighted?::boolean)
    ::void
    (if highlighted?
	(set-color! (focused-parenthesis-color))
	(set-color! (parenthesis-color)))
    (canvas:drawRect 0 0 width (horizontal-bar-height) paint))

  (define (draw-vertical-bar! height::real
			      highlighted?::boolean)
    ::void
    (if highlighted?
	(set-color! (focused-parenthesis-color))
	(set-color! (parenthesis-color)))
    (canvas:drawRect 0 0 (vertical-bar-width) height paint))

  (define (space-width)::real 16)

  (define (line-simplification-resolution)::real 20)
  
  (define (draw-rounded-rectangle! width::real height::real)
    ::void
    (set-color! #xffffffff)
    (canvas:drawRoundRect 0 0 (as int width) (as int height)
			  10 10 paint)
    (set-color! text-color)
    (paint:setStyle Paint:Style:STROKE)
    (canvas:drawRoundRect 0 0 (as int width) (as int height)
			  10 10 paint)
    (paint:setStyle Paint:Style:FILL))

  (define (fill-background! width::real height::real)::void
    (set-color! #xffffffff)
    (canvas:drawRect 0 0 (as int width) (as int height)
		     paint))
  
  (define (draw-popup! width::real height::real)::void
    (paint:setColor (- text-color
		       #x77000000))
    (canvas:drawRoundRect 0 0 (as int width) (as int height)
			  25 25 paint))

  (define (horizontal-popup-margin)::real 4)
  (define (vertical-popup-margin)::real 40)

  (define (draw-rectangle! width::real height::real)::void
    (let ((b ::int 2))
      (set-color! text-color)
      (canvas:drawRect 0 0 (as int width) (as int b) paint)
      (canvas:drawRect 0 (as int (- height b))
		       (as int width) (as int height) paint)
      (canvas:drawRect 0 b b (as int (- height b)) paint)
      (canvas:drawRect (as int (- width b)) b
		       (as int width) (as int (- height b))
		       paint)
      ))


  (define (draw-border! width::real height::real)::void
    (set-color! text-color)
    (canvas:drawRect 6 6 (- width 6) 16 paint)
    (canvas:drawRect 6 6 16 (- height 6) paint)
    (canvas:drawRect (- width 14) 6 (- width 6) (- height 6) paint)
    (canvas:drawRect 6 (- height 14) (- width 6) (- height 6) paint))

  (define (border-size)::real 20)

  (define (height/width-ratio)::real 1)
  
  (define (min-line-height)::real
    (let ((font ::Font (the-atom-font)))
      font:size))

  (define (min-box-height)::real
    (let ((font ::Font (the-atom-font)))
      (max font:size
	   (+ top-left-extent:height
	      bottom-left-extent:height)
	   (+ top-right-extent:height
	      bottom-right-extent:height))))

  (define (draw-custom-box!
	   draw-left-paren!::(maps (real) to: void)
	   draw-right-paren!::(maps (real) to: void)
	   paren-width::real
	   width::real height::real context::Cursor)
    ::void
    (let ((left-color ::long (parenthesis-color))
	  (right-color ::long (parenthesis-color))
	  (t ::Traversal (the-traversal)))
      (match (the-cursor)
	(`(#\[ . ,,context) 
	 (set! left-color (focused-parenthesis-color))
	 (set! right-color (matching-parenthesis-color))
	 (let ((left t:left)
	       (top t:top))
	   (set! t:parent-left (+ t:parent-left left))
	   (set! t:parent-top (+ t:parent-top top))
	   (set! t:left 0)
	   (set! t:top 0)
	   (mark-cursor! (quotient paren-width 2)
			 0)
	   (set! t:left left)
	   (set! t:top top)
	   (set! t:parent-top (- t:parent-top top))
	   (set! t:parent-left (- t:parent-left left))))
	(`(#\] . ,,context)
	 (set! left-color (matching-parenthesis-color))
	 (set! right-color (focused-parenthesis-color))
	 (let ((left t:left)
	       (top t:top))
	   (set! t:parent-left (+ t:parent-left t:left))
	   (set! t:parent-top (+ t:parent-top t:top))
	   (set! t:left 0 #;(+ paren-width width))
	   (set! t:top 0)
	   (mark-cursor! (- width (quotient paren-width 2)) 0)
	   (set! t:left left)
	   (set! t:top top)
	   (set! t:parent-top (- t:parent-top t:top))
	   (set! t:parent-left (- t:parent-left t:left))))
	(_
	 (values)))
      (set-color! left-color)
      (draw-left-paren! height)
      (with-translation ((- width paren-width) 0)
	(set-color! right-color)
	(draw-right-paren! height))))
  
  (define (open-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-left-extent:height
				 bottom-left-extent:height))))
      (canvas:drawPath top-left-paren paint)
      (canvas:drawRect 0 top-left-extent:height
		       10 (+ top-left-extent:height
			     line-height)
		       paint)
      (with-translation (0 (+ top-left-extent:height
			      line-height))
	  (canvas:drawPath bottom-left-paren paint))))

  (define (close-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-extent:height
				 bottom-right-extent:height))))
      (canvas:drawPath top-right-paren paint)
      (canvas:drawRect (- top-right-extent:width 10)
		       top-right-extent:height
		       top-right-extent:width
		       (+ top-right-extent:height line-height)
		       paint)

      (with-translation (0 (+ top-right-extent:height
			      line-height))
	  (canvas:drawPath bottom-right-paren paint))))

  (define (paren-width)::real
    (+ 1 top-left-extent:width))

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
				 top-left-extent:height
				 bottom-left-extent:height))))
      (canvas:drawPath top-left-quote-paren paint)
      (canvas:drawRect 0 top-left-quote-extent:height
		       10 (+ top-left-quote-extent:height
			     line-height)
		       paint)
      (with-translation (0 (+ top-left-quote-extent:height
			      line-height))
	  (canvas:drawPath bottom-left-quote-paren paint))))

  (define (close-quote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 top-right-extent:height
				 bottom-right-extent:height))))
      (canvas:drawPath top-right-quote-paren paint)
      (canvas:drawRect (- top-right-quote-extent:width 10)
		       top-right-quote-extent:height
		       top-right-quote-extent:width
		       (+ top-right-quote-extent:height line-height)
		       paint)
      (with-translation (0 (+ top-right-quote-extent:height
			      line-height))
	  (canvas:drawPath bottom-right-quote-paren paint))))

    (define (quote-paren-width)::real
      (+ 1 top-left-quote-extent:width))
    
    (define (draw-quote-box! width::real
			     height::real
			     context::Cursor)
      ::void
      (draw-custom-box!
       (lambda (width::real)::void (open-quote-paren! height))
       (lambda (width::real)::void (close-quote-paren! height))
       (quote-paren-width)
       width height context))

  (define (open-quasiquote-paren! height::real)::void
    (let ((line-height (max 0 (- height top-left-extent:height))))
      (canvas:drawPath top-left-quote-paren paint)
      (canvas:drawRect 0 top-left-quote-extent:height
		       10 (+ top-left-quote-extent:height
			     line-height)
		       paint)))

  (define (close-quasiquote-paren! height::real)::void
    (let ((line-height (max 0 (- height top-right-extent:height))))
      (canvas:drawPath top-right-quote-paren paint)
      (canvas:drawRect (- top-right-quote-extent:width 10)
		       top-right-quote-extent:height
		       top-right-quote-extent:width
		       (+ top-right-quote-extent:height line-height)
		       paint)))

  (define (quasiquote-paren-width)::real
    (+ 1 top-left-quote-extent:width))

  (define (draw-quasiquote-box! width::real
				height::real
				context::Cursor)
    ::void
    (draw-custom-box!
     (lambda (width::real)::void (open-quasiquote-paren! height))
     (lambda (width::real)::void (close-quasiquote-paren! height))
     (quasiquote-paren-width)
     width height context))

  (define (open-unquote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 bottom-left-quote-extent:height))))
      (canvas:drawRect 0 0 10 line-height paint)
      (with-translation (0 line-height)
	(canvas:drawPath bottom-left-quote-paren paint))
      ))

  (define (close-unquote-paren! height::real)::void
    (let ((line-height (max 0 (- height
				 bottom-right-quote-extent:height))))
      (canvas:drawRect (- bottom-right-quote-extent:width 10) 0
		       bottom-right-quote-extent:width line-height
		       paint)
      (with-translation (0 line-height)
	(canvas:drawPath bottom-right-quote-paren paint))))

  (define (unquote-paren-width)::real
    (+ 1 bottom-left-quote-extent:width))

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
    (canvas:drawRect 0 10 2.5 20 paint)
    (canvas:drawRect 5 10 10 20 paint)
    (with-translation (10 0)
      (open-unquote-paren! height)))

  (define (close-unquote-splicing-paren! height::real)
    ::void
    (close-unquote-paren! height)
    (canvas:drawRect 20 10 25 20 paint)
    (canvas:drawRect 27.5 10 30 20 paint))

  (define (unquote-splicing-paren-width)::real
    (+ (unquote-paren-width) 10))

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
    (canvas:drawPath quote-marker paint))

  (define (quote-marker-width)::real
    (+ 1 quote-marker-extent:width))
  
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
    (canvas:drawPath top-left-quote-paren paint))

  (define (close-quasiquote-marker! height::real)
    ::void
    (canvas:drawPath top-right-quote-paren paint))

  (define (quasiquote-marker-width)::real
    (+ 1 top-left-quote-extent:width))

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
    (with-translation (0 (- height bottom-left-quote-extent:height))
      (canvas:drawPath bottom-left-quote-paren paint)))

  (define (close-unquote-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-right-quote-extent:height))
      (canvas:drawPath bottom-right-quote-paren paint)))

  (define (unquote-marker-width)::real
    (+ 1 bottom-left-quote-extent:width))
  
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
    (with-translation (0 (- height bottom-left-quote-extent:height))
      (canvas:drawRect 0 10 2.5 20 paint)
      (canvas:drawRect 5 10 10 20
		       paint)
      (with-translation (10 0)
	(canvas:drawPath bottom-left-quote-paren paint))))

  (define (close-unquote-splicing-marker! height::real)
    ::void
    (with-translation (0 (- height bottom-right-quote-extent:height))
      (canvas:drawPath bottom-right-quote-paren paint)
      (canvas:drawRect 20 10 25 20 paint)
      (canvas:drawRect 27.5 10 30 20 paint)))

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

  (define (measure-text-index-position-into!
	   target::Position text::CharSequence index::int
	   font::Font)
    ::Position
    (let ((segment-start 0)
	  (string-end (text:length)))
      (escape-with break
	(for i from 0 below string-end
	     (when (eq? (text:charAt i) #\newline)
	       (set! target:top (+ target:top font:size))
	       (set! segment-start i))
	     (when (= i index)
	       (let ((line (text:subSequence segment-start i)))
		 (set! target:left (+ target:left
				      (text-width line font))))
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
	   (parent ::Traversal (the-traversal))
	   (height ::float font:size)
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
		 (width (text-width fragment font)))
	    (set-color! background-color)
	    (canvas:drawRect traversal:left traversal:top
			     (+ traversal:left width)
			     (+ traversal:top height)
			     paint)
	    (set-color! text-color)
	    (canvas:drawText fragment traversal:left
			     (+ traversal:top height)
			     paint)
	    (traversal:expand-by! width)))

	(paint:setTypeface font:face)
	(paint:setTextSize font:size)
	(for i from 0 below string-end
	     (when (and focused? (eqv? (head (the-cursor))
				       i))
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

  (define (draw-string! text::CharSequence context::Cursor)
    ::void
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

  (define (measure-quoted-text-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (set! target:left (+ target:left
			 (* 2 single-quote-extent:width)))
    (set! target:top (+ target:top
			single-quote-extent:height))
    (measure-string-index-position-into! target text index))
  
  (define (draw-quoted-text! text::CharSequence
			     context::Cursor)
    ::void
    (parameterize ((the-cursor-offset
		    quoted-text-cursor-offset))
      (let* ((w ::real single-quote-extent:width)
	     (h ::real single-quote-extent:height)
	     (e ::Extent (text-extent text
				      (the-string-font)))
	     (2w ::real (* 2 w))
	     (h/2 ::real (quotient h 2))
	     (r ::real (min 2w h))
	     (r/2 ::real (quotient r 2)))

	(paint:setPathEffect dashed-line)
	(canvas:drawLine 2w h/2 (+ 2w e:width) h/2 paint)
	(canvas:drawLine w h w (+ h e:height) paint)
	(canvas:drawLine 2w (+ h e:height h/2)
			 (+ 2w e:width) (+ h e:height h/2) paint)
	(canvas:drawLine (+ 2w e:width w) h
			 (+ 2w e:width w) (+ h e:height) paint)
	(paint:setPathEffect #!null)
	
	(canvas:drawPath single-quote paint)
	(with-translation (w 0)
	  (canvas:drawPath single-quote paint)
	  (with-translation (w h/2)
	    (let ((t ::Traversal (the-traversal)))
	      (set! t:left (+ t:left 2w))
	      (set! t:top (+ t:top h))
	      (draw-string! text context)
	      (set! t:left (- t:left 2w))
	      (set! t:top (- t:top h)))
	    (let (
		  )
	      (with-translation ((- w) (+ e:height h/2))
		(canvas:drawCircle 0 0 (/ r 2) paint))
	      (with-translation ((+ e:width w) (- h/2))
		(canvas:drawCircle 0 0 (/ r 2) paint))
	      (with-translation (e:width e:height)
		(canvas:drawPath single-quote paint)
		(with-translation (single-quote-extent:width 0)
		  (canvas:drawPath single-quote paint)))))))))

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


  (define (text-extent text::CharSequence font::Font)::Extent
    (let* ((line-start 0)
	   (lines ::int 1)
	   (line-height ::float font:size)
	   (max-width 0)
	   (string-end (text:length)))
      (for i from 0 below string-end
	   (when (eq? (text:charAt i) #\newline)
	     (set! max-width
		   (max max-width
			(text-width (text:subSequence
				     line-start i)
				    font)))
	     (set! lines (+ lines 1))
	     (set! line-start (+ i 1))))
      (set! max-width
	(max max-width
		 (text-width
		  (text:subSequence line-start string-end)
		  font)))
      (Extent width: max-width
	      height: (* lines line-height))))

  (define (draw-caption! caption::CharSequence)::void
    (draw-text! caption (the-caption-font) #!null))

  (define (caption-extent caption::CharSequence)::Extent
    (text-extent caption (the-caption-font)))

  (define (caption-margin-top)::real 4)

  (define (caption-margin-bottom)::real 14)
  
  (define (caption-horizontal-margin)::real
    (space-width))

  (define (draw-text-input! text::CharSequence
			    context::Cursor)
    ::void
    (draw-text! text (the-text-input-font) context))

  (define (measure-text-input-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (measure-text-index-position-into! target
				       text
				       index
				       (the-text-input-font)))
  
  (define (text-input-extent text::CharSequence)::Extent
    (let ((extent ::Extent (text-extent
			    text (the-text-input-font))))
      (set! extent:height (* 1.2 extent:height))
      extent))

  (define (text-input-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (text-character-index-under x y text (the-text-input-font)))
  
  (define (atom-extent text::CharSequence)::Extent
    (let ((inner ::Extent (text-extent text (the-atom-font))))
      (Extent width: (+ inner:width 8)
	      height: (max (min-box-height)
			   (+ inner:height 16)))))

  (define atom-cursor-offset::Position (Position left: 0
						 top: 4))

  (define atom-frame-color ::long #xffdddddd)

  (define (draw-atom! text::CharSequence context::Cursor)::void
    (let* ((font (the-atom-font))
	   (extent ::Extent (text-extent text font)))
      (set-color! atom-frame-color)
      (canvas:drawRoundRect (as int 2) (as int 12)
			    (as int (+ extent:width 10))
			    (as int (+ extent:height 6))
			    12 12 paint)
      (with-translation (6 4)
	(parameterize ((the-cursor-offset
			atom-cursor-offset))
	  (draw-text! text font context)))))

  (define (measure-atom-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (set! target:left (+ target:left 6))
    (set! target:top (+ target:top 4))
    (measure-text-index-position-into! target
				       text
				       index
				       (the-atom-font)))
  
  (define (text-character-index-under x::real y::real
				      text::CharSequence
				      font::Font)
    ::int
    (let* ((line-height font:size)
	   (string-end (text:length)))
      (let loop ((i ::int 0)
		 (left ::int 0)
		 (top ::int 0))
	(if (is i >= string-end)
	    (max 0 (- i 1))
	    (let ((c (text:charAt i)))
	      (match c
		(#\newline
		 (if (is top <= y < (+ top line-height))
		     i
		     (loop (+ i 1) 0 (+ top line-height))))
		(_
		 (let ((width (text-width (text:subSequence
					   i (+ i 1))
					  font)))
		   (if (and (is top <= y < (+ top line-height))
			    (is left <= x < (+ left width)))
		       i
		       (loop (+ i 1) (+ left width)
			     top))))))))))

  (define (atom-character-index-under x::real y::real
				      text::CharSequence)
    ::int
    (text-character-index-under x y text (the-atom-font)))

  (define (draw-line-comment! text::CharSequence
			      context::Cursor)
    ::void
    (draw-text! text (the-comment-font) context))

  (define (measure-line-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (measure-text-index-position-into! target
				       text
				       index
				       (the-comment-font)))
  
  (define (line-comment-extent text::CharSequence)
    ::Extent
    (text-extent text (the-comment-font)))

  (define (line-comment-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (text-character-index-under x y text (the-comment-font)))

  (define (draw-block-comment! text::CharSequence
			       context::Cursor)
    ::void
    (let* ((font ::Font (the-block-comment-font))
	   (outer ::Extent (block-comment-extent text))
	   (margin ::real (the-block-comment-margin)))
      (draw-rectangle! outer:width outer:height)
      (with-translation (margin (* 0.4 font:size))
	(draw-text! text font context))))

  (define (measure-block-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (let ((font ::Font (the-block-comment-font)))
      (set! target:left (+ target:left
			   (the-block-comment-margin)))
      (set! target:top (+ target:top (* 0.4 font:size)))
      (measure-text-index-position-into! target text index
					 font)))

  (define (block-comment-extent text::CharSequence)::Extent
    (let* ((font ::Font (the-block-comment-font))
	   (inner ::Extent (text-extent text font))
	   (margin ::real (the-block-comment-margin)))
      (Extent width: (+ inner:width margin margin)
	      height: (+ inner:height font:size))))

  (define (block-comment-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (let* ((font ::Font (the-block-comment-font))
	   (margin ::real (the-block-comment-margin)))
      (text-character-index-under (- x margin)
				  (- y (* 0.4 font:size))
				  text font)))

  (define (draw-point! left::real top::real aRGB::int)::void
    (let* ((alpha ::int
		  (- 255
		     (bitwise-and
		      #xff
		      (bitwise-arithmetic-shift aRGB -24))))
	   (red ::int (bitwise-and
		       #xff
		       (bitwise-arithmetic-shift aRGB -16)))
	   (green ::int (bitwise-and
			 #xff
			 (bitwise-arithmetic-shift aRGB -8)))
	   (blue ::int (bitwise-and #xff aRGB)))
      (paint:setColor (as int
			  (bitwise-ior
			   (bitwise-arithmetic-shift alpha 24)
			   (bitwise-arithmetic-shift red 16)
			   (bitwise-arithmetic-shift green 8)
			   blue)))
      (canvas:drawCircle left top 10.0 paint)))

  (define (onDraw c::Canvas)::void
    (set! canvas c)
    (clear!)
    (safely
     (screen:render!))
    (invoke (current-message-handler)
	    'display-messages canvas))

  (define pending-animations
    ::java.util.Collection
    (java.util.concurrent.ConcurrentLinkedQueue))

  (define last-animation-event-time-ms ::long 0)
  
  (define (animate!)::void 
    (unless (pending-animations:isEmpty)
      (let* ((now ::long (current-time-ms))
	     (delta-ms ::long (- now
				 last-animation-event-time-ms)))
	(for animation::Animation in pending-animations
	  (unless (animation:advance! delta-ms)
	    (pending-animations:remove animation)))
	(set! last-animation-event-time-ms now)
	(invalidate)
	(unless (pending-animations:isEmpty)
	  (sync:postDelayed (lambda () (animate!)) 40)))))
  
  (define (play! animation::Animation)::void
    (unless (any (is _ eq? animation) pending-animations)
      (let ((was-empty? ::boolean (pending-animations:isEmpty)))
	(pending-animations:add animation)
	(when was-empty?
	  (set! last-animation-event-time-ms (current-time-ms))
	  (sync:postDelayed (lambda () (animate!)) 40)))))

  (define window-position ::(array-of int)
    ((array-of int) length: 2))
  
  (AndroidView source)
  (setFocusable #t)
  (invoke-special AndroidView (this)
		  'getLocationInWindow window-position)
  ;;(WARN window-position)
  (setFocusableInTouchMode #t)
  ;;(setClickable #t)
  (paint:setFlags Paint:ANTI_ALIAS_FLAG))

(define-object (GRASP)::Keeper
  
  (define reaction-to-request-response
    (attribute (request-code::int)::(maps (Object) to: void)
      nothing))

  (define last-request-code ::int 0)

  (define (new-request-code)::int
    (set! last-request-code (+ last-request-code 1))
    last-request-code)

  (define (onConfigurationChanged config::AndroidConfiguration)::void
    (invoke-special AndroidActivity (this)
		    'onConfigurationChanged config)

    (let* ((resources ::AndroidResources (getResources))
	   (metrics ::DisplayMetrics
		    (resources:getDisplayMetrics)))
      (screen:set-size! metrics:widthPixels metrics:heightPixels))

    #;(match config:orientation
      (,AndroidConfiguration:ORIENTATION_LANDSCAPE
       (WARN 'landscape (screen:extent)))
      (,AndroidConfiguration:ORIENTATION_PORTRAIT
       (WARN 'portrait (screen:extent)))))

  (define (onActivityResult requestCode::int
			    resultCode::int
			    data::Intent)
    ::void
    (safely
     ((reaction-to-request-response requestCode) resultCode data)
     (unset! (reaction-to-request-response requestCode))))
  
  (define (onRequestPermissionsResult requestCode::int
                                      permissions::(array-of
						    String)
                                      grantResults::(array-of
						     int))
    ::void
    (invoke-special AndroidActivity (this)
		    'onRequestPermissionsResult requestCode
                                      permissions
                                      grantResults)
    (safely
     (cond
      ((and (is (length grantResults) > 0)
	    (eq? (grantResults 0)
		 PackageManager:PERMISSION_GRANTED))
       ((reaction-to-request-response requestCode) grantResults)
       (unset! (reaction-to-request-response requestCode)))
      (else
       (WARN "permission request "requestCode" for "
	     permissions" has been denied: "grantResults)
       (unset! (reaction-to-request-response requestCode))))))

  (define (with-permissions permissions::(array-of String)
			    action::procedure)
    (safely
     (if (any (is (checkSelfPermission _)
		  eq? PackageManager:PERMISSION_DENIED)
	      permissions)
	 (let ((request-code ::int (new-request-code)))
	   (set! (reaction-to-request-response
		  request-code) action)
	   (requestPermissions permissions request-code))
	 (action '(PackageManager:PERMISSION_GRANTED)))))

  (define (with-permission permission::String action::procedure)
    (with-permissions ((array-of String) permission)
      action))

  (define (with-read-permission action::procedure)
    (with-permission Manifest:permission:WRITE_EXTERNAL_STORAGE
      action))

  (define (with-write-permission action::procedure)
    (with-permission Manifest:permission:READ_EXTERNAL_STORAGE
      action))

  (define (with-intent intent::Intent action::procedure)
    (let ((request (new-request-code)))
      (set! (reaction-to-request-response request) action)
      (runOnUiThread
       (lambda ()
	 (startActivityForResult intent request)))))
  
  (define (initial-directory)::java.io.File
    (android.os.Environment:getExternalStorageDirectory))

  (define (file-system-roots)::(list-of java.io.File)
    `(,(android.os.Environment:getExternalStorageDirectory)
      ,(invoke-special android.content.Context
		       (this) 'getFilesDir)
      ,(invoke-special android.content.Context
		       (this) 'getExternalFilesDir #!null)))
  
  (define view :: View)

  (define process-finger ::(array-of TouchEventProcessor)
    ((array-of TouchEventProcessor) length: 10))

  (define (invalidating result::boolean)::boolean
    (when result
      (view:invalidate))
    result)

  (define (onTouchEvent event::MotionEvent)::boolean
    (define (pointer-down? event-action::int)::boolean
      (or (eq? event-action MotionEvent:ACTION_DOWN)
	  (eq? event-action MotionEvent:ACTION_POINTER_DOWN)))

    (define (pointer-up? event-action::int)::boolean
      (or (eq? event-action MotionEvent:ACTION_UP)
	  (eq? event-action MotionEvent:ACTION_POINTER_UP)
	  (eq? event-action MotionEvent:ACTION_OUTSIDE)
	  (eq? event-action MotionEvent:ACTION_CANCEL)))
    (safely
     (invalidating
      (let ((parent ::AndroidView (view:getParent)))
      (match (event:getActionMasked)
	(,@pointer-down?
	 (let* ((i ::int (event:getActionIndex))
		(p ::int (event:getPointerId i))
		(finger ::TouchEventProcessor
			(process-finger p)))
	   (finger:press! (- (event:getX i) (parent:getLeft))
			  (- (event:getY i) (parent:getTop))
			  (event:getEventTime))))
	(,@pointer-up?
	 (let* ((i ::int (event:getActionIndex))
		(p ::int (event:getPointerId i))
		(finger ::TouchEventProcessor
			(process-finger p)))
	   (finger:release! (- (event:getX i) (parent:getLeft))
			    (- (event:getY i) (parent:getTop))
			    (event:getEventTime))))
	(,MotionEvent:ACTION_MOVE
	 (let ((n ::int (event:getPointerCount))
	       (result ::boolean #f))

	   (for i from 0 below n
		(let* ((p ::int (event:getPointerId i))
		       (finger ::TouchEventProcessor
			       (process-finger p)))
		  (set! result
			(or (finger:move!
			     (- (event:getX i) (parent:getLeft))
			     (- (event:getY i) (parent:getTop))
			     (event:getEventTime))
			    result))))
	   result))
	(_
	 #f)))
      )))

  (define (onKeyUp keyCode::int event::KeyEvent)::boolean
    #f)

  (define (onKeyDown keyCode::int event::KeyEvent)::boolean
    (safely
     (parameterize ((unicode-input (integer->char
				    (event:getUnicodeChar))))
       (invalidating
	(screen:key-typed!
	 (as long
	     (bitwise-ior
	      (as long keyCode)
	      (if (event:ctrl-pressed?) CTRL_MASK 0)
	      (if (event:alt-pressed?) ALT_MASK 0)
	      (if (event:shift-pressed?) SHIFT_MASK 0)
	      ))
	 '())))))

  (define sync::android.os.Handler (android.os.Handler))

  (define env ::gnu.mapping.Environment #!null)

  (define scheme ::gnu.expr.Language #!null)

  (define save-state ::procedure (lambda () (values)))
  
  (define (onPause)::void
    (invoke-special AndroidActivity (this) 'onPause)
    (save-state))
  
  (define (onCreate savedState::Bundle)::void
    (invoke-special AndroidActivity (this) 'onCreate
		    savedState)

    (set! (default-transform) (lambda () (Isogonal)))
    (set! (current-message-handler) (ScreenLogger 100))
    
    (set! scheme (or kawa.standard.Scheme:instance
		     (kawa.standard.Scheme)))
    (kawa.standard.Scheme:registerEnvironment)
    (set! env (scheme:getEnvironment))
    (kawa.standard.Scheme:loadClass "kawa.lib.kawa.base" env)    
    (gnu.mapping.Environment:setCurrent env)
    
    (let* ((window ::AndroidWindow (invoke-special
				    AndroidActivity
				    (this) 'getWindow))
	   (decor ::AndroidView (window:getDecorView))
	   (flags ::int
		  (bitwise-ior
		   #;AndroidView:SYSTEM_UI_FLAG_HIDE_NAVIGATION
		   AndroidView:SYSTEM_UI_FLAG_FULLSCREEN
		   #;AndroidView:SYSTEM_UI_FLAG_IMMERSIVE
		   )))
      (window:setSoftInputMode
       (logior
	WindowManager:LayoutParams:SOFT_INPUT_STATE_VISIBLE
	WindowManager:LayoutParams:SOFT_INPUT_ADJUST_RESIZE))
      (decor:setSystemUiVisibility flags))
    
    (initialize-activity (this))

    (safely
    (set! (file-display-name
	   (invoke-static
	    android.os.Environment
	    'getExternalStorageDirectory))
	  "[Shared Storage]")

    (set! (file-display-name
	   (invoke-special
	    android.content.Context
	    (this) 'getFilesDir))
	  "[Private Storage]")

    (set! (file-display-name
	   (invoke-special
	    android.content.Context
	    (this) 'getExternalFilesDir
	    #!null))
	  "[Private/External]"))
    
    (safely (initialize-keymap))
    (set! (the-keeper) (this))
    (set! (the-system-clipboard)
	  (AndroidSystemClipboard
	   (invoke-special
	    AndroidActivity (this)
	    'getSystemService
	    android.content.Context:CLIPBOARD_SERVICE)))
    (set! external-open-file
	  (lambda (finger::byte editor::DocumentEditor)
	    (lambda _
	      (with-intent (Intent
			    Intent:ACTION_OPEN_DOCUMENT
			    category: Intent:CATEGORY_OPENABLE
			    type: "*/*")
		(lambda (resultCode intent)
		  (safely
		   (when (eq? resultCode
			      AndroidActivity:RESULT_OK)
		     (let* ((uri ::Uri (intent:getData))
			    (r ::ContentResolver
			       (invoke-special
				android.content.Context
				(this) 'getContentResolver))
			    (c ::DbCursor
			       (r:query
				uri ((array-of String)
				     DbColumn:DISPLAY_NAME
				     DbColumn:SIZE)
				#!null #!null #!null))
			    (s ::java.io.InputStream
			       (r:openInputStream uri))
			    (p ::gnu.kawa.io.InPort
			       (gnu.kawa.io.InPort
				(java.io.InputStreamReader s))))
		       (try-finally
			(and (c:moveToNext)
			     (let ((name (c:getString 0)))
			       (screen:overlay:clear!)
			       (editor:load-from-port
				p (Named thing: uri
					 name: name))))
			(c:close))))))))))
    #;(set! (open-file) external-open-file)
    (set! external-save-file
	  (lambda (finger::byte editor::DocumentEditor)
	    (lambda _
	      (with-intent (Intent
			    Intent:ACTION_CREATE_DOCUMENT
			    category: Intent:CATEGORY_OPENABLE
			    type: "text/scm")
		(lambda (resultCode intent)
		  (safely
		   (when (eq? resultCode
			      AndroidActivity:RESULT_OK)
		     (let* ((uri ::Uri (intent:getData))
			    (r ::ContentResolver
			       (invoke-special
				android.content.Context
				(this) 'getContentResolver))
			    (s ::java.io.OutputStream
			       (r:openOutputStream uri))
			    (p ::gnu.kawa.io.OutPort
			       (gnu.kawa.io.OutPort
				(java.io.OutputStreamWriter s))))
		       (parameterize ((current-output-port p))
			 (show-document editor:document)
			 (flush-output-port))
		       (p:close)
		       (s:flush)
		       (s:close)))))))))
    ;;(set! (save-file) external-save-file)
        
    (set! view (View (this) sync))
    (set! the-view view)

    (letrec* ((random ::java.util.Random
		      (java.util.Random))
	      (signal ::($bracket-apply$
			 java.util.concurrent.ConcurrentMap
			 String BlockingQueue)
		      (java.util.concurrent.ConcurrentHashMap))
	      (new-utterance-id (lambda ()
				  ::String
				  (string-append
				   "GRASP"
				   (number->string
				    (random:nextLong)
				    36))))
	      (mouth ::Mouth
		     (Mouth
		      (this)
		      (lambda (status)
			(mouth:setOnUtteranceProgressListener
			 (object (android.speech.tts.UtteranceProgressListener)
			   ((onDone utteranceId::String)::void
			    (and-let* ((queue ::BlockingQueue
					      (signal:get utteranceId)))
			      (signal:remove utteranceId)
			      (queue:put #t)))

			   ((onError utteranceId::String)::void
			    (and-let* ((queue ::BlockingQueue
					      (signal:get utteranceId)))
			      (signal:remove utteranceId)
			      (queue:put #f)))
			   
			   ((onStart utteranceId::String)::void
			    (values))
			   ))))))
      (define (say . words)::void
	(let ((text ::String (apply string-append words))
	      (id ::string (new-utterance-id))
	      (bundle ::Bundle (Bundle))
	      (queue ::BlockingQueue
		     (ArrayBlockingQueue 1)))
	  (bundle:putString
	   Mouth:Engine:KEY_PARAM_UTTERANCE_ID id)
	  (signal:put id queue)
	  (match (mouth:speak text Mouth:QUEUE_ADD
			      bundle id)
	    (,Mouth:SUCCESS
	     (queue:take))
	    (,Mouth:ERROR
	     (signal:remove id)))))

      (define (before-possible-exit action::procedure)::void
	(set! save-state action))
      
      (define (listen . prompt)::string
	(let ((recognized ::BlockingQueue (ArrayBlockingQueue 1)))
	  (with-intent (recognize-speech
			(if (null? prompt)
			    #!null
			    (apply string-append prompt)))
	    (lambda (resultCode response)
	      (recognized:put 
	       (and-let* ((intent ::Intent response)
			  (extra ::java.util.List 
				 (intent:getStringArrayListExtra
				  RecognizerIntent:EXTRA_RESULTS))
			  ((not (extra:isEmpty))))
		 (extra 0)))))
	  (let ((result (or (recognized:take) #!null)))
	    (the-view:invalidate)
	    result)))
      
      (define (ask question::string)::string
	(say question)
	(listen question))

      (define (application-directory)::string
	(*:toString (invoke-special
		     android.content.Context
		     (this) 'getFilesDir)))

      (define (projects-directory)::string
	(*:toString (invoke-special
		     android.content.Context
		     (this) 'getExternalFilesDir #!null)))

      (define (show-keyboard!)::void
	(view:showKeyboard))

      (define (open-asset filename ::string)::gnu.kawa.io.InPort
	(let* ((assets ::AssetManager (invoke (this) 'getAssets)))
	  (gnu.kawa.io.InPort
	   (java.io.InputStreamReader
	    (assets:open filename)))))

      (define input-files '())
      
      (let-syntax ((export (syntax-rules ()
			     ((_ identifier ...)
			      (begin
				(env:define 'identifier #!null identifier)
				...)))))
	(export mouth say listen ask
		application-directory
		projects-directory
		show-keyboard!
		the-view
		before-possible-exit
		open-asset
		input-files
		)))
    
    (let* ((resources ::AndroidResources (getResources))
	   (metrics ::DisplayMetrics
		    (resources:getDisplayMetrics)))
      (screen:set-size! metrics:widthPixels metrics:heightPixels))
    
    (view:setSystemUiVisibility
     (bitwise-ior
      AndroidView:SYSTEM_UI_FLAG_FULLSCREEN
      #;AndroidView:SYSTEM_UI_FLAG_IMMERSIVE))
    (view:setFitsSystemWindows #t)
    (setContentView view)
    (set-painter! view)
    (view:request-redraw!)
    
    (let* ((parent ::AndroidView (view:getParent))
	   (span ::Rect (Rect))
	   (observer ::ViewTreeObserver (parent:getViewTreeObserver)))
      (observer:addOnGlobalLayoutListener
       (lambda ()
	 (parent:getWindowVisibleDisplayFrame span)
	 (screen:set-size! (span:width) (span:height))))
      (screen:set-size! (span:width) (span:height))

      (for expression in '((import (language define-interface))
			   (import (language define-object))
			   (import (language define-type))
			   (import (language define-syntax-rule))
			   (import (language define-parameter))
			   (import (language fundamental))
			   (import (language examples))
			   (import (language while))
			   (import (language for))
			   (import (language infix))
			   (import (language match))
			   (import (editor interfaces painting))
			   (import (editor interfaces elements))
			   (set-painter! the-view))
	(safely
	 (eval expression)))

      (for expression in init-script
	(safely
	 (eval expression)))
      
      (screen:set-size! (span:width) (span:height))
      (view:request-redraw!))
    
    (let ((postpone ::Postponed (EventRunner sync view)))
      (for finger from 0 below 10
	   (set! (process-finger finger)
		 (TouchEventProcessor finger screen
				      postpone
				      vicinity: 15))))
    )
  

  (AndroidActivity))
