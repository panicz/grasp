(module-name (editor types extensions gutenbergson))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language for))
(import (utils functions))
(import (language fundamental))

(import (language define-cache))

(import (editor interfaces painting))
(import (editor types primitive))
(import (editor interfaces elements))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor input transforms))
(import (editor input screen))

(import (editor types extensions extensions))
(import (editor types extensions combinators))

(import (utils print))

(define-alias Word string)

(define-enum FontStyle
  (Bold
   Italic
   Literal))

(define-type (EndFontStyle style: FontStyle))

(define-interface TextDivisionUnit ()
  (render! max-line-width::real
	   line-height::real
	   draw-word!::(maps (real real CharSequence (EnumSetOf FontStyle))
			     to: void))::real
  (height max-line-width::real line-height::real)::real
  )

(define-object (Paragraph content::(sequence-of
				    (either
				     Word
				     FontStyle
				     EndFontStyle)))
  ::TextDivisionUnit
  (define (render! max-line-width::real
		   line-height::real
		   draw-word!::(maps (real real CharSequence (EnumSetOf FontStyle))
				    to: void))::real
    (let ((style ::(EnumSetOf FontStyle) (EnumSet:noneOf FontStyle))
	  (left ::real 0)
	  (top ::real 0))
      (for token in content
	(match token
	  (word::Word
	   (let* ((word-width ::real 10 #;(painter:styled-text-width word style))
		  (expanded ::real (+ left word-width)))
	     (when (is expanded > max-line-width)
	       (set! left 0)
	       (set! top (+ top line-height)))

	     (draw-word! left top word style)

	     (set! left (+ expanded space-width))))
	  
	  (modifier::FontStyle
	   (style:add modifier))
	  
	  ((EndFontStyle style: modifier)
	   (style:remove modifier))))
      
      (+ top line-height)))

  (define cached-height
    (cache (max-line-width::real line-height::real)
	   (render! max-line-width line-height
		    (lambda (left::real top::real word::CharSequence
					style::(EnumSetOf FontStyle))
		      (values)))))
  
  (define (height max-line-width::real line-height::real)::real
    (cached-height max-line-width line-height))
 
  )

(define-object (InteractiveBookReader document-source::string)
  ::Maximizable
  
  (define (draw! context::Cursor)::void
    (painter:draw-caption! "GutenBergSon"))

  (define scroll ::real 0)
  
  (define size ::Extent
    (painter:caption-extent "GutenBergSon"))
  
  (define (set-size! width::real height::real anchor::ResizeAnchor)::void
    (set! size:width width)
    (set! size:height height))

  (define (extent)::Extent size)

  (define (value)::Object
    (cons (Atom "InteractiveBookReader")
	  (cons document-source (empty))))

  (MaximizableWidget))

(set! (extension 'InteractiveBookReader)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as InteractiveBookReader (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create InteractiveBookReader from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
