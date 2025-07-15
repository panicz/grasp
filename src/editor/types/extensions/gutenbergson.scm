(module-name (editor types extensions gutenbergson))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language for))
(import (utils functions))
(import (language fundamental))
(import (language assert))
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

(define-interface TextDivisionUnit ()
  (render! max-line-width::real line-height::real)::real
  (height max-line-width::real line-height::real)::real
  )

(define-object (Paragraph content::(sequence-of
				    (either
				     Word
				     TextStyle
				     EndTextStyle)))
  ::TextDivisionUnit
  (define (layout-words word-operation::(maps (real real Word TextDecoration)
					      to: void)
			max-line-width::real
			line-height::real)
    ::real
    (let ((style ::TextDecoration (RegularText))
	  (left ::real 0)
	  (top ::real 0))
      (for token in content
	(match token
	  (word::Word
	   (let* ((word-width ::real (painter:styled-text-width word style))
		  (space-width ::real (painter:styled-text-width " " style))
		  (expanded ::real (+ left word-width)))
	     (when (is expanded > max-line-width)
	       (set! left 0)
	       (set! top (+ top line-height)))

	     (word-operation left top word style)

	     (set! left (+ expanded space-width))))
	  
	  (modifier::TextStyle
	   (assert (not (style:contains modifier)))
	   (style:add modifier))
	  
	  ((EndTextStyle style: modifier)
	   (assert (style:contains modifier))
	   (style:remove modifier))))
      
      (+ top line-height)))

  (define (render! max-line-width::real
		   line-height::real)
    ::real
    (layout-words (lambda (left::real top::real word::Word style::TextDecoration)
		    (painter:draw-styled-text! left top word style))
		  max-line-width
		  line-height))
  
  (define cached-height
    (cache (max-line-width::real line-height::real)
	   (layout-words nothing max-line-width line-height)))
  
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
