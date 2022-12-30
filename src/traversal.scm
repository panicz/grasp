(import (define-interface))
(import (define-type))
(import (default-value))
(import (define-parameter))
(import (extent))
(import (fundamental))
(import (indexable))
(import (painter))
(import (match))
(import (infix))
(import (print))


(define-type (Traversal left: real := 0
			top: real := 0
			index: int := 0
			max-width: real := 0
			max-line-height: real := 0)
  extending Base with
  ((advance/extent! extent::Extent)::void
   (set! left (+ left extent:width))
   (set! max-line-height (max extent:height
			      max-line-height))
   (set! max-width (max left max-width))
   (set! index (+ index 1)))

  ((advance-by! width::real)::void
   (set! left (+ left width))
   (set! max-width (max max-width left)))

  ((new-line!)::void
   (set! top (+ top max-line-height))
   (set! left 0)
   (set! max-line-height (invoke (the-painter)
				 'min-line-height)))
  
  )


(define-parameter (the-traversal) ::Traversal
  (Traversal left: 0
	     top: 0
	     index: 0
	     max-width: 0
	     max-line-height: 0))
