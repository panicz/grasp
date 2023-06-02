(import (define-interface))
(import (define-type))
(import (define-object))
(import (extent))
(import (match))
(import (infix))
(import (functions))
(import (fundamental))
(import (indexable))
(import (painter))
(import (cursor))
(import (primitive))
(import (extension))
(import (print))

(define-type (Button action: (maps () to: void)
		     label: string)
  extending Magic
  with
  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (painter:caption-extent label))
	  (horizontal-margin ::real
			     (painter:caption-horizontal-margin))
	  (vertical-margin ::real
			   (painter:caption-vertical-margin)))
    (painter:draw-rounded-rectangle!
      (+ inner:width (* horizontal-margin 2))
      (+ inner:height (* vertical-margin 2)))
    (with-translation (horizontal-margin vertical-margin)
      (painter:draw-caption! label))))

  ((as-expression)::cons
   (origin (this)))
  
  ((extent)::Extent
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (painter:caption-extent label))
	  (horizontal-margin ::real
			     (painter:caption-horizontal-margin))
	  (vertical-margin ::real
			   (painter:caption-vertical-margin)))

     (Extent width: (+ inner:width (* horizontal-margin 2))
	     height: (+ inner:height (* vertical-margin 2)))))

  ((key-pressed key::char)::boolean
   (cond ((eq? key #\newline)
	  (action)
	  #t)
	 (else
	  #f)))
  
  ((tap! finger::byte x::real y::real)::boolean
   (action)
   #t))

(set! (extension 'Button)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as Button (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Button from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
