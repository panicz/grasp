(import (define-type))
(import (define-interface))
(import (define-object))
(import (painter))

(define-interface Transform ()
  (apply! painter::Painter)::void
  (unapply! painter::Painter)::void

  (map x::real y::real)::(Values real real)
  (unmap x::real y::real)::(Values real real)

  (translate! dx::real dy::real)::void

  (stretch! x00::real y00::real x10::real y10::real
            x01::real y01::real x11::real y11::real)
  ::void
  )

(define-object (Translation)::Transform
  (define left ::real 0)
  (define top ::real 0)

  (define (apply! painter::Painter)::void
    (painter:translate! left top))
  
  (define (unapply! painter::Painter)::void
    (painter:translate! (- left) (- top)))

  (define (map x::real y::real)::(Values real real)
    (values (+ x left) (+ y top)))
  
  (define (unmap x::real y::real)::(Values real real)
    (values (- x left) (- y top)))

  (define (translate! dx::real dy::real)::void
    (set! left (+ left dx))
    (set! top (+ top dy)))

  (define (stretch! x00::real y00::real x10::real y10::real
		    x01::real y01::real x11::real y11::real)
    ::void
    (values))
  )
