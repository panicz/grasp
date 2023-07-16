(import (define-type))
(import (define-interface))
(import (define-object))
(import (define-parameter))
(import (functions))
(import (painter))
(import (print))

(define-interface Map2D ()
  (map x::real y::real)::(Values real real)
  (unmap x::real y::real)::(Values real real)
  )

(define-interface Transform (Map2D)
  (apply! painter::Painter)::void
  (unapply! painter::Painter)::void

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
    (values (- x left) (- y top)))
  
  (define (unmap x::real y::real)::(Values real real)
    (values (+ x left) (+ y top)))

  (define (translate! dx::real dy::real)::void
    (set! left (+ left dx))
    (set! top (+ top dy)))

  (define (stretch! x00::real y00::real x10::real y10::real
		    x01::real y01::real x11::real y11::real)
    ::void
    (WARN "Translation does not suppirt stretching"))
  )

(define-parameter (default-transform)::(maps () to: Transform)
  (lambda () (Translation)))

(define-object (Isogonal)::Transform

  (define left ::real 0)
  (define top ::real 0)
  (define scale ::real 1.0)
  (define 1/scale ::real 1.0)

  (define angle/rad ::real 0.0) 

  (define s ::real 0.0)
  (define c ::real 1.0)
  
  (define (apply! painter::Painter)::void
    (painter:scale! scale)
    (painter:rotate! angle/rad)
    (painter:translate! left top))
  
  (define (unapply! painter::Painter)::void
    (painter:translate! (- left) (- top))
    (painter:rotate! (- angle/rad))
    (painter:scale! 1/scale))

  ;; document to editor
  (define (unmap x::real y::real)::(Values real real)
    (let ((dx (+ x left))
	  (dy (+ y top)))
      (values
       (* scale (- (* c dx) (* s dy)))
       (* scale (+ (* s dx) (* c dy))))))

  ;; editor to document
  (define (map x::real y::real)::(Values real real)
    (values
     (- (* (+ (* c x) (* s y)) 1/scale) left)
     (- (* (- (* c y) (* s x)) 1/scale) top)))

  (define (translate! dx::real dy::real)::void
    (set! left (+ left (* (+ (* c dx) (* s dy))
			  1/scale)))
    (set! top (+ top (* (- (* c dy) (* s dx))
			1/scale))))

  (define (stretch! x00::real y00::real x10::real y10::real
		    x01::real y01::real x11::real y11::real)
    ::void
    (let* ((px ::real (- x00 x10))
	   (py ::real (- y00 y10))
	   (d1 ::real (hypotenuse px py))
	   (sx ::real (- x01 x11))
	   (sy ::real (- y01 y11))
	   (d2 ::real (hypotenuse sx sy))
	   (scale* ::real (/ (* scale d2) d1))
	   (1/scale* ::real (/ 1.0 scale*))
	   (da ::real (- (atan sy sx) (atan py px)))
	   (angle*/rad ::real (+ angle/rad da))
	   (s* ::real (sin angle*/rad))
	   (c* ::real (cos angle*/rad))
	   (dx ::real (- (* (+ (* c x00) (* s y00))
			    1/scale)
			 (* (+ (* c* x01) (* s* y01))
			    1/scale*)))
	   (dy ::real (- (* (- (* c y00) (* s x00))
			    1/scale)
			 (* (- (* c* y01) (* s* x01))
			    1/scale*))))
      (set! left (- left dx))
      (set! top (- top dy))
      (set! angle/rad angle*/rad)
      (set! s s*)
      (set! c c*)
      (set! scale scale*)
      (set! 1/scale 1/scale*)))
  )
