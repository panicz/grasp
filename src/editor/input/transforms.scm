(module-name (editor input transforms))

(import (language define-type))
(import (language define-interface))
(import (language define-object))
(import (language define-parameter))
(import (utils functions))
(import (srfi :11))
(import (language match))
(import (language infix))
(import (language fundamental))
(import (editor interfaces painting))

(import (utils print))

(define-interface Map2D ()
  (map x::real y::real)::(Values real real)
  (unmap x::real y::real)::(Values real real)
  )

(define-interface TransformParameters ()
  (get-angle)::real
  (set-angle! rad::real)::void
  (get-scale)::real
  (set-scale! s::real)::void
  (get-left)::real
  (set-left! l::real)::void
  (get-top)::real
  (set-top! t::real)::void
  )
  
(define-interface Transform (Map2D TransformParameters Struct)
  (within painter::Painter action::procedure)::void

  (translate! dx::real dy::real)::void

  (stretch! x00::real y00::real x10::real y10::real
            x01::real y01::real x11::real y11::real)
  ::void
  
  (scale! factor::real x::real y::real)::void

  (rotate! factor::real x::real y::real)::void

  )

(define-type (Translation left: real := 0
			  top: real := 0)
  implementing Transform
  with

  ((within painter::Painter action::procedure)::void
   (let ((l left)
	 (t top))
     (painter:translate! l t)
     (action)
     (painter:translate! (- l) (- t))))

  ((map x::real y::real)::(Values real real)
   (values
    (- x left)
    (- y top)))
  
  ((unmap x::real y::real)::(Values real real)
   (values
    (+ x left)
    (+ y top)))

  ((translate! dx::real dy::real)::void
    (set! left (as int (round (+ left dx))))
    (set! top (as int (round (+ top dy)))))

  ((stretch! x00::real y00::real x10::real y10::real
		    x01::real y01::real x11::real y11::real)
    ::void
    (WARN "Translation does not support stretching"))

  ((scale! factor::real x::real y::real)::void
   (values))

  ((rotate! factor::real x::real y::real)::void
   (values))
  
  ((get-angle)::real 0)
  ((set-angle! rad::real)::void (values))
  ((get-scale)::real 1.0)
  ((set-scale! s::real)::void (values))
  ((get-left)::real left)
  ((set-left! l::real)::void (set! left (as int (round l))))
  ((get-top)::real top)
  ((set-top! t::real)::void (set! top (as int (round t))))
  )

(define-parameter (default-transform)::(maps () to: Transform)
  (lambda () (Translation)))

(define-type (Isogonal left: real := 0
		       top: real := 0
		       scale: real := 1.0
		       angle/rad: real := 0.0)

  implementing Transform
  with
  
  ((within painter::Painter action::procedure)::void
   (let ((s scale)
	 (a angle/rad)
	 (l left)
	 (t top))
     (painter:scale! s)
     (painter:rotate! a)
     (painter:translate! l t)
     (action)
     (painter:translate! (- l) (- t))
     (painter:rotate! (- a))
     (painter:scale! (/ 1.0 s))))

  ;; document to editor
  ((unmap x::real y::real)::(Values real real)
    (let ((dx (+ x left))
	  (dy (+ y top))
	  (s (sin angle/rad))
	  (c (cos angle/rad)))
      (values
       (as int (round (* scale (- (* c dx) (* s dy)))))
       (as int (round (* scale (+ (* s dx) (* c dy))))))))

  ;; edit or to document
  ((map x::real y::real)::(Values real real)
    (let ((s (sin angle/rad))
	  (c (cos angle/rad)))
      (values
       (as int (round (- (/ (+ (* c x) (* s y)) scale) left)))
       (as int (round (- (/ (- (* c y) (* s x)) scale) top)))
       )))

  ((translate! dx::real dy::real)::void
    (let ((s (sin angle/rad))
	  (c (cos angle/rad)))

      (set! left (+ left (/ (+ (* c dx) (* s dy))
			    scale)))
      (set! top (+ top (/ (- (* c dy) (* s dx))
			  scale)))))

  ((stretch! x00::real y00::real x10::real y10::real
	     x01::real y01::real x11::real y11::real)
    ::void
    (let* ((px ::real (- x00 x10))
	   (py ::real (- y00 y10))
	   (d1 ::real (hypotenuse px py))
	   (sx ::real (- x01 x11))
	   (sy ::real (- y01 y11))
	   (d2 ::real (hypotenuse sx sy))
	   (s (sin angle/rad))
	   (c (cos angle/rad))
	   (scale* ::real (/ (* scale d2) d1))
	   (da ::real (- (atan sy sx) (atan py px)))
	   (angle*/rad ::real (+ angle/rad da))
	   (s* ::real (sin angle*/rad))
	   (c* ::real (cos angle*/rad))
	   (dx ::real (- (/ (+ (* c x00) (* s y00))
			    scale)
			 (/ (+ (* c* x01) (* s* y01))
			    scale*)))
	   (dy ::real (- (/ (- (* c y00) (* s x00))
			    scale)
			 (/ (- (* c* y01) (* s* x01))
			    scale*))))
      (set! left (as int (round (- left dx))))
      (set! top (as int (round (- top dy))))
      (set! angle/rad angle*/rad)
      (set! scale scale*)))
  
  ((scale! factor::real x::real y::real)::void
   (let-values (((x0 y0) (map x y)))
     (set! scale (* scale factor))
     (let-values (((x1 y1) (map x y)))
       (set! left (as int (round (+ left (- x1 x0)))))
       (set! top (as int (round (+ top (- y1 y0))))))))

  ((rotate! angle/deg::real x::real y::real)::void
   (let-values (((x0 y0) (map x y)))
     (set! angle/rad (+ angle/rad (* angle/deg pi 1/180)))
     (let-values (((x1 y1) (map x y)))
       (set! left (as int (round (+ left (- x1 x0)))))
       (set! top (as int (round (+ top (- y1 y0))))))))
  
  ((get-angle)::real angle/rad)
  ((set-angle! rad::real)::void
   (set! angle/rad rad))
  ((get-scale)::real scale)
  ((set-scale! s::real)::void
   (set! scale (as double s)))
  ((get-left)::real left)
  ((set-left! l::real)::void
   (set! left l))
  ((get-top)::real top)
  ((set-top! t::real)::void
   (set! top t))
  )

(define-type (Transition of: Transform
                         from: Transform
			 to: Transform
			 around: Position := #!null
			 duration/ms: int
			 progress/ms: int := 0)
  implementing Animation
  with
  ((advance! timestep/ms::int)::boolean
   (set! progress/ms (+ progress/ms timestep/ms))
   (let ((progress ::float (/ progress/ms duration/ms)))
     (cond
      (around
       (let*-values (((x0 y0) (of:unmap around:left around:top))
                     ((scale) (tween (from:get-scale)
		                     (to:get-scale)
		                     progress))
		     ((angle) (tween (from:get-angle)
		                     (to:get-angle)
		                     progress))
	             ((x1 y1) (begin
				(of:set-scale! scale)
				(of:set-angle! angle)
				(of:unmap around:left around:top))))
         (of:translate! (- x0 x1) (- y0 y1))))
      (else
       (of:set-scale! (tween (from:get-scale) (to:get-scale)
			     progress))
       (of:set-angle! (tween (from:get-angle) (to:get-angle)
			     progress))
        (of:set-left! (tween (from:get-left) (to:get-left)
			    progress))
       (of:set-top! (tween (from:get-top) (to:get-top)
			   progress))
       )))
   (is progress/ms < duration/ms))

  ((tween initial-value::real final-value::real progress::real)
   ::real
   (cond
    ((is progress <= 0) initial-value)
    ((is progress >= 1) final-value)
    (else
     (+ initial-value
        (* (- final-value initial-value)
	   (sin (* progress pi/2))))))))
