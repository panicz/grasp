(module-name (editor input transforms))

(import (language define-type))
(import (language define-interface))
(import (language define-object))
(import (language define-parameter))
(import (utils functions))
(import (srfi :11))
(import (language match))
(import (language infix))
(import (language examples))
(import (language fundamental))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor utils interpolation))

(import (utils print))

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
  
(define-interface Transform (BiMap2D TransformParameters Struct)
  (within painter::Painter action::procedure)::void

  (translate! dx::real dy::real)::void

  (stretch! x00::real y00::real x10::real y10::real
            x01::real y01::real x11::real y11::real)
  ::void
  
  (scale! factor::real x::real y::real)::void

  (rotate! factor::real x::real y::real)::void

  ;; this one returns a copy of this transform
  ;; such that (inside-out inner-x inner-y)
  ;; would return (values outer-x outer-y)
  (translating inner-x::real inner-y::real
		#;to outer-x::real outer-y::real)
  ::Transform
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

  ((outside-in x::real y::real)::(Values real real)
   (values
    (- x left)
    (- y top)))
  
  ((inside-out x::real y::real)::(Values real real)
   (values
    (+ x left)
    (+ y top)))

  ((translating inner-x::real inner-y::real
		 #;to outer-x::real outer-y::real)
   ::Transform
   (Translation left: (- outer-x inner-x)
		top: (- outer-y inner-y)))
  
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
  ((inside-out x::real y::real)::(Values real real)
    (let ((dx (+ x left))
	  (dy (+ y top))
	  (s (sin angle/rad))
	  (c (cos angle/rad)))
      (values
       (as int (round (* scale (- (* c dx) (* s dy)))))
       (as int (round (* scale (+ (* s dx) (* c dy))))))))

  ;; edit or to document
  ((outside-in x::real y::real)::(Values real real)
    (let ((s (sin angle/rad))
	  (c (cos angle/rad)))
      (values
       (as int (round (- (/ (+ (* c x) (* s y)) scale) left)))
       (as int (round (- (/ (- (* c y) (* s x)) scale) top)))
       )))

 ((translating inner-x::real inner-y::real
	       #;to outer-x::real outer-y::real)
  ::Transform
  (let ((s (sin angle/rad))
	(c (cos angle/rad)))
    (Isogonal left: (- (/ (+ (* c outer-x) (* s outer-y)) scale)
		       inner-x)
	      top: (- (/ (- (* c outer-y) (* s outer-x)) scale)
		      inner-y)
	      scale: scale
	      angle/rad: angle/rad)))

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
   (let-values (((x0 y0) (outside-in x y)))
     (set! scale (* scale factor))
     (let-values (((x1 y1) (outside-in x y)))
       (set! left (as int (round (+ left (- x1 x0)))))
       (set! top (as int (round (+ top (- y1 y0))))))))

  ((rotate! angle/deg::real x::real y::real)::void
   (let-values (((x0 y0) (outside-in x y)))
     (set! angle/rad (+ angle/rad (* angle/deg pi 1/180)))
     (let-values (((x1 y1) (outside-in x y)))
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

(e.g.
 (and-let* ((t ::Transform (Translation))
	    (t ::Transform (t:translating 1 2 3 4))
	    (3 4 (t:inside-out 1 2))
	    (1 2 (t:outside-in 3 4)))))

(define (only-scale&rotation transform ::BiMap2D)::Transform
  (let*-values (((-left -top) (transform:outside-in 0 0))
		((x y) (transform:outside-in 1 0))
		((dx dy) (values (- x -left) (- y -top)))
		((scale) (hypotenuse dx dy))
		((angle) (atan (- dy) dx)))
    (Isogonal angle/rad: angle scale: (/ scale))))

(define-type (DragAnimation of: Drag
			    from: Position
			    to: Position
			    duration/ms: int
			    interpolate: Tween := sine-interpolation)
  implementing Animation
  with
  (progress/ms type: int init-value: 0)
  (previous-x type: real init-value: +nan.0)
  (previous-y type: real init-value: +nan.0)
  
  ((advance! timestep/ms::int)::boolean
   (when (or (nan? previous-x)
	     (nan? previous-y))
     (set! previous-x from:left)
     (set! previous-y from:top))
   (set! progress/ms (+ progress/ms timestep/ms))
   (if (is progress/ms < duration/ms)
       (let* ((progress ::float (/ progress/ms duration/ms))
	      (x (interpolate from:left to:left progress))
	      (y (interpolate from:top to:top progress))
	      (dx (- x previous-x))
	      (dy (- y previous-y)))
	 (of:move! x y dx dy)
	 (set! previous-x x)
	 (set! previous-y y)
	 #t)
       (let ((vx ::float (/ (- to:left previous-x) timestep/ms))
	     (vy ::float (/ (- to:top previous-y) timestep/ms)))
	 (of:drop! to:left to:top vx vy)
	 #f)
       )))

(define-type (Transition of: Transform
                         from: Transform
			 to: Transform
			 around: Position := #!null
			 duration/ms: int
			 progress/ms: int := 0
			 tween-scale: Tween
			 := sine-interpolation
			 tween-angle: Tween
			 := sine-interpolation
			 tween-left: Tween
			 := sine-interpolation
			 tween-top: Tween
			 := sine-interpolation)
  implementing Animation
  with
  ((advance! timestep/ms::int)::boolean
   (set! progress/ms (+ progress/ms timestep/ms))
   (let ((progress ::float (/ progress/ms duration/ms)))
     (cond
      (around
       (let*-values (((x0 y0) (of:inside-out
			       around:left
			       around:top))
                     ((scale) (tween-scale
			       (from:get-scale)
		               (to:get-scale)
		               progress))
		     ((angle) (tween-angle
			       (from:get-angle)
		               (to:get-angle)
		               progress))
	             ((x1 y1) (begin
				(of:set-scale! scale)
				(of:set-angle! angle)
				(of:inside-out
				 around:left
				 around:top))))
         (of:translate! (- x0 x1) (- y0 y1))))
      (else
       (of:set-scale!
	(tween-scale
	 (from:get-scale)
	 (to:get-scale)
	 progress))
       (of:set-angle!
	(tween-angle
	 (from:get-angle)
	 (to:get-angle)
	 progress))
       (of:set-left!
	(tween-left
	 (from:get-left)
	 (to:get-left)
	 progress))
       (of:set-top!
	(tween-top
	 (from:get-top)
	 (to:get-top)
	 progress))
       )))
   (is progress/ms < duration/ms)))
