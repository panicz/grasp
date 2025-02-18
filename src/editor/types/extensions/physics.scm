(module-name (editor types extensions physics))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language for))
(import (utils functions))
(import (language fundamental))

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
(import (editor types extensions canvas))
(import (extra collisions))

(import (utils print))

(define-type (ContactPoint position: (sequence-of real)
			   direction: (sequence-of real)))

(define-interface Body (Animate Collider)
  (impulse! force ::real contact-point ::ContactPoint)
  ::void
  
  (contact-points another ::Collider)
  ::(list-of ContactPoint)
  
  (current-mass)::real
  (current-velocity)::(sequence-of real)
  (current-bounciness)::real
  
  )

(define (collide! one ::Body #;with another ::Body)::void
  (let ((contacts (one:contact-points another)))
    (for contact::ContactPoint in contacts
      (and-let* ((m1 ::real (one:current-mass))
		 (m2 ::real (another:current-mass))
		 (m ::real (/ (+ (/ m1) (/ m2))))
		 ((is m > 0))
		 ((is m finite?))
		 (v1 ::real
		     (apply
		      + (map * contact-point:direction
			     (one:current-velocity))))
		 (v2 ::real
		     (apply
		      + (map * contact-point:direction
			     (another:current-velocity))))
		 (b1 ::real (one:current-bounciness))
		 (b2 ::real (another:current-bounciness))
		 (bounciness ::real (+ 1.0 (* b1 b2)))
		 (impulse ::real (* bounciness
				    m (- v2 v1))))
	(one:impulse! impulse contact)
	(another:impulse! (- impulse) contact)))))

(define (magnitude v::(sequence-of real))::real
  (sqrt (apply + (map square v))))

(define (distance v1::(sequence-of real)
		  v2::(sequence-of real))
  ::real
  (magnitude (map - v1 v2)))

(define-type (PhysicalSphere mass: real
			     velocity: (sequence-of real)
			     bounciness: real := 0.9
			     color: long := #x000000)
  extending BoundingSphere with
  ((contact-points another ::Body)
   ::(list-of ContactPoint)
   (match another
     (sphere::PhysicalSphere
      (otherwise '()
	(and-let* ((r (map - sphere:center (the center)))
		   (d (magnitude r))
		   ((is r > 0))
		   (n (map (lambda (x) (/ x d)) r))
		   (p (- (+ (the radius)
			    sphere:radius) d))
		   ((is p >= 0)))
	  `(,(ContactPoint
	      position: (map (lambda (c v)
			       (+ c (* (- (the radius)
					  (* 0.5 p))
				       v)))
			     (the center) n)
	      direction: n)))))
     
     (wall::GridWall
      (otherwise '()
	(and-let* ((center (the center))
		   (dimensions (min (length wall:open)
				    (length wall:close)
				    (length center)))
		   ((is dimensions > 0))
		   (position ((array-of real)
			    length: dimensions)))
	  (set! (position 0)
	    (argmin (lambda (x)
		      (abs (- x (center 0))))
		    (wall:open 0) (wall:close 0)))
	  (for i from 1 below dimensions
	       (set! (position i)
		 (if (is (wall:open i) <= (center i)
			 <= (wall:close i))
		     (center i)
		     (argmin (lambda (x)
			       (abs (- x (center i))))
			     (wall:open i)
			     (wall:close i)))))
	  (and-let* ((direction (map - position center))
		     (distance (magnitude direction))
		     ((is distance > 0))
		     (normal (map (lambda (x)
				    (/ x distance))
				  direction)))
	    `(,(ContactPoint
		position: position
		direction: normal))))))))
  implementing Body with
  ((advance! timestep/ms::int)::boolean
   (let ((c (the center))
	 (v (the velocity)))
     (for i from 0 below (min (length c)
			      (length v))
	  (set! (c i) (+ (c i) (* (v i)
				  timestep/ms))))))

  ((render!)
   (let ((c (the center)))
     (painter:precise-fill-circle!
      (c 0) (c 1) (the radius) color)))

  ((current-mass)::real mass)

  ((current-bounciness)::real bounciness)
  
  ((current-velocity)::(sequence real)
   velocity)
  
  ((impulse! force ::real contact-point ::ContactPoint)
   ::void
   (let ((v (the velocity))
	 (/m (/ mass))
	 (n contact-point:direction))
     (for i from 0 below (min (length v)
			      (length n))
	  (set! (v i) (+ (v i) (* force (n i) /m))))))
  )
	  
(define-type (GridWall bounciness: real := 0.9
		       color: long := #x000000)
  extending BoundingBox with
  ((contact-points another ::Body)
   ::(list-of ContactPoint)
   (match another
     (sphere::BoundingSphere
      (let ((points (sphere:contact-points (this))))
	(for p::ContactPoint in points
	  (ContactPoint position: p:position
			direction: (map - direction)))))
     (_
      #| For now we assume that walls
      cannot collide with other walls |#
      '())))
  
  implementing Body with
  ((advance! timestep/ms::int)::boolean
   (values))

  ((render!)::void
   (let ((opening (the open))
	 (closing (the close)))
     (when (and (every finite? opening)
		(every finite? opening))
       (painter:precise-fill-rectangle!
	(opening 0) (opening 1)
	(closing 0) (closing 1)
	color))))
  
  ((current-mass)::real +inf.0)
  
  ((current-velocity)::(sequence real)
   '(0.0 0.0 0.0))

  ((current-bounciness)::real bounciness)
  
  ((impulse! force ::real contact-point ::ContactPoint)
   ::void
   (values)))

(define-object (PhysicsStage width ::real
			     height ::real
			     content ::(list-of
					Body))  
  ::World
  (define (set-size! w::real h::real anchor::ResizeAnchor)
    ::void
    (set! width w)
    (set! height h)
    (invoke-special
     PreciseCanvas (this) 'set-size!
     w h anchor))

  (define (advance! timestep/ms::int)::boolean
    (for item ::Animate in content
	 (item:advance! timestep/ms))
    (for (a::Body b::Body) in (collisions content)
      (collide! a #;with b))
    )

  (PreciseCanvas width height content))

(set! (extension 'PhysicsStage)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (bordered
	       (as PhysicsStage (eval source))) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Movement from "
		    source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
