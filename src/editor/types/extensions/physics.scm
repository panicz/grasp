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
(import (editor types extensions widgets))

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
		      + (map * contact:direction
			     (one:current-velocity))))
		 (v2 ::real
		     (apply
		      + (map * contact:direction
			     (another:current-velocity))))
		 (b1 ::real (one:current-bounciness))
		 (b2 ::real (another:current-bounciness))
		 (bounciness ::real (+ 1.0 (* b1 b2)))
		 (impulse ::real (* bounciness
				    m (- v1 v2))))
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
  ((current-mass)::real mass)

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
		   (position (make-vector dimensions)))
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
		direction: normal))))))
  ))

  implementing Body with
  ((advance! timestep/ms::int)::boolean
   (let ((c (the center))
	 (v (the velocity)))
     (for i from 0 below (min (length c)
			      (length v))
	  (set! (c i) (+ (c i) (* (v i)
				  timestep/ms))))))

  ((render!)::void
   (let ((c (the center)))
     (painter:precise-fill-circle!
      (c 0) (c 1) (the radius) color)))

  ((current-bounciness)::real bounciness)
  
  ((current-velocity)::(sequence-of real)
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
      (map (lambda (p::ContactPoint)
	     (ContactPoint position: p:position
			   direction: (map - p:direction)))
	   (sphere:contact-points (this))))
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
  
  ((current-velocity)::(sequence-of real)
   '(0.0 0.0 0.0))

  ((current-bounciness)::real bounciness)
  
  ((impulse! force ::real contact-point ::ContactPoint)
   ::void
   (values)))

(define-object (PhysicsStage width ::real
			     height ::real
			     content ::(list-of
					Body))  
  ::WorldPlayer

  (define left-wall ::GridWall
    (GridWall open: (vector -inf.0 0)
	      close:
	      (vector
	       0
	       (* (painter:precise-resolution-down)
		  height))))

  (define top-wall ::GridWall
    (GridWall open: (vector 0 -inf.0)
	      close:
	      (vector
	       (* (painter:precise-resolution-right)
		  width)
	       0)))

  (define right-wall ::GridWall
    (GridWall open:
	      (vector
	       (* (painter:precise-resolution-down)
		  width)
	       0)
	      close:
	      (vector
	       +inf.0
	       (* (painter:precise-resolution-down)
		  height))))

  (define bottom-wall ::GridWall
    (GridWall open:
	      (vector
	       0
	       (* (painter:precise-resolution-down)
		  height))
	      close:
	      (vector
	       (* (painter:precise-resolution-down)
		  width)
	       +inf.0)))
  
  (define (set-size! w::real h::real anchor::ResizeAnchor)
    ::void

    (let-values (((w* h*)
		  (painter:precise-outside-in w h)))
      (set! (right-wall:open 0) w*)
      (set! (top-wall:close 0) w*)
      (set! (bottom-wall:close 0) w*)
    
      (set! (bottom-wall:open 1) h*)
      (set! (left-wall:close 1) h*)
      (set! (right-wall:close 1) h*))

    (set! width w)
    (set! height h)
    (invoke-special
     PreciseCanvas (this) 'set-size!
     w h anchor))

  (define content-with-walls ::(list-of Body)
    `(,left-wall
      ,top-wall
      ,right-wall
      ,bottom-wall
      . ,content))

  (define running? ::boolean #t)
  
  (define (advance! timestep/ms::int)::boolean
    (for item ::Animate in content
	 (item:advance! timestep/ms))
    (for (a::Body b::Body) in (collisions
			       content-with-walls)
      (collide! a #;with b))
    running?
    )

  (define (rewind!)::void
    (WARN "PhysicalStage cannot be rewinded"))

  (define (back!)::void
    (WARN "PhysicalStage cannot be moved back"))
  
  (define (play!)::void
    (set! running? #t)
    (painter:play! (this)))
  
  (define (pause!)::void
    (set! running? #f))
  
  (define (next!)::void
    (advance! 40))
  
  (define (fast-forward!)::void
    (WARN "PhysicalStage cannot be fast-forwarded"))
  
  (define (playing?)::boolean
    running?)
  
  (PreciseCanvas width height content))

(set! (extension 'PhysicsStage)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (BorderedAnimation
	       (as PhysicsStage (eval source))) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create PhysicsStage from "
		    source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))

(define-simple-extension (PhysicsStepper
			  width ::real
			  height ::real
			  content ::(list-of
				     Body))
  (PlayerWithControls
   (PhysicsStage width height content)))

(set! (extension 'PhysicsStepper)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (as PhysicsStepper (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create PhysicsStepper from "
		    source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
