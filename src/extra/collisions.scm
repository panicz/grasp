(module-name (extra collisions))

(import (language define-type))
(import (language define-interface))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language examples))
(import (utils functions))

(define-interface Collider ()
  (dimensionality)::int
  (opening dimension ::int)::real
  (closing dimension ::int)::real
  (collides-with? another ::Collider)::boolean
  )

(define-type (Boundary position: real
		       item: Collider))

(define-type (OpeningBoundary)
  extending Boundary)

(define-type (ClosingBoundary)
  extending Boundary)

(define (opening collider ::Collider dimension ::int)::Boundary
  (OpeningBoundary item: collider
		   position: (collider:opening dimension)))

(define (closing collider ::Collider dimension ::int)::Boundary
  (ClosingBoundary item: collider
		   position: (collider:closing dimension)))

(define (dimensionality collider ::Collider)::int
  (collider:dimensionality))

(define-type (Box2D left: real
		    top: real
		    right: real
		    bottom: real)
  implementing Collider
  with
  ((dimensionality) 2)
  
  ((collides-with? another ::Collider)::boolean
   (match another
     (box::Box2D
      (and (is left <= box:right)
	   (is right >= box:left)
	   (is top <= box:bottom)
	   (is bottom >= box:top)))
     (circle::Circle
      (is (min (hypotenuse (- circle:left left)
			   (- circle:top top))
	       (hypotenuse (- circle:left right)
			   (- circle:top top))
	       (hypotenuse (- circle:left left)
			   (- circle:top bottom))
	       (hypotenuse (- circle:left right)
			   (- circle:top bottom))) <= circle:radius))))
  
  ((opening dimension ::int)::real
   (match dimension
     (1 top)
     (0 left)))
  
  ((closing dimension ::int)::real
   (match dimension
     (1 bottom)
     (0 right)))
  )

(define-type (Circle left: real
		     top: real
		     radius: real)
  implementing Collider
  with
  ((dimensionality) 2)
  
  ((collides-with? another ::Collider)::boolean
   (match another
     (circle::Circle
      (is (hypotenuse (- left circle:left)
		      (- top circle:top))
	  <= (+ radius circle:radius)))
     (box::Box2D
      (box:collides-with? (this)))))
  
  ((opening dimension ::int)::real
   (match dimension
     (1 (- top radius))
     (0 (- left radius))))
  
  ((closing dimension ::int)::real
   (match dimension
     (1 (+ top radius))
     (0 (+ left radius))))
  )

(define (collisions #;between items ::(list-of Collider))
  ;; Based on Game Programming Gems vol.2, chapter 2.7: "Recursive Dimension
  ;; Clustering: A Fast Algorithm for Collision Detection" by Steve Rabin
  (define (recursive-dimensional-clustering dimension ::int
					    items ::(list-of Collider))
    ::(list-of (list Collider Collider))
    (match items
      (`(,first ,second . ,rest)
     #;(assert (every (is (dimensionality _) = (dimensionality first) 
			  = (dimensionality second)) rest))
       (if (is dimension >= (dimensionality first))
	   (only (lambda (p)
		   (and-let* ((`(,first::Collider ,second::Collider) p))
		     (first:collides-with? second)))
		 (sublists items 2))
	   (let* ((boundaries (append-map (lambda (item)
					    `(,(opening item dimension) 
					      ,(closing item dimension)))
					  items))
		  (sorted (sort boundaries (is _:position < _:position))))
	     (let process ((boundaries sorted)
			   (groups '())
			   (current-group '()))
	       (match boundaries
		 ('()
		  (let ((groups (if (null? current-group)
				    groups
				    `(,current-group . ,groups))))
		    (append-map (lambda (group)
				  (recursive-dimensional-clustering
				   (+ dimension 1) group))
				groups)))
		 (`(,(ClosingBoundary position: p item: e) . ,remaining-boundaries)
		  (process remaining-boundaries
			   (or (and-let* ((`(,_ ,_ . ,_) current-group))
				 `(,current-group . ,groups))
			       groups)
			   (only (isnt _ eq? e) current-group)))
		 
		 (`(,(OpeningBoundary position: p item: e) . ,remaining-boundaries)
		  (let ((current-group `(,e . ,current-group)))
		    (process remaining-boundaries
			     groups
			     current-group))))))))
      (_ '())))
  (recursive-dimensional-clustering 0 items))

(define (circle-bounding-box circle ::Circle)::Box2D
  (Box2D left: (- circle:left circle:radius)
	 top: (- circle:top circle:radius)
	 right: (+ circle:left circle:radius)
	 bottom: (+ circle:top circle:radius)))

(define (box-bounding-circle box::Box2D)::Circle
  (let* ((width (- box:right box:left))
	 (height (- box:bottom box:top))
	 (diameter (hypotenuse width height)))
    (Circle left: (+ box:left (* 0.5 width))
	    top: (+ box:top (* 0.5 height))
	    radius: (* 0.5 diameter))))

(e.g.
 ;; see fig. 2.7.4 (p. 231) in Game Programming Gems vol. 2
 (let ((D (Circle left: 6 top: 16 radius: 3))
       (E (Circle left: 14 top: 14 radius: 3))
       (F (Circle left: 19 top: 11 radius:  4))
       (G (Circle left: 22 top: 3 radius: 2)))
   (equal? (collisions (list D E F G))
	   `((,E ,F)))))

