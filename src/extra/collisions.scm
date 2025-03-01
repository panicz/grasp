(module-name (extra collisions))

(import (language define-type))
(import (language define-interface))
(import (language define-object))
(import (language for))
(import (language infix))
(import (language match))
(import (language examples))
(import (utils functions))

(define-interface Collider ()
  (dimensionality)::int
  (opening dimension ::int)::real
  (closing dimension ::int)::real
  (current-center)::(sequence-of real)
  (move-by! displacement::(sequence-of real))::void
  (collides-with? another ::Collider)::boolean
  )

(define-type (Boundary position: real
		       item: Collider))

(define-type (OpeningBoundary)
  extending Boundary)

(define-type (ClosingBoundary)
  extending Boundary)

(define (opening collider ::Collider dimension ::int)
  ::Boundary
  (OpeningBoundary item: collider
		   position: (collider:opening dimension)))

(define (closing collider ::Collider dimension ::int)
  ::Boundary
  (ClosingBoundary item: collider
		   position: (collider:closing dimension)))

(define (dimensionality collider ::Collider)::int
  (collider:dimensionality))

(define-type (BoundingSphere center: (sequence-of real)
			     radius: real)
  implementing Collider
  with
  ((dimensionality) (length center))
  
  ((collides-with? another ::Collider)::boolean
   (match another
     (circle::BoundingSphere
      (is (apply + (map square
			(map - center circle:center)))
	  <= (square (+ radius circle:radius))))
     (box::BoundingBox
      (box:collides-with? (this)))))

  ((current-center)::(sequence-of real)
   center)

  ((move-by! displacement::(sequence-of real))::void
   (for i from 0 below (min (length displacement)
			    (length center))
	(set! (center i) (+ (center i) (displacement i)))))
  
  ((opening dimension ::int)::real
   (- (center dimension) radius))
  
  ((closing dimension ::int)::real
   (+ (center dimension) radius))
  )

(define-type (BoundingBox open: (sequence-of real)
			  close: (sequence-of real))
  implementing Collider
  with
  ((dimensionality)
   ;;(assert (= (length open) (length close)))
   (length open))

  ((current-center)::(sequence-of real)
   (let* ((d (dimensionality))
	  (result (make-vector d)))
     (for i from 0 below d
	  (set! (result i) (/ (+ (open i) (close i)) 2)))
     result))

  ((move-by! displacement::(sequence-of real))::void
   (for i from 0 below (min (length displacement)
			    (dimensionality))
	(set! (open i) (+ (open i) (displacement i)))
	(set! (close i) (+ (close i) (displacement i)))))
  
  ((collides-with? another ::Collider)::boolean
   (match another
     (box::BoundingBox
      (escape-with return
	(for i from 0 below (min (dimensionality)
				 (box:dimensionality))
	     (unless (overlap? (open i) (close i)
			       (box:open i) (box:close i))
	       (return #f))
	     (return #t))))
     
     (sphere::BoundingSphere
      (let ((dmin 0))
	(for i from 0 below (min (dimensionality)
				 (sphere:dimensionality))
	     (cond
	      ((is (sphere:center i) <= (open i))
	       (set! dmin (+ dmin
			     (square (- (sphere:center i)
					(open i))))))
	      ((is (sphere:center i) >= (close i))
	       (set! dmin (+ dmin
			     (square (- (sphere:center i)
					(close i))))))))
	(is dmin <= (square sphere:radius))))))
  
  ((opening dimension ::int)::real
   (open dimension))
  
  ((closing dimension ::int)::real
   (close dimension))
  )


(define (collisions #;among items ::(list-of Collider))
  ;; Based on Game Programming Gems vol.2, chapter 2.7:
  ;; "Recursive Dimension Clustering: A Fast Algorithm
  ;; for Collision Detection" by Steve Rabin
  (define (clusters dimension ::int items ::(list-of Collider))
    ::(list-of (list Collider Collider))
    (match items
      (`(,first ,second . ,rest)
       #;(assert (every (is (dimensionality _)
			  = (dimensionality first) 
			  = (dimensionality second)) rest))
       (if (is dimension >= (dimensionality first))
	   (only (lambda (p)
		   (and-let* ((`(,first::Collider
				 ,second::Collider) p))
		     (first:collides-with? second)))
		 (sublists items 2))
	   (let* ((boundaries (append-map
			       (lambda (item)
				 `(,(opening item dimension) 
				   ,(closing item dimension)))
			       items))
		  (sorted (sort boundaries (is _:position
					       < _:position))))
	     (let process ((boundaries sorted)
			   (groups '())
			   (current-group '()))
	       (match boundaries
		 ('()
		  (let ((groups (if (null? current-group)
				    groups
				    `(,current-group
				      . ,groups))))
		    (append-map (lambda (group)
				  (clusters
				   (+ dimension 1) group))
				groups)))
		 (`(,(ClosingBoundary position: p item: e)
		    . ,remaining-boundaries)
		  (process remaining-boundaries
			   (or (and-let* ((`(,_ ,_ . ,_)
					   current-group))
				 `(,current-group . ,groups))
			       groups)
			   (only (isnt _ eq? e)
				 current-group)))
		 
		 (`(,(OpeningBoundary position: p item: e)
		    . ,remaining-boundaries)
		  (let ((current-group `(,e . ,current-group)))
		    (process remaining-boundaries
			     groups
			     current-group))))))))
      (_ '())))
  (clusters 0 items))

(e.g.
 ;; see fig. 2.7.4 (p. 231) in Game Programming Gems vol. 2
 (let ((D (BoundingSphere center: #(6 16) radius: 3))
       (E (BoundingSphere center: #(14 14) radius: 3))
       (F (BoundingSphere center: #(19 11) radius: 4))
       (G (BoundingSphere center: #(22 3) radius: 2)))
   (equal? (collisions (list D E F G))
	   `((,E ,F)))))

