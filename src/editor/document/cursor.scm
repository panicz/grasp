(module-name (editor document cursor))

(import (srfi :11))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-cache))
(import (language define-property))
(import (language define-parameter))
(import (language keyword-arguments))
(import (language infix))
(import (language fundamental))
(import (language match))
(import (language assert))
(import (language examples))

(import (utils functions))
(import (utils print))

(import (editor interfaces elements))

;; See the `fundamental.scm` file for a detailed explanation
;; how cursors are represented

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (let ((x :: Indexable object))
	   (x:part-at index)))
	
	(else
	 object
	 #;(error "Don't know how to extract "index
		" from "object))))


(define (cursor-ref #!optional
		    (tile (the-document))
		    (cursor::Cursor (the-cursor)))
  (match cursor
    ('()
     tile)
    (`(,head . ,tail)
     (let ((parent (cursor-ref tile tail)))
       (part-at head parent)))
    (_
     (error "Unable to refer to cursor "cursor
	    " in "tile))))

(define (the-expression #!key
			(at::Cursor (the-cursor))
			(in (the-document)))
  (cursor-ref in at))

(define (fully-expanded? cursor::Cursor #;on document)
  ::boolean
  (and-let* ((`(,tip . ,root) cursor)
             (target (cursor-ref document root)))
    (eq? target (part-at tip target))))

(define (cursor-core cursor::Cursor document)::Cursor
  (otherwise cursor
    (and-let* ((`(,tip . ,root) cursor)
	       (parent ::Indexable (cursor-ref document root))
	       (target ::Indexable (parent:part-at tip)))
      (if (eq? parent target)
	  (cursor-core root document)
	  cursor))))

(define (cursor-terminal+stem #!optional
		     (cursor ::Cursor (the-cursor))
		     (document (the-document)))
  ::(Values Tile Cursor)
  (match cursor
    (`(,h . ,t)
     (let-values (((parent stem) (cursor-terminal+stem t document)))
       (if (isnt parent Tile?)
	   (values parent stem)
	   (let ((target (part-at h parent)))
	     (if (eq? parent target)
		 (values parent t)
		 (values target cursor))))))
    (_
     (values document cursor))))

(define (cursor-stem #!optional
		     (cursor ::Cursor (the-cursor))
		     (document (the-document)))
  ::Cursor
  (let-values (((terminal stem) (cursor-terminal+stem cursor document)))
    stem))

#;(assert
   (forall (document cursor)
     (isnt (cursor-core cursor document) fully-expanded? document)))

(define (innermost-composition #!key
			       (at::Cursor (the-cursor))
			       (in (the-document)))
  (match at
    (`(,head . ,tail)
     (let ((parent (cursor-ref in tail)))
       (if (pair? parent)
	   (let ((this (part-at head parent)))
	     (if (pair? this)
		 this
		 parent))
	   (innermost-composition at: tail in: in))))
    ('()
     (assert (pair? in))
     in)))

(define (outermost-expression #!key
			      (at::Cursor (the-cursor))
			      (in (the-document)))
  (match at
    (`(,second ,first)
     (cursor-ref in at))
    (`()
     in)
    (`(,last)
     (cursor-ref in at))
    (`(,skip . ,rest)
     (outermost-expression at: rest in: in))))

(define (first-index object)
  (cond ((Indexable? object)
 	 (let ((x :: Indexable object))
	   (x:first-index)))

	((string? object)
	 0)

	((or (pair? object) (null? object))
	 #\[)

	(else
	 (error "Don't know how to obtain first index from "
		object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (let ((x :: Indexable object))
	   (x:last-index)))

	((string? object)
	 (string-length (as string object)))

	((or (pair? object) (null? object))
	 #\])
	
	(else
	 (error "Don't know how to obtain last index from "
		object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (let ((x :: Indexable object))
	   (x:next-index index)))

	((string? object)
	 (min (last-index object) (+ index 1)))

	(else
	 (error "Don't know how to obtain next index to "
		index" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (let ((x :: Indexable object))
	   (x:previous-index index)))

	((string? object)
	 (max 0 (- index 1)))
	
	(else
	 (error "Don't know how to obtain previous index to "
		index " in "object))))

(define (cursor-next #!optional
		     (cursor::Cursor (the-cursor))
		     (expression (the-document)))
  ::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref expression tail))
	    (next (next-index head parent)))
       (if (equal? head next)
	   (cursor-next tail expression)
	   (hash-cons next tail))))
    (_
     cursor)))

(define (cursor-climb-front #!optional
			    (cursor::Cursor (the-cursor))
			    (expression (the-document)))
  ::Cursor
  (define (climb-front cursor::Cursor target)
    ::Cursor
    (let* ((index (first-index target))
	   (child (part-at index target)))
      (if (eq? child target)
	  (if (and (pair? cursor)
		   (eq? (cursor-ref expression
				    (tail
				     cursor))
			target))
	      cursor
	      (hash-cons index cursor))
	  (climb-front (hash-cons index cursor)
		       child))))
    
  (climb-front cursor
	       (cursor-ref expression cursor)))

(define (cursor-back #!optional
		     (cursor::Cursor (the-cursor))
		     (expression (the-document)))
  ::Cursor
  (match cursor
    (`(,head . ,tail)
     (let* ((parent (cursor-ref expression tail))
	    (previous (previous-index head parent)))
       (if (equal? head previous)
	   (cursor-back tail expression)
	   (hash-cons previous tail))))
    (_
     cursor)))

(define (cursor-climb-back #!optional
			   (cursor::Cursor (the-cursor))
			   (expression (the-document)))
  ::Cursor
  (define (climb-back cursor::Cursor target)::Cursor
    (let* ((index (last-index target))
	   (child (part-at index target)))
      (if (eq? child target)
	  (if (and (pair? cursor)
		   (eq? (cursor-ref expression
				    (tail
				     cursor))
			target))
	      cursor
	      (hash-cons index cursor))
	  (climb-back (hash-cons index cursor)
		      child))))
  (climb-back cursor
	      (cursor-ref expression cursor)))  


(define (cursor-advance #!optional
			(cursor::Cursor (the-cursor))
			(document (the-document)))
  ::Cursor
  (define (next cursor)
    (cursor-climb-front
     (cursor-next cursor
		  document)
     document))
  (let ((updated (next cursor)))
    (or #;(and-let* ((`(,_ ,_ . ,root) updated)
		   (parent (cursor-ref document root))
		   (target (cursor-ref document updated))
		   (limit (last-index target))
		   ((isnt limit eqv? 0))
		   ((isnt target pair?))
		   ((isnt parent empty?))
		   ((eqv? (head updated) limit)))
	  (next updated))
	updated)))

(define (cursor-retreat #!optional
			(cursor::Cursor (the-cursor))
			(document (the-document)))
  ::Cursor
  (define (next cursor)
    (cursor-climb-back
     (cursor-back cursor
		  document)
     document))
  (let ((updated (next cursor)))
    (or #;(and-let* ((`(,_ ,_ . ,root) updated)
		   (parent (cursor-ref document root))
		   (target (cursor-ref document updated))
		   (limit (first-index target))
		   ((isnt target pair?))
		   ((isnt parent null?))
		   ((isnt parent EmptyListProxy?))
		   ((eqv? (head updated) limit)))
	  (next updated))
	updated)))

(define-single-cache (selection-start+end cursor::Cursor
					  range::integer)
  (cond
   ((is range < 0)
    (list
     (iterations (- range)
		 cursor-retreat
		 cursor)
     cursor))

   ((is range > 0) 
    (list
     cursor
     (iterations range
		 cursor-advance
		 cursor)))
   (else
    (list cursor cursor))))

(define (the-selection)::(Values Cursor Cursor)
  (match (selection-start+end (the-cursor)
			      (the-selection-range))
    (`(,start ,end)
     (values start end))))

(define (within-selection? context::Cursor)::boolean
  ;; implicitly parameterized with (the-document),
  ;; (the-cursor) and (the-selection-range),
  ;; because cursor< is parameterized with (the-document),
  ;; and (the-selection) is implicitly parameterized
  ;; with (the-document), (the-cursor)
  ;; and (the-selection-range)
  (and (isnt (the-selection-range) zero?)
       (let-values (((selection-start selection-end) (the-selection)))
	 (is selection-start cursor< context cursor< selection-end))))

