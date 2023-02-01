(import (srfi :11))
(import (define-syntax-rule))
(import (define-interface))
(import (define-type))
(import (define-cache))
(import (define-property))
(import (default-value))
(import (define-parameter))
(import (keyword-arguments))
(import (infix))
(import (fundamental))
(import (indexable))
(import (match))
(import (space))
(import (assert))
(import (functions))
(import (print))

;; See the `fundamental.scm` file for a detailed explanation
;; how cursors are represented

(define (cell-index cell::pair index::int)::Indexable*
  (assert (is index >= 0))
  (cond ((= index 0)
         (pre-head-space cell))
        ((= index 1)
	 (car cell))
        ((= index 2)
         (post-head-space cell))
        ((dotted? cell)
         (cond ((= index 3)
                (head-tail-separator cell))
               ((= index 4)
                (pre-tail-space cell))
               ((= index 5)
		(cdr cell))
               ((= index 6)
                (post-tail-space cell))))
        (else
         (cell-index (cdr cell) (- index 2)))))

(define (set-cell-index! cell::pair index::int value)
  (assert (is index >= 0))
  (cond ((= index 0)
         (set! (pre-head-space cell) value))
        ((= index 1)
	 (if (is value empty?)
	     (set! (car cell) '())
	     (set! (car cell) value)))
        ((= index 2)
         (set! (post-head-space cell) value))
        ((dotted? cell)
         (cond ((= index 3)
                (set! (head-tail-separator cell) value))
               ((= index 4)
                (set! (pre-tail-space cell) value))
               ((= index 5)
		(if (is value instance? EmptyListProxy)
		    (set! (car cell) '())
		    (set! (cdr cell) value)))
               ((= index 6)
                (set! (post-tail-space cell) value))))
        (else
         (set-cell-index! (cdr cell) (- index 2) value))))

(set! (setter cell-index) set-cell-index!)

(define (last-cell-index cell::list
			 #!optional
			 (initial::int 2))
  ::int
  (cond
   ((null? cell) 0)
   
   ((dotted? cell)
    (+ initial 4))
   ((pair? (tail cell))
    (last-cell-index (tail cell)
		     (+ initial 2)))
   
   (else
    initial)))

(define (part-at index::Index object)::Indexable*
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'part-at index))

	((pair? object)
	 (if (or (eq? index #\[) (eq? index #\]))
	     object
	     (cell-index object (as int index))))
	
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
	 (invoke (as Indexable object)
		 'first-index))

	((string? object)
	 0)

	((or (pair? object) (null? object))
	 #\[)

	(else
	 (error "Don't know how to obtain first index from "
		object))))

(define (last-index object)
  (cond ((Indexable? object)
	 (invoke (as Indexable object) 'last-index))

	((string? object)
	 (string-length (as string object)))

	((or (pair? object) (null? object))
	 #\])
	
	(else
	 (error "Don't know how to obtain last index from "
		object))))

(define (next-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'next-index index))

	((string? object)
	 (min (last-index object) (+ index 1)))

	((or (pair? object) (null? object))
	 (match index
	   (#\[ 0)
	   (#\] #\])
	   (,@(is _ < (last-cell-index object))
	    (+ index 1))
	   (_
	    #\])))
	
	(else
	 (error "Don't know how to obtain next index to "
		index" in "object))))

(define (previous-index index::Index object)::Index
  (cond ((Indexable? object)
	 (invoke (as Indexable object)
		 'previous-index index))

	((string? object)
	 (max 0 (- index 1)))
	
	((or (pair? object) (null? object))
	 (match index
	   (0 #\[)
	   (#\] (last-cell-index object))
	   (#\[ #\[)
	   (_ (- index 1))))
	
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

(define/kw (space-preceding cursor::Cursor
			    in: document := (the-document))
  ::Space
  (match cursor
    (`(,tip ,top . ,root)
     (let* ((grandpa (cursor-ref document root))
	    (parent (part-at top grandpa))
	    (target (part-at tip parent)))
       (cond ((Space? target)
	      target)
	     ((and (eq? target parent)
		   (integer? top)
		   (pair? grandpa))
	      (part-at (- top 1) grandpa))
	     ((and (pair? parent)
		   (integer? tip))
	      (part-at (- tip 1) parent)))))))
