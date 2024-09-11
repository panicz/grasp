(module-name (editor types spaces))

(import (srfi :11))
(import (language assert))
(import (language define-type))
(import (language define-interface))
(import (language attributes))
(import (language define-object))
(import (language define-cache))
(import (language keyword-arguments))
(import (language fundamental))
(import (language infix))
(import (language examples))
(import (language match))
(import (language for))

(import (utils functions))
(import (utils hash-table))
(import (utils print))
(import (utils conversions))

(import (editor interfaces elements))
(import (editor interfaces painting))
(import (editor document cursor))

(define (fragment-size fragment)
  (match fragment
    (,@integer?
     fragment)

    (,@Comment?
     0)
    ))

(define (space-fragment-index fragments::list index::int)
  (cond
   ((zero? index)
    (values fragments index))
   
   ((and (pair? fragments)
	 (isnt (car fragments) integer?))
    (space-fragment-index (cdr fragments) (- index 1)))

    ((or (isnt fragments pair?)
	(is (fragment-size (car fragments)) >= index))
     (values fragments index))
    
   (else
    (space-fragment-index (cdr fragments)
			  (as int
			      (- index
				 (fragment-size
				  (car fragments))
				 1))))))

;; in the following examples the symbol 'comment' stands for
;; comments, because the functions defined in this module
;; treat everything that is not a number as a comment - but
;; in practice, a comment must implement the Comment interface
;; from the (editor interfaces elements) module

(e.g.
 (space-fragment-index '(comment 0 0) 0)
 ===> (comment 0 0) 0)

(e.g.
 (space-fragment-index '(comment 0 0) 1)
 ===> (0 0) 0)

(e.g.
 (space-fragment-index '(0 0) 0)
 ===> (0 0) 0)

(e.g.
 (space-fragment-index '(0 0) 1)
 ===> (0) 0)

(e.g.
 (space-fragment-index '(1 0) 0)
 ===> (1 0) 0)

(e.g.
 (space-fragment-index '(1 0) 1)
 ===> (1 0) 1)

(e.g.
 (space-fragment-index '(1 0) 2)
 ===> (0) 0)

(e.g.
 (space-fragment-index '(3 5) 0)
 ===> (3 5) 0)

(e.g.
 (space-fragment-index '(3 5) 1)
 ===> (3 5) 1)

(e.g.
 (space-fragment-index '(3 5) 2)
 ===> (3 5) 2)

(e.g.
 (space-fragment-index '(3 5) 3)
 ===> (3 5) 3)

(e.g.
 (space-fragment-index '(3 5) 4)
 ===> (5) 0)

(e.g.
 (space-fragment-index '(3 5) 5)
 ===> (5) 1)

(e.g.
 (space-fragment-index '(3 5) 6)
 ===> (5) 2)

(e.g.
 (space-fragment-index '(3 5) 7)
 ===> (5) 3)

(e.g.
 (space-fragment-index '(3 5) 8)
 ===> (5) 4)

(e.g.
 (space-fragment-index '(3 5) 9)
 ===> (5) 5)

(e.g.
 (space-fragment-index '(3 5) 10)
 ===> () 0)

(define (delete-space-fragment! fragments::pair
				position::int)
  ::pair
  (let-values (((cell index) (space-fragment-index
			      fragments
			      position)))
    (match cell
      (`(,,index ,next . ,rest)
       (set! (car cell) (as int (+ index next)))
       (set! (cdr cell) rest)
       fragments)
      
      (`(,,@(isnt _ integer?) ,next . ,rest)
       (set! (car cell) next)
       (set! (cdr cell) rest)
       fragments)
      
      (`(,,@(is _ > 0) . ,_)
       (set! (car cell) (as int (- (car cell) 1)))
       fragments)
      (_
       fragments)
      )))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 0)))
   (and (equal? result '(0 2 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 1)))
   (and (equal? result '(3 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 2)))
   (and (equal? result '(1 1 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 3)))
   (and (equal? result '(1 1 3))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 4)))
   (and (equal? result '(1 5))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 5)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 6)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(e.g.
 (let* ((fragments (list 1 2 3))
	(result (delete-space-fragment! fragments 7)))
   (and (equal? result '(1 2 2))
	(eq? result fragments))))

(define-object (Space fragments ::pair)::ExpandableTextualElement
  
  (define (clone)::Element
    (Space (copy fragments)))
  
  (define (equals other::java.lang.Object)::boolean
    (and-let* (((Space fragments: ,fragments) other))))
  
  (define (part-at index::Index)::Indexable*
   (try-catch
    (let-values (((fragments* index*) (space-fragment-index
				       fragments index)))
      (if (or (empty? fragments*)
	      (number? (car fragments*)))
	  (this)
	  (car fragments*)))
    (ex java.lang.Throwable
	(WARN"fragments: "fragments", index "index)
	#!null)))

  (define (first-index)::Index 0)

  (define (last-index)::Index
   (fold-left (lambda (total next)
		(if (number? next)
		    (+ total next)
		    total))
	      (length (cdr fragments))
	      fragments))

  (define (next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  (define (previous-index index::Index)::Index
   (max (first-index) (- index 1)))

  (define (index< a::Index b::Index)::boolean
   (is (as int a) < (as int b)))

  (define (insert-space! position::int)::void
   (let-values (((cell index) (space-fragment-index
			       fragments
			       position)))
     (set! (car cell) (as int (+ (car cell) 1)))))

  (define (insert-break! position::int)::void
    (if (= position 0)
	(set! fragments (cons 0 fragments))
	(let-values (((cell index) (space-fragment-index
				    fragments
				    position)))
	  (set! (cdr cell) (cons 0 (cdr cell))))))

  (define (draw! context::Cursor)::void
   (let-values (((selection-start selection-end) (the-selection)))
     (let* ((enters-selection-drawing-mode?::boolean
	     (and (pair? selection-start)
		  (equal? (cdr selection-start) context)
		  (integer? (car selection-start))))
	    (exits-selection-drawing-mode?::boolean
	     (and (pair? selection-end)
		  (equal? (cdr selection-end) context)
		  (integer? (car selection-end))))
	    (space-width ::real (painter:space-width))
	    (t0 ::Traversal (the-traversal))
	    (t ::Traversal (t0:clone))
	    (left ::real t:left)
	    (top ::real t:top))
       (parameterize ((the-traversal t))
	 (let skip ((input ::list fragments)
		    (total ::int 0))
	   (define (expand-with-cursor! width::real)
	     (and-let* ((`(,tip . ,sub) (the-cursor)))
	       (when (and (integer? tip)
			  (equal? sub context)
			  (is total <= tip <= (+ total
						 width)))
		 (with-translation ((- t:left left
				       (* space-width
					  (- total tip))) (- t:top top -1))

		   (set! t:parent-left (+ t:parent-left t:left
					  (* space-width
					     (- tip total))))
		   (set! t:parent-top (+ t:parent-top t:top))
		   (painter:mark-cursor! 0 0)
		   (set! t:parent-left (- t:parent-left t:left
					  (* space-width
					     (- tip total))))
		   (set! t:parent-top (- t:parent-top t:top))
		   ))
	       (when (and enters-selection-drawing-mode?
			  (is total <= (car
					selection-start) <= (+ total
							       width)))
		 (painter:enter-selection-drawing-mode!))
	       (when (and exits-selection-drawing-mode?
			  (is total <= (car
					selection-end) <= (+ total
							     width)))
		 (painter:exit-selection-drawing-mode!)))
	     (t:expand-by! (* width space-width)))

	   (match input
	     (`(,comment::Comment . ,rest)
	      (with-translation ((- t:left left) (- t:top top))
		(comment:draw! (hash-cons (+ total 1) context)))
	      (comment:expand! t)
	      (when (comment:breaks-line?)
		(t:on-end-line #t))
	      (skip rest (+ total 2)))

	     (`(,width::integer ,next::integer . ,_)
	      (expand-with-cursor! width)
	      (t:on-end-line #t)
	      (t:new-line!)
	      (skip (cdr input) (+ total width 1)))

	     (`(,width::integer . ,rest)
	      (expand-with-cursor! width)
	      (skip rest (+ total width)))

	     ('()
	      (set! t0:on-end-line t:on-end-line))))))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (and-let* ((space-width ::real (painter:space-width))
		(traversal ::Traversal (the-traversal))
		(t ::Traversal (traversal:clone))
		;; we need to restore the coordinates to
		;; the-traverse's reference frame
		(x ::real (+ x t:left))
		(y ::real (+ y t:top)))
       (let skip ((input ::list fragments)
		  (total ::int 0))
	 (match input
	   (`(,comment::Comment . ,rest)
	    (or
	     (comment:cursor-under* (- x t:left)
				    (- y t:top)
				    (hash-cons*
				     (as int (+ total 1))
				     path))
	     (begin
	       (comment:expand! t)
	       (skip rest (+ total 2)))))

	   (`(,width::integer ,next-line-prefix::integer . ,_)
	    (cond
	     ((is 0 <= (- y t:top) < t:max-line-height)
	      (hash-cons (as int (+ total
				    (min width
					 (quotient
					  (max 0 (- x t:left))
					  space-width))))
			 path))
	     (else
	      (t:expand-by! (* space-width width))
	      (unless (eq? t:on-end-line nothing)
		(WARN "overridden on-end-line invoked from cursor-under in"
		      (this)))
	      (t:new-line!)
	      (skip (cdr input)
		    (as int (+ total width 1))))))
	   (`(,width::integer . ,rest)
	    (or
	     (and (is 0 <= (- y t:top) < t:max-line-height)
		  (is x < (+ t:left (* space-width width)))
		  (hash-cons (as int (+ total
					(quotient
					 (max 0 (- x t:left))
					 space-width)))
			     path))
	     (begin
	       (t:expand-by! (* space-width width))
	       (skip rest (+ total width)))))

	   ('()
	    #!null))))))
  (define (print out::gnu.lists.Consumer)::void
   (let process ((input fragments))
     (match input
       (`(,width::integer ,next-line-prefix::integer . ,_)
	(for n from 0 below width
	     (out:append #\space))
	(out:append #\newline)
	(process (cdr input)))

       (`(,width::integer . ,rest)
	(for n from 0 below width
	     (out:append #\space))
	(process rest))

       (`(,comment::Comment . ,rest)
	(comment:print out)
	(process rest))

       (_
	(values)))))

  (define (expand! t::Traversal)::void
   (let* ((space-width ::real (painter:space-width)))
     (let skip ((input ::list fragments)
		(total ::int 0))
       (match input
	 (`(,comment::Comment . ,rest)
	  (comment:expand! t)
	  (skip rest (+ total 2)))

	 (`(,width::integer ,next-line-prefix::integer . ,_)
	  (t:expand-by! (* space-width width))
	  (t:new-line!)
	  (skip (cdr input) (+ total width 1)))

	 (`(,width::integer . ,rest)
	  (t:expand-by! (* space-width width))
	  (skip rest (+ total width)))

	 ('()
	  (values))))))

  (define (insert-char! c::char index::int)::void
   (match (integer->char c)
     (#\space
      (insert-space! index))
     (#\newline
      (insert-break! index))))

  (define (delete-char! index::int)::char
   (let ((result (char-ref index)))
     (delete-space-fragment! fragments index)
     result))

  (define (char-ref index::int)::char
   (let-values (((fragment index) (space-fragment-index
				   fragments index)))
     (match fragments
       (`(,,index ,,@integer? . ,_)
	#\newline)
       (_
	#\space))))

  (define (split! position::int)::Textual
   (split-fragments! fragments position))

  (define (merge! next::Textual)::boolean
   (and-let* ((next ::Space next)
	      (suffix (last-tail fragments))
	      (`(,n) suffix)
	      (`(,m . ,appendix) next:fragments))
     (set! (car suffix) (as int (+ m n)))
     (set! (cdr suffix) appendix)
     #t))

  (define (text-length)::int
   (fold-left (lambda (x0::int f)::int
		      (+ x0 (fragment-size f)
			 (if (number? f) 1 0)))
	      (car fragments) (cdr fragments)))
  )


(define (SingleSpace)::Space
  (Space (list 1)))

(define (SingleSpace? space ::Space)::boolean
  (and-let* (((Space fragments: '(1)) space))))

(define (NewLine)::Space
  (Space (list 0 0)))

(define (NewLine? space ::Space)::boolean
  (and-let* (((Space fragments: '(0 0)) space))))

(define (EmptySpace)::Space
  (Space (list 0)))

(define (EmptySpace? space ::Space)::boolean
  (and-let* (((Space fragments: '(0)) space))))

(define (SpaceFrom whitespace ::gnu.text.Char)::Space
  (match whitespace
    (#\space   (SingleSpace))
    (#\newline (NewLine))))

(define (insert-space! space::Space position::int)
  (space:insert-space! position))

(define (insert-break! space::Space position::int)
  (space:insert-break! position))

(define (insert-whitespace! c::char space::Space
			    position::int)
  (assert (char-whitespace? c))
  (if (eq? c #\newline)
      (insert-break! space position)
      (insert-space! space position)))

(define (join-spaces! a::Space b::Space)::Space
  (let ((suffix (last-pair a:fragments)))
    (set! (car suffix)
      (as int (+ (car suffix) (car b:fragments))))
    (set! (cdr suffix) (cdr b:fragments))
    (set! b:fragments (cons 0 '()))
    a))

(define (split-fragments! fragments::pair
			  index::int)
  ::Space
  (match fragments
    (`(,first . ,rest)
     (cond
      ((is index <= first)
       (let ((reminent (cons (- first index) rest)))
	 (set! (car fragments) index)
	 (set! (cdr fragments) '())
	 (Space reminent)))
      
      ((and-let* ((`(,next . ,rest*) rest)
		  ((isnt next integer?)))
	 (cond
	  ((= index (+ first 1))
	   (set! (cdr fragments) '())
	   (Space rest))
	  (else
	   (split-fragments!
	    rest*
	    (as int (- index first 1)))))))
      (else
       (split-fragments!
	rest
	(as int (- index first 1))))))))


(define (split-space! space::Space index::int)::Space
  "Truncates space and returns the rest in the result"
  (split-fragments! space:fragments index))

(e.g.
 (let* ((fragments (list 3 6 9))
	(space (Space fragments))
	(rest (split-space! space 0)))
   (and (equal? space (Space '(0)))
	(equal? rest (Space '(3 6 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 1)))
   (and (equal? space (Space '(1)))
	(equal? rest (Space '(2 6 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 3)))
   (and (equal? space (Space '(3)))
	(equal? rest (Space '(0 6 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 4)))
   (and (equal? space (Space '(3 0)))
	(equal? rest (Space '(6 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 5)))
   (and (equal? space (Space '(3 1)))
	(equal? rest (Space '(5 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 10)))
   (and (equal? space (Space '(3 6)))
	(equal? rest (Space '(0 9))))))

(e.g.
 (let* ((space (Space (list 3 6 9)))
	(rest (split-space! space 11)))
   (and (equal? space (Space '(3 6 0)))
	(equal? rest (Space '(9))))))

(define (skip-first-line s::Space)::Space
  (match s:fragments
    (`(,,@integer? ,,@integer? . ,_)
     (Space (cdr s:fragments)))
    (_
     (Space '(0)))))

(define-object (HeadTailSeparator)::Indexable
  (define (part-at index::Index)::Indexable* (this))
  (define (first-index)::Index #\|)
  (define (last-index)::Index #\|)
  (define (next-index index::Index)::Index #\|)
  (define (previous-index index::Index)::Index #\|)
  (define (index< a::Index b::Index) #f)

  (define (toString)::String "|"))

(define-constant head/tail-separator
  (HeadTailSeparator))

(define (head/tail-separator? x)
  (instance? x HeadTailSeparator))

(define-object (HorizontalBar width0::real)::Tile

  (define width :: real 0)
  
  (define (draw! context::Cursor)::void
    (painter:draw-horizontal-bar!
     width
     (and-let* ((`(#\| . ,,context) (the-cursor))))))
  
  (define (extent)::Extent
    (Extent width: width
	    height: (painter:horizontal-bar-height)))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((inner (extent)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (hash-cons (invoke (this) 'first-index) path)))))

  (HeadTailSeparator)
  (set! width width0))

(define-attribute+ (horizontal-bar width)
  (HorizontalBar (as real width)))

(define-object (VerticalBar height0::real)::Tile

  (define height :: real 0)
  
  (define (draw! context::Cursor)::void
    (painter:draw-vertical-bar!
     height
     (and-let* ((`(#\| . ,,context) (the-cursor))))))

  (define (extent)::Extent
    (Extent width: (painter:vertical-bar-width)
	    height: height))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((inner (extent)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (hash-cons (invoke (this) 'first-index) path)))))

  (HeadTailSeparator)
  (set! height height0))

(define-attribute+ (vertical-bar height)
    (VerticalBar (as real height)))

(define-attribute (head-tail-separator cell)
  head/tail-separator)

(define (empty? x)::boolean
  (and (is x gnu.lists.LList?)
       (isnt x gnu.lists.Pair?)))

(define-object (EmptyListProxy space::Space)::ShadowedTile

  (define (value) '())

  (define (hashCode)::int
    (java.lang.System:identityHashCode (this)))

  (define (part-at index::Index)::Indexable*
    (match index
      (0 space)
      (_ (this))))

  (define (first-index)::Index #\[)

  (define (last-index)::Index #\])

  (define (next-index index::Index)::Index
    (match index
      (#\[ 0)
      (_ #\])))

  (define (previous-index index::Index)::Index
    (match index
      (#\] 0)
      (_ #\[)))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let* ((outer (extent))
	     (paren-width (painter:paren-width)))
	(and (is 0 <= x < outer:width)
	     (is 0 <= y < outer:height)
	     (cond ((is 0 <= x < paren-width)
		    (hash-cons (first-index) path))
		   ((is paren-width <= x < (- outer:width
					      paren-width))
		    (hash-cons (as int 0) path))
		   ((is (- outer:width paren-width) <= x)
		    (hash-cons (last-index) path))
		   (else
		    #!null))))))

  (define (index< a::Index b::Index)
    (or (and (eqv? a #\[) (or (eqv? b 0)
			      (eqv? b #\])))
	(and (eqv? a 0) (eqv? b #\]))))

  (define (extent)::Extent
    (let* ((traversal (Traversal
		       max-line-height:
		       (painter:min-box-height))))
      (space:expand! traversal)
      (Extent width: (+ (* 2 (painter:paren-width))
			traversal:max-width)
	      height: (+ traversal:top traversal:max-line-height))))

  (define (draw! context::Cursor)::void
    (let ((outer (extent)))
      (painter:draw-box! outer:width outer:height context)
      (with-translation ((painter:paren-width) 0)
	  (space:draw! (hash-cons (as int 0) context)))))

  (define (clone)::Element
    (EmptyListProxy (space:clone)))
  
  (define (print out::gnu.lists.Consumer)::void
    (out:append #\()
    (space:print out)
    (out:append #\)))

  (gnu.lists.LList))

(define/kw (empty space ::Space := (EmptySpace))::EmptyListProxy
  (EmptyListProxy space))

(define-early-constant Empty ::EmptyListProxy (empty))

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


