(import (srfi :11))
(import (define-type))
(import (define-interface))
(import (hash-table))
(import (define-property))
(import (define-object))
(import (define-cache))
(import (keyword-arguments))
(import (fundamental))
(import (indexable))
(import (infix))
(import (examples))
(import (match))
(import (for))
(import (functions))
(import (assert))
(import (conversions))
(import (painter))
(import (extent))
(import (print))

(define (fragment-size fragment)
  (match fragment
    (,@integer?
     fragment)

    (,@Comment?
     0)
    ))

(define (space-fragment-index fragments::list index::int)
  (if (or (isnt fragments pair?)
	  (is (fragment-size (car fragments)) >= index))
      (values fragments index)
      (space-fragment-index (cdr fragments)
			    (as int
				(- index
				   (fragment-size
				    (car fragments))
				   1)))))

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
      (`(,,@(is _ > 0) . ,_)
       (set! (car cell) (as int (- (car cell) 1)))
       fragments)
      (_
       fragments
       ))))

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

(define-type (Space fragments: pair)
  implementing Element with
  ((part-at index::Index)::Indexable*
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

  ((first-index)::Index 0)
  
  ((last-index)::Index
   (fold-left (lambda (total next)
		(if (number? next)
		    (+ total next)
		    total))
	      (length (cdr fragments))
	      fragments))
  
  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))
  
  ((previous-index index::Index)::Index
   (max (first-index) (- index 1)))
   
  ((index< a::Index b::Index)::boolean
   (is (as int a) < (as int b)))

  ((insert-space! position::int)::void
   (let-values (((cell index) (space-fragment-index
			       fragments
			       position)))
     (set! (car cell) (as int (+ (car cell) 1)))))

  ((insert-break! position::int)::void
   (let-values (((cell index) (space-fragment-index
			       fragments
			       position)))
     (set! (cdr cell) (cons 0 (cdr cell)))))

  ((draw! context::Cursor)::void
   (let-values (((selection-start selection-end) (the-selection)))
     (let* ((painter ::Painter (the-painter))
	    (enters-selection-drawing-mode?::boolean
	     (and (pair? selection-start)
		  (equal? (cdr selection-start) context)
		  (integer? (car selection-start))))
	    (exits-selection-drawing-mode?::boolean
	     (and (pair? selection-end)
		  (equal? (cdr selection-end) context)
		  (integer? (car selection-end))))
	    (space-width ::real (painter:space-width))
	    (t ::Traversal (invoke (the-traversal) 'clone))
	    (left ::real t:left)
	    (top ::real t:top))
       (let skip ((input ::list fragments)
		  (total ::int 0))
	 (define (expand-with-cursor! width::real)
	   (and-let* ((`(,tip . ,sub) (the-cursor)))
	     (when (and (integer? tip)
			(equal? sub context)
			(is total <= tip <= (+ total
					       width)))
	       (painter:mark-cursor! (- t:left left
					(* space-width
					   (- total tip)))
				     (- t:top top -1)))
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
	    (parameterize ((the-traversal t))
	      (with-translation ((- t:left left) (- t:top top))
		  (comment:draw! (hash-cons (+ total 1) context))))
	    (comment:expand! t)
	    (skip rest (+ total 2)))
	   
	   (`(,width::integer ,next::integer . ,_)
	    (expand-with-cursor! width)
	    (t:new-line!)
	    (skip (cdr input) (+ total width 1)))
	   
	   (`(,width::integer . ,rest)
	    (expand-with-cursor! width)
	    (skip rest (+ total width)))
	   
	   
	   ('()
	    (values)))))))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (and-let* ((painter ::Painter (the-painter))
		(space-width ::real (painter:space-width))
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
				     (+ total 1) path))
	     (begin
	       (comment:expand! t)
	       (skip rest (+ total 2)))))
	   
	   (`(,width::integer ,next-line-prefix::integer . ,_)
	    (cond
	     ((is 0 <= (- y t:top) < t:max-line-height)
	      (hash-cons (+ total
			    (min width
				 (quotient (- x t:left)
					   space-width)))
			 path))
	     (else
	      (t:expand-by! (* space-width width))
	      (t:new-line!)
	      (skip (cdr input)
		    (as int (+ total width 1))))))
	   (`(,width::integer . ,rest)
	    (or
	     (and (is 0 <= (- y t:top) < t:max-line-height)
		  (is x < (+ t:left (* space-width width)))
		  (hash-cons (+ total
				(quotient (- x t:left)
					  space-width))
			     path))
	     (begin
	       (t:expand-by! (* space-width width))
	       (skip rest (+ total width)))))
	   
	   ('()
	    #!null))))))
  ((print out::gnu.lists.Consumer)::void
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

  implementing Expandable
  with
  ((expand! t::Traversal)::void
   (let* ((painter ::Painter (the-painter))
	  (space-width ::real (painter:space-width)))
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

  implementing Textual
  with
  ((insert-char! c::char index::int)::void
   (match (integer->char c)
     (#\space
      (insert-space! index))
     (#\newline
      (insert-break! index))))
  
  ((delete-char! index::int)::char
   (let ((result (char-ref index)))
     (delete-space-fragment! fragments index)
     result))
  
  ((char-ref index::int)::char
   (let-values (((fragment index) (space-fragment-index
				   fragments index)))
     (match fragments
       (`(,,index . ,_)
	#\newline)
       (_
	#\space))))

  ((split! position::int)::Textual
   (split-fragments! fragments position))

  ((merge! next::Textual)::boolean
   (and-let* ((next ::Space next)
	      (suffix (last-tail fragments))
	      (`(,n) suffix)
	      (`(,m . ,appendix) next:fragments))
     (set! (car suffix) (as int (+ m n)))
     (set! (cdr suffix) appendix)
     #t))
  
  ((text-length)::int
   (fold-left (lambda (x0::int f)::int
		      (+ x0 (fragment-size f)
			 (if (number? f) 1 0)))
	      (car fragments) (cdr fragments)))
  )


(define (SingleSpace)::Space
  (Space fragments: (list 1)))

(define (SingleSpace? space ::Space)::boolean
  (and-let* (((Space fragments: '(1)) space))))

(define (NewLine)::Space
  (Space fragments: (list 0 0)))

(define (NewLine? space ::Space)::boolean
  (and-let* (((Space fragments: '(0 0)) space))))

(define (EmptySpace)::Space
  (Space fragments: (list 0)))

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
  (cond
   ((is index <= (car fragments))
    (let ((reminent (cons (- (car fragments) index)
			  (cdr fragments))))
      (set! (car fragments) index)
      (set! (cdr fragments) '())
      (Space fragments: reminent)))
   (else
    (split-fragments!
     (cdr fragments)
     (as int (- index (car fragments) 1))))))


(define (split-space! space::Space index::int)::Space
  "Truncates space and returns the rest in the result"
  (split-fragments! space:fragments index))

(e.g.
 (let* ((fragments (list 3 6 9))
	(space (Space fragments: fragments))
	(rest (split-space! space 0)))
   (and (equal? space (Space fragments: '(0)))
	(equal? rest (Space fragments: '(3 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 1)))
   (and (equal? space (Space fragments: '(1)))
	(equal? rest (Space fragments: '(2 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 3)))
   (and (equal? space (Space fragments: '(3)))
	(equal? rest (Space fragments: '(0 6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 4)))
   (and (equal? space (Space fragments: '(3 0)))
	(equal? rest (Space fragments: '(6 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 5)))
   (and (equal? space (Space fragments: '(3 1)))
	(equal? rest (Space fragments: '(5 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 10)))
   (and (equal? space (Space fragments: '(3 6)))
	(equal? rest (Space fragments: '(0 9))))))

(e.g.
 (let* ((space (Space fragments: (list 3 6 9)))
	(rest (split-space! space 11)))
   (and (equal? space (Space fragments: '(3 6 0)))
	(equal? rest (Space fragments: '(9))))))

(define (skip-first-line s::Space)::Space
  (match s:fragments
    (`(,_ ,_ . ,_)
     (Space fragments: (cdr s:fragments)))
    (_
     (Space fragments: '(0)))))

;; In the following property definitions, we'd like
;; the input to be of type "cons", rather than "pair",
;; but at this point it isn't yet defined.

;; a cell is "dotted?" naturally when it is
;; a pair whose "tail" isn't a list (so for example,
;; if it's a symbol or a number).

;; But every cell can be stipulated to be "dotted?".
;; However, it is an error (currently unhandled)
;; to stipulate a cell whose tail isn't a list
;; to be not dotted, i.e. it is an error to invoke
;;
;; (set! (dotted? pair) #f)
;;
;; Instead, (unset! (dotted? pair)) should be used.

(define-property (dotted? cell::pair)::boolean
  (not (or (empty? (cdr cell))
	   (pair? (cdr cell)))))

;; `pre-head-space` appears before the first element
;; of a list, after the opening paren (therefore
;; it should be considered sparse).

(define-property+ (pre-head-space cell::pair)::Space
  (Space fragments: (cons 0 '())))

;; `post-head-space` appears after each element
;; in a list (and should therefore be considered
;; dense: expect as many `post-head-space`s as there
;; are cells visible in the document).

(define-property+ (post-head-space cell::pair)::Space
  (if (and (not (dotted? cell))
	   (empty? (cdr cell)))
      (Space fragments: (cons 0 '()))
      (Space fragments: (cons 1 '()))))

;; `pre-` and `post-tail-space` only appear
;; in the pairs that are `dotted?` (so they
;; can both be considered sparse)

(define-property+ (pre-tail-space cell::pair)::Space
  (Space fragments: (cons 1 '())))

(define-property+ (post-tail-space cell::pair)::Space
  (Space fragments: (cons 0 '())))

(define (last-space sequence::pair)::Space
  (let ((cell ::pair (last-pair sequence)))
    (if (dotted? cell)
	(post-tail-space cell)
	(post-head-space cell))))

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
   (invoke (the-painter) 'draw-horizontal-bar! width))
  (define (extent)::Extent
    (Extent width: width
	    height: (invoke (the-painter)
			    'horizontal-bar-height)))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((inner (extent)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (hash-cons (invoke (this) 'first-index) path)))))

  (HeadTailSeparator)
  (set! width width0))

(define-object (VerticalBar height0::real)::Tile

  (define height :: real 0)
  (define (draw! context::Cursor)::void
    (invoke (the-painter) 'draw-vertical-bar! height))
  
  (define (extent)::Extent
    (Extent width: (invoke (the-painter) 'vertical-bar-width)
	    height: height))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((inner (extent)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (hash-cons (invoke (this) 'first-index) path)))))

  (HeadTailSeparator)
  (set! height height0))

(define-cache (horizontal-bar width)
  (HorizontalBar (as real width)))

(define-cache (vertical-bar height)
  (VerticalBar (as real height)))

(define (should-the-bar-be-horizontal? dotted-pair)
  ::boolean
  (assert (dotted? dotted-pair))
  (and-let* (((Space fragments: `(,_ ,_ . ,_))
	      (post-head-space dotted-pair))
	     ((Space fragments: `(,_ ,_ . ,_))
	      (pre-tail-space dotted-pair)))))

(define-property (head-tail-separator cell)
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
	     (painter (the-painter))
	     (paren-width (painter:paren-width)))
	(and (is 0 <= x < outer:width)
	     (is 0 <= y < outer:height)
	     (cond ((is 0 <= x < paren-width)
		    (hash-cons (first-index) path))
		   ((is paren-width <= x < (- outer:width
					      paren-width))
		    (hash-cons 0 path))
		   ((is (- outer:width paren-width) <= x)
		    (hash-cons (last-index) path))
		   (else
		    #!null))))))
  
  (define (index< a::Index b::Index)
    (or (and (eqv? a #\[) (or (eqv? b 0)
			      (eqv? b #\])))
	(and (eqv? a 0) (eqv? b #\]))))

  (define (extent)::Extent
    (let* ((painter (the-painter))
	   (traversal (Traversal
		       max-line-height:
		       (painter:min-box-height))))
      (space:expand! traversal)
      (Extent width: (+ (* 2 (painter:paren-width))
			traversal:max-width)
	      height: (+ traversal:top traversal:max-line-height))))

  (define (draw! context::Cursor)::void
    (let ((outer (extent))
	  (painter (the-painter)))
      (painter:draw-box! outer:width outer:height context)
      (with-translation ((painter:paren-width) 0)
	  (space:draw! (hash-cons 0 context)))))

  (define (print out::gnu.lists.Consumer)::void
    (out:append #\[)
    (space:print out)
    (out:append #\]))

  (define (toString)::String
    (string-append "[" (with-output-to-string
			(lambda ()
			  (space:print (current-output-port))))
		   "]"))

  (gnu.lists.LList))

(define/kw (empty space ::Space := (EmptySpace))::EmptyListProxy
  (EmptyListProxy space))

(define cell-display-properties
  (list
   dotted?
   pre-head-space
   post-head-space
   pre-tail-space
   post-tail-space))

(define (copy-properties properties original cell)
  (for property in properties
    (update! (property cell) (property original)))
  cell)

(define (tail-space-to-head original cell)
  (update! (pre-head-space cell)
	   (pre-tail-space original))
  (update! (post-head-space cell)
	   (post-tail-space original))
  cell)

(define (head-space-to-tail original cell)
  (update! (pre-tail-space cell)
	   (pre-head-space original))
  (update! (post-tail-space cell)
	   (post-head-space original))
  cell)

(define (tree-map/preserve properties f l)
  (if (pair? l)
      (copy-properties
       properties
       l
       (cons
	(tree-map/preserve properties f (car l))
	(tree-map/preserve properties f (cdr l))))
      (f l)))

(define (print-space space::Space
		     #!optional (port (current-output-port)))
  #;(write space:fragments port)
  (space:print port))

(define (show-empty-list space)::void
  (write-char #\()
  (print-space space)
  (write-char #\)))

(define (show-dotted-tail p::pair)::void
  (write-char #\.)
  (print-space (pre-tail-space p))
  (show (cdr p))
  (print-space (post-tail-space p)))

(define (show-pair p::pair)::void
  (show (car p))
  (print-space (post-head-space p))
  (cond ((dotted? p)
	 (show-dotted-tail p))
	((pair? (cdr p))
	 (show-pair (cdr p)))))

(define (show p)::void
  (cond
   ((pair? p)
    (write-char #\()
    (print-space (pre-head-space p))
    (show-pair p)
    (write-char #\)))
   
   ((EmptyListProxy? p)
    (invoke (as EmptyListProxy p) 'print (current-output-port)))
   
   (else
    (write p))))

(define (show-document d::pair)
  (cond
   ((empty? (car d))
    (let ((proxy (as EmptyListProxy (car d))))
      (proxy:space:print (current-output-port))))
   ((pair? (car d))
    (print-space (pre-head-space (car d)))
    (show-pair (car d)))
   
   (else
    (display (car d)))))

(define (show->string p)::string
  (with-output-to-string
    (lambda ()
      (show p))))
