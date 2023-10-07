(module-name (editor types primitive))

(import (srfi :11))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-property))
(import (language define-cache))
(import (language define-parameter))
(import (language keyword-arguments))
(import (utils hash-table))
(import (language match))
(import (language examples))
(import (language infix))
(import (language fundamental))
(import (editor interfaces elements))

(import (editor types spaces))
(import (editor document cursor))
(import (language for))
(import (editor interfaces painting))
(import (utils functions))
(import (utils print))
(import (utils conversions))
(import (editor types texts))

(define-property+ (id x)::int
  (override-size id))

;; we override Pair with Object's default equality and hash functions
;; (TODO: patch the Kawa implementation of Cons)

;;  0 1 2 3 4 5 6

;; ( car  .  cdr )

;;  0 1 2  3 4  5  6 7 8  9  10
;; ( car cadr caddr  .  cdddr  )

;;  0 1 2
;; (  x  )


;; The two modes of operation are "editing mode"
;; and "evaluating mode". The difference is in the
;; treatment of Shadowed elements: when we are (evaluating?),
;; Shadowed elements behave as if they were transparent,
;; so that we can only see their (value).
;; But if we're not (evaluating?), then we can
;; see (and operate on) Shadowed elements  themselves.

(define-enum CellAccessMode (Editing Evaluating))

(define-parameter (cell-access-mode) ::CellAccessMode
  CellAccessMode:Editing)

(define-syntax-rule (with-eval-access actions ...)
  (parameterize ((cell-access-mode CellAccessMode:Evaluating))
    actions ...))

(define (evaluating?) ::boolean
  (eq? (cell-access-mode) CellAccessMode:Evaluating))

(define (editing?) ::boolean
  (eq? (cell-access-mode) CellAccessMode:Editing))

(define-property+ (screen-position element #|::Element|#)::Position
  (Position))

;; The purpose of Atoms is to solve the problem that
;; the actual atomic Scheme values have different
;; types (e.g. the result of reading "1" is a number,
;; but the result of reading "1+" is a symbol, and
;; the result of reading "1+:" is a keyword).
;; Atoms therefore provide an identity for the
;; edited objects, even though the "value" of
;; those atoms can be a different kind of object
;; on every query.
(define-interface MatchableShadowedTextualTile (Matchable ShadowedTextualTile))

(define-object (Atom name::String)::MatchableShadowedTextualTile

  (define builder :: java.lang.StringBuilder)

  (define cache #!null)

  (define (value)
    (or cache
	(let ((result (call-with-input-string name read)))
	  (set! cache result)
	  result)))

  (define (draw! context::Cursor)
    ::void
    (let ((painter ::Painter (the-painter)))
      (painter:draw-atom!
       name
       ;;(string-append name "/" (number->string (id (this))))
       context)))

  (define (extent)::Extent
    (let ((painter ::Painter (the-painter)))
      (painter:atom-extent
       name
       ;;(string-append name "/" (number->string (id (this))))
       )))

  (define (part-at index::Index)::Indexable*
    (this))

  (define (first-index)::Index
    0)

  (define (last-index)::Index
    (string-length name))

  (define (next-index index::Index)::Index
    (min (last-index) (+ index 1)))

  (define (previous-index index::Index)::Index
    (max 0 (- index 1)))

  (define (index< a::Index b::Index)::boolean
    (and (number? a) (number? b)
	 (is a < b)))

  (define (insert-char! c::char index::int)::void
    (builder:insert index c)
    (set! cache #!null)
    (let ((s ::String (builder:toString)))
      (set! name (s:intern))))

  (define (delete-char! index::int)::char
    (let ((result (builder:charAt index)))
      (builder:deleteCharAt index)
      (set! cache #!null)
      (let ((s ::String (builder:toString)))
	(set! name (s:intern)))
      result))

  (define (char-ref index::int)::char
    (builder:charAt index))

  (define (split! position::int)::Textual
    (let ((reminent ::Atom (Atom (name:substring position))))
      (builder:setLength position)
      (set! cache #!null)
      (let ((s ::String (builder:toString)))
	(set! name (s:intern)))
      reminent))

  (define (merge! next::Textual)::boolean
    (and-let* ((next ::Atom next))
      (builder:append next:builder)
      (set! cache #!null)
      (let ((s ::String (builder:toString)))
	(set! name (s:intern)))
      #t))

  (define (text-length)::int
    (builder:length))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((inner (extent))
	    (painter (the-painter)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (recons (painter:atom-character-index-under x y
							 name)
		     path)
	     ))))

  (define (matches? x)::boolean
    (or
     (and-let* ((atom ::Atom x))
       (string=? name atom:name))
     (and-let* ((s ::gnu.mapping.Symbol x))
       (string=? name (s:getName)))
     (and (eq? x #f)
	  (or (name:equalsIgnoreCase "#f")
	      (name:equalsIgnoreCase "#false")))
     (and (eq? x #t)
	  (or (name:equalsIgnoreCase "#t")
	      (name:equalsIgnoreCase "#true")))
     (and (number? x)
	  (name:equals (number->string x)))))

  (define (toString)::String
    name
    ;;(string-append name "/" (number->string (id (this))))
    )

  (define (clone)::Element
    (Atom name))
  
  (set! builder (java.lang.StringBuilder name)))


(define-interface MatchableTile (Matchable Tile))

(define-object (cons car cdr)::MatchableTile

  (define (matches? x)::boolean
    (and-let* ((`(,h . ,t) x)
	       ((match/equal? car h))
	       ((match/equal? cdr t)))))
  
  (define (equals object)::boolean
   (eq? object (this)))

  (define (hashCode)::int
    (java.lang.System:identityHashCode (this)))

  (define (draw! context::Cursor)
    ::void
    (let* ((inner (sequence-extent (this)))
	   (painter (the-painter))
	   (paren-width (painter:paren-width)))
      (painter:draw-box! (+ inner:width (* 2 paren-width))
			 inner:height
			 context)
      (with-translation (paren-width 0)
	  (draw-sequence! (this) context: context))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let* ((inner (sequence-extent (this)))
	     (painter ::Painter (the-painter))
	     (paren-width (painter:paren-width)))

      (and (is 0 <= y < inner:height)
	   (or (and (is 0 <= x < paren-width)
		    (recons (first-index) path))

	       (and (is 0 <= (- x paren-width) < inner:width)
		    (cursor-under (- x paren-width) y
				  (this) context: path))
	       (and (is 0 <= (- x paren-width inner:width)
			< paren-width)
		    (recons (last-index) path)))))))

  (define (extent)::Extent
    (let ((extent ::Extent (sequence-extent
			    (this)))
	  (painter ::Painter (the-painter)))
      (Extent width: (+ extent:width
			(* 2 (painter:paren-width)))
	      height: extent:height)))

  (define (part-at index::Index)::Indexable*
    (if (or (eq? index (first-index))
	    (eq? index (last-index)))
	(this)
	(cell-index (this) (as int index))))

  (define (first-index)::Index
    #\[)

  (define (last-index)::Index
    #\])

  (define (next-index index::Index)::Index
    (match index
      (,(first-index) 0)
      (,(last-index) (last-index))
      (,@(is _ < (last-cell-index (this)))
       (+ index 1))
      (_
       (last-index))))

  (define (previous-index index::Index)::Index
    (match index
      (0 (first-index))
      (,(last-index) (last-cell-index (this)))
      (,(first-index) (first-index))
      (_ (- index 1))))

  (define (index< a::Index b::Index)::boolean
    (or (and (is a eqv? (first-index))
	     (isnt b eqv? (first-index)))
	(and (number? a) (number? b)
	     (is a < b))
	(and (is b eqv? (last-index))
	     (isnt a eqv? (last-index)))))

  (define (getCar)
    (let ((element (invoke-special pair (this) 'getCar)))
      (if (and (evaluating?)
	       (is element Shadowed?))
	  (let ((shadowed ::Shadowed element))
	    (shadowed:value))
	  element)))

  (define (getCdr)
    (let ((element (invoke-special pair (this) 'getCdr)))
      (if (and (evaluating?)
	       (is element Shadowed?))
	  (let ((shadowed ::Shadowed element))
	    (shadowed:value))
	  element)))

  (define (clone)::Element
    (cons car cdr))
  
  (pair car cdr))

(define-object (immutable-cons car cdr)::Tile

  (define (setCar value)
    (error "The cons cell is immutable: " (this)))

  (define (setCdr value)
    (error "The cons cell is immutable: "(this)))

  (cons car cdr))

(define-cache (recons head tail)
  (immutable-cons head tail))

(define-syntax cons*
  (syntax-rules ()
    ((_ a b)
     (cons a b))

    ((_ a b c ...)
     (cons a (cons* b c ...)))))

(define-syntax recons*
  (syntax-rules ()
    ((_ a b)
     (recons a b))

    ((_ a b c ...)
     (recons a (recons* b c ...)))))

(define-syntax cursor
  (syntax-rules ()
    ((_) '())
    ((_ indices ...)
     (recons* indices ... '()))))

(define (empty-space-extent space::Space)
  ::Extent
  (let ((painter ::Painter (the-painter)))
    (Extent width: (apply max space:fragments)
	    height: (* (painter:min-box-height)
		       (length space:fragments)))))

(define/kw (traverse sequence::list
		     doing: action ::(maps (Element Traversal)
					   to: void)
		     := nothing
		     returning: result ::(maps (Traversal) to: ,a)
		     := nothing)
  ;; ::,a
  (let* ((painter (the-painter))
         (traversal (Traversal
		     max-line-height:
		     (painter:min-line-height))))

    (parameterize ((the-traversal traversal))

      (define (step-over-dotted-tail! pair::pair)::void
	(let* ((horizontal? (should-the-bar-be-horizontal? pair))
               (bar ::Element (if horizontal?
				  (horizontal-bar
				   traversal:max-width)
				  (vertical-bar
				   traversal:max-line-height)))
               (pre-tail ::Space (if horizontal?
				     (skip-first-line
				      (pre-tail-space pair))
				     (pre-tail-space pair)))
               (item ::Element (tail pair))
	       (post-tail ::Space (post-tail-space pair)))
          (action bar traversal)
          (traversal:advance! bar)
	  (when horizontal?
	    (set! traversal:left 0))
	  (action pre-tail traversal)
          (traversal:advance! pre-tail)
          (action item traversal)
          (traversal:advance! item)
          (action post-tail traversal)
          (traversal:advance! post-tail)))

      (define (step! pair::pair)
	(let ((item ::Element (head pair))
              (post-head ::Space (post-head-space pair)))
          (action item traversal)
          (traversal:advance! item)
          (action post-head traversal)
          (traversal:advance! post-head)
          (cond ((dotted? pair)
		 (step-over-dotted-tail! pair)
		 (result traversal))
		((pair? (tail pair))
		 (step! (tail pair)))
		(else
		 (result traversal)))))

      (if (pair? sequence)
	  (let ((pre-head ::Space (pre-head-space sequence)))
            (action pre-head traversal)
            (traversal:advance! pre-head)
            (step! sequence))
	  (result traversal))
      )))

(define (draw-sequence! #!optional
			(elems::list (head (the-document)))
			#!key (context::Cursor (recons 1 '())))
  ::void
  (let-values (((selection-start selection-end) (the-selection)))
    (let ((painter ::Painter (the-painter)))
      (traverse
       elems
       doing:
       (lambda (item::Element traversal::Traversal)
	 (with-translation (traversal:left
			    traversal:top)
	     (unless (is item instance? Space)
	       (let ((position ::Position (screen-position item)))
		 (set! position:left
		       (painter:current-translation-left))
		 (set! position:top
		       (painter:current-translation-top))))
	     (let ((context (recons traversal:index
				    context)))
	       (when (equal? context selection-start)
		 (painter:enter-selection-drawing-mode!))
	       (item:draw! context)
	       (when (equal? context selection-end)
		 (painter:exit-selection-drawing-mode!)))))))))


(define (draw! object #!key
	      (context::Cursor '()))
  ::void
  (let ((painter (the-painter)))
    (cond ((instance? object Element)
	   (let ((element ::Element object))
	     (element:draw! context)))

	  ((null? object)
	   (values))

	  (else
	   (with-translation (0 1)
	       (painter:draw-string!
		(with-output-to-string
		  (lambda () (write object)))
		(otherwise #!null
		  (and (pair? (the-cursor))
		       (equal? (cdr (the-cursor)) context)
		       (car (the-cursor))))))))))

(define (cursor-under left::real top::real
		      #!optional
		      (elems::list (head (the-document)))
		      #!key (context::Cursor (recons 1 '())))
  ::Cursor
  (call/cc
   (lambda (return)
     (traverse
      elems
      doing:
      (lambda (item::Element t::Traversal)
	(and-let* ((cursor (item:cursor-under*
			    (- left t:left)
			    (- top t:top)
			    (recons t:index context))))
	  (return cursor)))
      returning: (lambda (t::Traversal)
		   context)))))

(define-type (LineEnding reach: real
			 space: Space
			 index: int))

(define (line-ending-embracing position::real
			       #;from box::cons)
  ::LineEnding
  (let* ((last-space ::Space (pre-head-space box))
	 (previous-left ::real 0)
	 (next ::Traversal (Traversal))
	 (painter ::Painter (the-painter))
	 (space-width ::real (painter:space-width)))
    (define (breaking? element)::boolean
      (or (integer? element)
	  (and-let* ((comment ::Comment element))
	    (comment:breaks-line?))))

    (define (width element)::real
      (match element
	(number::integer
	 (* number space-width))
	(comment::Comment
	 (let ((extent ::Extent (extent+ comment)))
	   extent:width))))

    (call/cc
     (lambda (return)
       (traverse
	box
	doing:
	(lambda (item::Element current::Traversal)
	  (and-let* ((space ::Space item))
	    (set! last-space space)
	    (next:assign current)
	    (let skip ((input ::list space:fragments)
		       (fragment-index ::int 0))
	      (match input
		(`(,,@breaking? ,,@integer? . ,_)
		 (cond
		  ((is next:top <= position
		       < (+ next:top
			    next:max-line-height))
		   (return
		    (LineEnding
		     space: space
		     reach: current:left
		     index: fragment-index)))
		  (else
		   (next:expand-by! (width (car input)))
		   (next:new-line!)
		   (skip (cdr input) (+ fragment-index 1)))))

		(`(,,@(lambda (x)
			(or (integer? x)
			    (and-let* ((c ::Comment x))
			      (not (c:breaks-line?)))))
		   . ,rest)
		 (next:expand-by! (width (car input)))
		 (skip rest (+ fragment-index 1)))
		('()
		 (values))))
	      (set! previous-left current:left)))
	returning:
	(lambda (t::Traversal)
	  (LineEnding reach: previous-left
		      space: last-space
		      index: (last-space:last-index))))))))

#|
(define (cursor-above cursor::Cursor
		      #!optional
		      (document::list (head (the-document))))
  ::Cursor
  ...)

(define (cursor-below cursor::Cursor
		      #!optional
		      (document::list (head (the-document)))
		      #!key (context::Cursor (recons 1 '())))
  ::Cursor
  (call/cc
   (lambda (return)
     (traverse
      document
      doing:
      (lambda (item::Element t::Traversal)
	...)))))

|#

(define (sequence-extent #!optional
			 (elems::list (head (the-document))))
  ::Extent
  (if (empty? elems)
      (let* ((painter ::Painter (the-painter))
	     (traversal ::Traversal (Traversal
				     max-line-height:
				     (painter:min-box-height)))
	     (empty ::EmptyListProxy elems))
	(empty:space:expand! traversal)
	(Extent width: traversal:max-width
		height: (+ traversal:top traversal:max-line-height)))
      (traverse elems
		returning:
		(lambda (traversal::Traversal)
		  (Extent width: traversal:max-width
			  height: (+ traversal:top
				     traversal:max-line-height))))))

