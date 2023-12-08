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
(import (language while))
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

(define-syntax-rule (with-edit-access actions ...)
  (parameterize ((cell-access-mode CellAccessMode:Editing))
    actions ...))

(define (evaluating?) ::boolean
  (eq? (cell-access-mode) CellAccessMode:Evaluating))

(define (editing?) ::boolean
  (eq? (cell-access-mode) CellAccessMode:Editing)
)
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
    (painter:draw-atom!
       name
       ;;(string-append name "/" (number->string (id (this))))
       context))

  (define (extent)::Extent
    (painter:atom-extent
     name
     ;;(string-append name "/" (number->string (id (this))))
     ))

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
      (let ((inner (extent)))
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
    (let* ((inner ::Extent (sequence-extent (this)))
	   (t ::Traversal (the-traversal))
	   (paren-width ::real (painter:paren-width)))
      (painter:draw-box! (+ inner:width (* 2 paren-width))
			 inner:height
			 context)
      (with-translation (paren-width 0)
	(set! t:parent-left (+ t:parent-left paren-width))
	(try-finally
	 (draw-sequence! (this) context: context)
	 (set! t:parent-left (- t:parent-left paren-width))))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let* ((inner ::Extent (sequence-extent (this)))
	     (t ::Traversal (the-traversal))
	     (paren-width ::real (painter:paren-width)))

      (and (is 0 <= y < inner:height)
	   (or (and (is 0 <= x < paren-width)
		    (recons (first-index) path))

	       (and (is 0 <= (- x paren-width) < inner:width)
		    (set! t:parent-left (+ t:parent-left paren-width))
		    (try-finally
		     (cursor-under (- x paren-width) y
				   (this) context: path)
		     (set! t:parent-left (- t:parent-left paren-width))))
	       (and (is 0 <= (- x paren-width inner:width)
			< paren-width)
		    (recons (last-index) path)))))))

  (define (extent)::Extent
    (let ((extent ::Extent (sequence-extent
			    (this))))
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

  (define pre-head-space ::Space (Space fragments: (pair 0 '())))

  (define dotted? ::boolean
    (not (or (empty? cdr)
	     (pair? cdr))))

  (define post-head-space ::Space
    (if (and (not dotted?)
	     (empty? cdr))
	(Space fragments: (pair 0 '()))
	(Space fragments: (pair 1 '()))))

  (define pre-tail-space ::Space
    (Space fragments: (pair 1 '())))

  (define post-tail-space ::Space
    (Space fragments: (pair 0 '())))
  
  (pair car cdr))

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

(define-syntax-rule (define-accessor (name object::type)::result)
  (define-early-constant name
    (let ((getter (lambda (object::type)
		    ::result
		    (slot-ref object 'name))))
      (set! (setter getter)
	    (lambda (object::type value::result)
	      ::void
	      (slot-set! object 'name value)))
      getter)))

(define-accessor (dotted? cell::cons)::boolean)
(define-accessor (pre-head-space cell::cons)::Space)
(define-accessor (post-head-space cell::cons)::Space)
(define-accessor (pre-tail-space cell::cons)::Space)
(define-accessor (post-tail-space cell::cons)::Space)

(define (last-space sequence::pair)::Space
  (let ((cell ::pair (last-pair sequence)))
    (if (dotted? cell)
	(post-tail-space cell)
	(post-head-space cell))))

(define (should-the-bar-be-horizontal? dotted-pair)
  ::boolean
  (assert (dotted? dotted-pair))
  (and-let* (((Space fragments: `(,_ ,_ . ,_))
	      (post-head-space dotted-pair))
	     ((Space fragments: `(,_ ,_ . ,_))
	      (pre-tail-space dotted-pair)))))

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
  (Extent width: (apply max space:fragments)
	  height: (* (painter:min-box-height)
		     (length space:fragments))))

(define-syntax traverse*
  (syntax-rules (doing: returning:)
    ((_ sequence doing: action returning: result)
     (let* ((parent ::Traversal (the-traversal))
	    (traversal ::Traversal
		       (Traversal
			max-line-height:
			(painter:min-line-height)
			parent-left: (+ parent:parent-left
					parent:left)
			parent-top: (+ parent:parent-top
				       parent:top)
			parent: parent)))

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
		    (traversal:on-end-line)
		    (result traversal))
		   ((pair? (tail pair))
		    (step! (tail pair)))
		   (else
		    (traversal:on-end-line)
		    (result traversal)))))

	 (if (pair? sequence)
	     (let ((pre-head ::Space (pre-head-space sequence)))
               (action pre-head traversal)
               (traversal:advance! pre-head)
               (step! sequence))
	     (begin
	       (traversal:on-end-line)
	       (result traversal)))
	 )))
    ((_ sequence doing: action)
     (traverse* sequence doing: action returning: nothing))

    ((_ sequence returning: result)
     (traverse* sequence doing: nothing returning: result))))

(define/kw (traverse sequence::list
		     doing: action ::(maps (Element Traversal)
					   to: void)
		     := nothing
		     returning: result ::(maps (Traversal) to: ,a)
		     := nothing)
  ;; ::,a
  (traverse* sequence doing: action returning: result))

(define (overlap? A-left ::real A-right ::real
		  B-left ::real B-right ::real)
  ::boolean
  (assert (is A-left <= A-right))
  (assert (is B-left <= B-right))
  (and (is A-left <= B-right)
       (is A-right >= B-left)))

(define (visible? left ::real top ::real right ::real bottom ::real)::boolean
  (and (overlap? left right (view-edge-left) (view-edge-right))
       (overlap? top bottom (view-edge-top) (view-edge-bottom))))

(define (draw-sequence! #!optional
			(elems::list (head (the-document)))
			#!key (context::Cursor (recons 1 '())))
  ::void
  (escape-with end-drawing
    (let*-values (((selection-start selection-end) (the-selection)))      
      (define (action item ::Element traversal ::Traversal)
	(escape-with skip-element
	  (with-translation (traversal:left
			     traversal:top)
	    (when (is item instance? Tile)
	      (let* ((document-left ::real (+ traversal:left traversal:parent-left))
		     (document-top ::real (+ traversal:top traversal:parent-top))
		     (e ::Extent (extent+ item))
		     (document-right ::real (+ document-left e:width))
		     (document-bottom ::real (+ document-top e:height)))

		(unless (visible? document-left document-top
				  document-right document-bottom)
		  (when (is document-top > (view-edge-bottom))
		    (end-drawing))
		  (skip-element))))
	    
	    (let ((context (recons traversal:index
				   context)))
	      (when (equal? context selection-start)
		(painter:enter-selection-drawing-mode!))
	      (item:draw! context)
	      (when (equal? context selection-end)
		(painter:exit-selection-drawing-mode!))))))
      (traverse elems doing: action))))
  
(define (draw! object #!key
	      (context::Cursor '()))
  ::void
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
		   (car (the-cursor)))))))))

(define (cursor-under left::real top::real
		      #!optional
		      (elems::list (head (the-document)))
		      #!key (context::Cursor (recons 1 '())))
  ::Cursor
  (escape-with return
    (define (action item ::Element t::Traversal)
      (and-let* ((cursor (item:cursor-under*
			  (- left t:left)
			  (- top t:top)
			  (recons t:index context))))
	(return cursor)))
    (define (result t ::Traversal) context)
    
    (traverse elems doing: action returning: result)))

(define-type (LineEnding reach: real
			 space: Space
			 index: int))

(define (line-ending-embracing position::real
			       #;from box::cons)
  ::LineEnding
  (let* ((last-space ::Space (pre-head-space box))
	 (previous-left ::real 0)
	 (next ::Traversal (Traversal))
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

    (escape-with return

      (define-syntax-rule (action item #|::Element|# current #|::Traversal|#)
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

      (define-syntax-rule (result t #|::Traversal|#)
	(LineEnding reach: previous-left
		    space: last-space
		    index: (last-space:last-index)))
      
      (traverse* box doing: action returning: result))))

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


(define (document-position-of-element-pointed-by cursor document 
                                                 #!key (context::Cursor (recons 1 '())))
  ::(Values real real)
  (escape-with return
    (define (action item ::Element traversal ::Traversal)
      (let ((c (recons traversal:index context)))
        (cond ((or (equal? c cursor)
		   (and (pair? cursor)
			(equal? c (cdr cursor))))
               (return (+ traversal:left traversal:parent-left)
                       (+ traversal:top traversal:parent-top)))
	      ((is c suffix? cursor)
	       (cond ((pair? item)
		      (let-values (((x y) (document-position-of-element-pointed-by
					   cursor item context: c)))
			(return (+ x (painter:paren-width)) y)))
		     (else
		      (return (+ traversal:left traversal:parent-left)
			      (+ traversal:top traversal:parent-top))))))))
   
    (define (fallback t::Traversal)
      (values (+ t:left t:parent-left) (+ t:top t:parent-top)))

    (traverse document doing: action returning: fallback)))

(define (sequence-extent #!optional
			 (elems::list (head (the-document))))
  ::Extent
  (cond
   ((empty? elems)
    (let* ((traversal ::Traversal (Traversal
				   max-line-height:
				   (painter:min-box-height)))
	   (empty ::EmptyListProxy elems))
      (empty:space:expand! traversal)
      (Extent width: traversal:max-width
	      height: (+ traversal:top traversal:max-line-height))))
   (else
    (define-syntax-rule (result traversal #|::Traversal|#)
		(Extent width: traversal:max-width
			height: (+ traversal:top
				   traversal:max-line-height)))
    (traverse* elems returning: result))))

(define cell-display-properties
  (list
   dotted?
   pre-head-space
   post-head-space
   pre-tail-space
   post-tail-space))

(define (copy-properties properties original cell)
  (for property in properties
    (set! (property cell) (property original)))
  cell)

(define (copy-properties* properties original cell)
  (copy-properties properties original cell)
  (when (and (pair? (cdr original))
	     (pair? (cdr cell)))
    (copy-properties* properties (cdr original) (cdr cell))))

(define (tail-space-to-head original cell)
  (set! (pre-head-space cell)
	   (pre-tail-space original))
  (set! (post-head-space cell)
	   (post-tail-space original))
  cell)

(define (head-space-to-tail original cell)
  (set! (pre-tail-space cell)
	   (pre-head-space original))
  (set! (post-tail-space cell)
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

   ((string? p)
    (write-char #\")
    (for c in p
      (when (or (eq? c #\")
		(eq? c #\\))
	(write-char #\\))
      (write-char c))
    (write-char #\"))
   
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

(define-parameter (the-pane-width)::real
  +inf.0)

(define-parameter (the-pane-height)::real
  +inf.0)

(define-parameter (the-pane-left)::real
  0)

(define-parameter (the-pane-top)::real
  0)


;; The following are expressed in the document
;; coordinates, and actually set in the Editor's
;; "draw!" method.

(define-parameter (view-edge-left)::real
  -inf.0)

(define-parameter (view-edge-right)::real
  +inf.0)

(define-parameter (view-edge-top)::real
  -inf.0)

(define-parameter (view-edge-bottom)::real
  +inf.0)


(define-syntax-rule (with-view-edges-transformed transform . actions)
  (let*-values (((pane-left pane-top) (values (the-pane-left)
					      (the-pane-top)))
		((pane-right pane-bottom)
		 (values (+ pane-left (the-pane-width))
			 (+ pane-top (the-pane-height))))

		((left-top top-left) (transform:outside-in pane-left pane-top))
		((right-bottom bottom-right) (transform:outside-in pane-right
								   pane-bottom))
		((left-bottom bottom-left) (transform:outside-in pane-left
								 pane-bottom))
		((right-top top-right) (transform:outside-in pane-right pane-top))
		((left right) (min+max left-top right-bottom left-bottom right-top))
		((top bottom) (min+max top-left bottom-right bottom-left top-right)))
    (parameterize ((view-edge-left left)
		   (view-edge-right right)
		   (view-edge-top top)
		   (view-edge-bottom bottom))
      . actions)))
