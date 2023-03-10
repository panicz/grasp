(import (srfi :11))
(import (srfi :17))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (default-value))
(import (define-parameter))
(import (keyword-arguments))
(import (match))
(import (infix))
(import (assert))
(import (for))
(import (examples))
(import (define-cache))
(import (print))
(import (string-building))
(import (functions))
(import (extent))
(import (fundamental))
(import (painter))

;; the methods provided by these interfaces should be thought of as
;; implicitly parameterized with (the-painter), (the-cursor)
;; and (the-selection-anchor) parameters

(define-type (Traversal left: real := 0
			top: real := 0
			index: int := 0
			max-width: real := 0
			max-line-height: real := 0)
  extending Base with
  ((advance/extent! extent::Extent)::void
   (set! left (+ left extent:width))
   (set! max-line-height (max extent:height
			      max-line-height))
   (set! max-width (max left max-width))
   (set! index (+ index 1)))

  ((advance-by! width::real)::void
   (set! left (+ left width))
   (set! max-width (max max-width left)))

  ((new-line!)::void
   (set! top (+ top max-line-height))
   (set! left 0)
   (set! max-line-height (invoke (the-painter)
				 'min-line-height)))
  
  )


(define-parameter (the-traversal) ::Traversal
  (Traversal left: 0
	     top: 0
	     index: 0
	     max-width: 0
	     max-line-height: 0))


(define-interface Indexable ()
  (part-at index::Index)::Indexable*
  
  (first-index)::Index
  (last-index)::Index
  
  (next-index index::Index)::Index
  (previous-index index::Index)::Index
  
  (index< a::Index b::Index)::boolean
  )

(define-interface Shadowed ()
  (value)::Object)

(define-interface Element (Indexable)
  (draw! context::Cursor)::void
  (cursor-under* x::real y::real path::Cursor)::Cursor*

  (advance! traversal::Traversal)::void
  #|
  (cursor-above* path::Cursor)::Cursor*
  (cursor-below* path::Cursor)::Cursor*
|#
  )

(define-interface Textual ()
  (insert-char! c::char index::int)::void
  (delete-char! index::int)::char
  (char-ref index::int)::char
  (text-length)::int
  
  (split! position::int)::Textual
  (merge! following::Textual)::boolean
  )

(define (text-length item::Textual)::int
  (item:text-length))

(define-object (Simple)::Element
  (define (typename)::String "Simple")
  (define (part-at index::Index)::Indexable* (this))
  
  (define (first-index)::Index 0)
  (define (last-index)::Index 0)
  
  (define (next-index index::Index)::Index 0)
  (define (previous-index index::Index)::Index 0)

  (define (index< a::Index b::Index)::boolean #f)

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    #!abstract)

  (Base))

(define-interface Tile (Element)
  (extent)::Extent
  (advance! traversal::Traversal)::void
  ;;:= (traversal:advance/extent! (extent))
  )

(define-interface ShadowedTile (Shadowed Tile))

(define-interface TextualTile (Textual Tile))

(define-interface ShadowedTextualTile (Shadowed TextualTile))

(define-interface Comment (Tile)
  ;; docelowo to bedzie musial byc Tile
  (print out::gnu.lists.Consumer)::void)

(define-interface TextualComment (Textual Comment))

#|
`the-cursor` and `the-document` are parameters
that provide the default values to some functions that
operate on cursors.
|#

(define-parameter (the-cursor) ::Cursor '())

(define-parameter (the-document) ::pair
  (cons (cons '() '()) '()))

(define-parameter (the-selection-anchor) :: Cursor '())

;; We stipulate that, for any N >= 1, () < (x1 ... xN)
;; (as a consequence, whenever one cursor is a proper
;; suffix of another, it is considered to be "earlier"
;; than the longer one)

(define (cursor< a::Cursor b::Cursor
		 #!optional (document (the-document)))
  ::boolean
  (define (k< k::int a*::Cursor b*::Cursor parent::Indexable)
    ::boolean
    (if (is k < 0)
	#f
	(let* ((a/k (a* k))
	       (b/k (b* k)))
	  (if (eqv? a/k b/k)
	      (k< (- k 1) a* b* (parent:part-at a/k))
	      (parent:index< a/k b/k)))))
  
  (let* ((length/a (length a))
	 (length/b (length b)))
    (cond ((is length/a < length/b)
	   (let ((stem/b (drop (- length/b length/a) b)))
	     (k< (- length/a 1) a stem/b document)))
	     
	  ((is length/a = length/b)
	   (k< (- length/a 1) a b document))
    
	  ((is length/a > length/b)
	   (not (cursor< b a document))))))

(define (the-selection)
  ;; implicitly parameterized with (the-document),
  ;; (the-cursor) and (the-selection-anchor),
  ;; because cursor< is parameterized with (the-document)
  ;; and the remaining parameters are used directly
   (if (is (the-selection-anchor) cursor< (the-cursor))
       (values (the-selection-anchor) (the-cursor))
       (values (the-cursor) (the-selection-anchor))))

(define (within-selection? context::Cursor)::boolean
  ;; implicitly parameterized with (the-document),
  ;; (the-cursor) and (the-selection-anchor),
  ;; because cursor< is parameterized with (the-document),
  ;; and (the-selection) is implicitly parameterized
  ;; with (the-document), (the-cursor)
  ;; and (the-selection-anchor)
  (and (pair? (the-selection-anchor))
       (isnt (the-selection-anchor) equal? (the-cursor))
       (let-values (((selection-start selection-end) (the-selection)))
	 (is selection-start cursor< context cursor< selection-end))))
