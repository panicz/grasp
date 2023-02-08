(import (srfi :11))
(import (srfi :17))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (default-value))
(import (define-parameter))
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

;; the methods provided by these interfaces should be thought of as
;; implicitly parameterized with (the-painter), (the-cursor)
;; and (the-selection-anchor) parameters

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
  (extent)::Extent)

(define-interface ShadowedTile (Shadowed Tile))

(define-interface TextualTile (Textual Tile))

(define-interface ShadowedTextualTile (Shadowed TextualTile))

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
