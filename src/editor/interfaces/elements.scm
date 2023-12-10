(module-name (editor interfaces elements))

(import (srfi :11))
(import (srfi :17))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-property))
(import (language define-parameter))
(import (language keyword-arguments))
(import (language match))
(import (language infix))
(import (language assert))
(import (language for))
(import (language examples))
(import (language define-cache))
(import (utils print))
(import (utils string-building))
(import (utils functions))

(import (language fundamental))
(import (editor interfaces painting))

;; the methods provided by these interfaces should be thought of as
;; implicitly parameterized with painter, (the-cursor)
;; and (the-selection-anchor) parameters

(define-property (extent-cached? tile::Tile)::boolean
  #f)

(define-property+ (cached-extent tile::Tile)::Extent
  (Extent width: 0 height: 0))

(define (extent+ tile::Tile)::Extent
  (let ((cached ::Extent (cached-extent tile)))
    (unless (is tile extent-cached?)
      (let ((fresh ::Extent (tile:extent)))
        (cached:assign fresh)
	(set! (extent-cached? tile) #t)))
    cached))

(define-type (Traversal left: real := 0
			top: real := 0
			index: int := 0
			max-width: real := 0
			max-line-height: real := 0
			parent-left: real := 0
			parent-top: real := 0
			parent: Traversal := #!null
			previous-line-height: real := 0
			on-end-line: (maps () to: void)
			:= nothing)
  extending Base with
  ((advance! element::Element)::void
   (cond
    ((Expandable? element)
     (let ((x ::Expandable element))
       (x:expand! (this))))
    ((Tile? element)
     (expand! (extent+ (as Tile element))))
    (else
     (error "Unable to advance over "element)))
   (set! index (+ index 1)))

  ((expand-by! width::real)::void
   (set! left (+ left width))
   (set! max-width (max max-width left)))

  ((expand! extent::Extent)::void
   (expand-by! extent:width)
   (set! max-line-height (max extent:height
			      max-line-height)))

  ((preceding-line-height)
   (if (and (zero? previous-line-height) parent)
       (parent:preceding-line-height)
       previous-line-height))
  
  ((new-line!)::void
   (on-end-line)
   (set! top (+ top max-line-height))
   (set! left 0)
   (set! previous-line-height max-line-height)
   (set! max-line-height (painter:min-line-height)))

  )


(define-interface Expandable ()
  (expand! t::Traversal)::void)

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

(define-interface Element (Indexable java.lang.Cloneable)
  (draw! context::Cursor)::void
  (cursor-under* x::real y::real path::Cursor)::Cursor*
  (clone)::Element
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

(define-object (Simple)::Indexable
  (define (typename)::String "Simple")
  (define (part-at index::Index)::Indexable* (this))

  (define (first-index)::Index 0)
  (define (last-index)::Index 0)

  (define (next-index index::Index)::Index 0)
  (define (previous-index index::Index)::Index 0)

  (define (index< a::Index b::Index)::boolean #f)

  (Base))

(define-interface Tile (Element)
  (extent)::Extent
  )

(define-interface ShadowedTile (Shadowed Tile))

(define-interface TextualTile (Textual Tile))

(define-interface ShadowedTextualTile (Shadowed TextualTile))

(define-interface Comment (Expandable Tile)
  (breaks-line?)::boolean
  (print out::gnu.lists.Consumer)::void)

(define-interface TextualComment (Textual Comment)
  (removable?)::boolean
  (remove-from! fragments::list)::list
  )

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
    (cond ((is length/b = 0)
	   #f)

	  ((is length/a = 0)
	   #t)

	  ((is length/a < length/b)
	   (let ((stem/b (drop (- length/b length/a) b)))
	     (k< (- length/a 1) a stem/b document)))

	  ((is length/a = length/b)
	   (k< (- length/a 1) a b document))

	  ((is length/a > length/b)
	   (not (cursor< b a document))))))

(define (the-selection)
  ;; temporary override:
  ;;(values (the-cursor) (the-cursor))
  
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


;; A Keeper is needed to obtain permissions on Android
;; - otherwise it does nothing special
(define-interface Keeper ()
  (with-read-permission action::(maps () to: void))::void
  (with-write-permission action::(maps () to: void))::void
  (initial-directory)::java.io.File
  )

(define-object (PermissiveKeeper)::Keeper
  (define (with-read-permission action::(maps () to: void))::void
    (action))
  (define (with-write-permission action::(maps () to: void))::void
    (action))
  (define (initial-directory)::java.io.File
    (let ((wd ::java.io.File (java.io.File ".")))
      (wd:getAbsoluteFile)))
  )

(define-parameter (the-keeper)::Keeper
  (PermissiveKeeper))


(define-interface Interactive ()
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean
  (second-press! finger::byte #;at x::real y::real)::boolean
  (double-tap! finger::byte x::real y::real)::boolean
  (long-press! finger::byte x::real y::real)::boolean
  (key-typed! key-code::long context::Cursor)::boolean

  (scroll-up! left::real top::real)::boolean
  (scroll-down! left::real top::real)::boolean
  (scroll-left! left::real top::real)::boolean
  (scroll-right! left::real top::real)::boolean
  (zoom-in! left::real top::real)::boolean
  (zoom-out! left::real top::real)::boolean
  (rotate-left! left::real top::real)::boolean
  (rotate-right! left::real top::real)::boolean
  )

(define-object (IgnoreInput)::Interactive
  (define (tap! finger::byte #;at x::real y::real)::boolean #f)
  (define (press! finger::byte #;at x::real y::real)::boolean #f)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #f)
  (define (double-tap! finger::byte x::real y::real)::boolean #f)
  (define (long-press! finger::byte x::real y::real)::boolean #f)
  
  (define (key-typed! key-code::long context::Cursor)::boolean #f)

  (define (scroll-up! left::real top::real)::boolean #f)
  (define (scroll-down! left::real top::real)::boolean #f)
  (define (scroll-left! left::real top::real)::boolean #f)
  (define (scroll-right! left::real top::real)::boolean #f)

  (define (zoom-in! left::real top::real)::boolean #f)
  (define (zoom-out! left::real top::real)::boolean #f)

  (define (rotate-left! left::real top::real)::boolean #f)
  (define (rotate-right! left::real top::real)::boolean #f)
  
  (Simple))

(define-object (ConsumeInput)::Interactive
  (define (tap! finger::byte #;at x::real y::real)::boolean #t)
  (define (press! finger::byte #;at x::real y::real)::boolean #t)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #t)
  (define (double-tap! finger::byte x::real y::real)::boolean #t)
  (define (long-press! finger::byte x::real y::real)::boolean #t)
  (define (key-typed! key-code::long context::Cursor)::boolean #t)

  (define (scroll-up! left::real top::real)::boolean #t)
  (define (scroll-down! left::real top::real)::boolean #t)
  (define (scroll-left! left::real top::real)::boolean #t)
  (define (scroll-right! left::real top::real)::boolean #t)

  (define (zoom-in! left::real top::real)::boolean #t)
  (define (zoom-out! left::real top::real)::boolean #t)

  (define (rotate-left! left::real top::real)::boolean #t)
  (define (rotate-right! left::real top::real)::boolean #t)
  
  (Simple))

(define-interface Drawable ()
  (draw!)::void
  )


(define-interface Pane (Drawable Interactive))

(define-interface Layer (Indexable Pane))

(define-interface Embeddable (Pane Map2D)
  (drop-at! x::real y::real expression::pair)::boolean
  
  (pane-under x::real y::real)::Embeddable

  (can-split-beside? line::Area)::boolean
  (split-beside! line::Area)::Embeddable
  
  (can-split-below? line::Area)::boolean
  (split-below! line::Area)::Embeddable
  )

(define-interface Editor (Embeddable java.lang.Cloneable)
  )

(define-parameter (the-editor)::Editor
  #!null)

(define-object (NullPane)::Embeddable
  (define (drop-at! x::real y::real expression::pair)::boolean
    #f)
  
  (define (draw!)::void (values))
  
  (define (pane-under x::real y::real)::Embeddable
    (this))
  
  (define (outside-in x::real y::real)::(Values real real)
    (values x y))
  
  (define (inside-out x::real y::real)::(Values real real)
    (values x y))
  
  (define (can-split-beside? line::Area)::boolean
    #f)
  
  (define (split-beside! line::Area)::Embeddable
    (this))
  
  (define (can-split-below? line::Area)::boolean
    #f)

  (define (split-below! line::Area)::Embeddable
    (this))

  (define (scroll-up! left::real top::real)::boolean
    #f)
  
  (define (scroll-down! left::real top::real)::boolean
    #f)
  
  (define (scroll-left! left::real top::real)::boolean
    #f)
  
  (define (scroll-right! left::real top::real)::boolean
    #f)

  (define (zoom-in! left::real top::real)::boolean
    #f)
  
  (define (zoom-out! left::real top::real)::boolean
    #f)
  
  (IgnoreInput))

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real vy::real)::void
  )

(define-interface Resizable ()
  (set-size! width::real height::real)::void
  (size)::Extent
  )

(define-interface ResizablePane (Resizable Pane))
