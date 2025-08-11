(module-name (editor interfaces elements))

(import (srfi :11))
(import (srfi :17))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language mapping))
(import (language attributes))
(import (language define-parameter))
(import (language keyword-arguments))
(import (language match))
(import (language infix))
(import (language assert))
(import (language while))
(import (language for))
(import (language examples))
(import (language define-cache))
(import (utils print))
(import (utils string-building))
(import (utils functions))

(import (language fundamental))
(import (editor interfaces painting))

(define-type (Highlight start: Cursor
			end: Cursor
			type: HighlightType
			:= HighlightType:OtherFinding))

(define-attribute (extent-cached? tile::Tile)::boolean
  #f)

(define-attribute+ (cached-extent tile::Tile)::Extent
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
			on-end-line: (maps (boolean) to: void)
			:= nothing)
  extending Base with
  ((advance! element::Element)::void
   (cond
    ((Expandable? element)
     (let ((x ::Expandable (as Expandable element)))
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

  ((preceding-line-height)::real
   (if (and (zero? previous-line-height) parent)
       (parent:preceding-line-height)
       previous-line-height))

  ((cursor)::Cursor
   (hash-cons index (if parent (parent:cursor) '())))
  
  ((new-line!)::void
   (set! top (+ top max-line-height))
   (set! left 0)
   (set! previous-line-height max-line-height)
   (set! max-line-height (painter:min-line-height)))
  )

;; the methods provided by these interfaces should be thought of as
;; implicitly parameterized with painter, (the-cursor)
;; and (the-highlights) parameters

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
  (measure-position #;of cursor::Cursor #;into target::Position
			 #;within context::Cursor)::Position
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

(define (textual=? a ::Textual b ::Textual)::boolean
  (escape-with return
    (let ((n ::int (a:text-length)))
      (cond
       ((= n (b:text-length))
	(for i::int from 0 below n
	     (when (isnt (a:char-ref i) eq?
			 (b:char-ref i))
	       (return #false)))
	(return #true))
       (else
	(return #false))))))

(define (prefix-end prefix-candidate ::Textual
		    subject ::Textual)
  ::(maybe int)
  (escape-with return
    (let ((n ::int (prefix-candidate:text-length))
	  (m ::int (subject:text-length)))
      (cond
       ((is n <= m)
	(for i::int from 0 below n
	     (when (isnt (prefix-candidate:char-ref i) eq?
			 (subject:char-ref i))
	       (return #!null)))
	(return n))
       (else
	(return #!null))))))

(define (suffix-start suffix-candidate ::Textual
		      subject ::Textual)
  ::(maybe int)
  (escape-with return
    (let* ((n ::int (suffix-candidate:text-length))
	   (m ::int (subject:text-length))
	   (d ::int (- m n)))
      (cond
       ((is n <= m)
	(for i::int from 0 below n
	     (when (isnt (suffix-candidate:char-ref i) eq?
			 (subject:char-ref (+ i d)))
	       (return #!null)))
	(return d))
       (else
	(return #!null))))))

(define (infix-start infix-candidate ::Textual
		     subject ::Textual)
  ::(maybe int)
  (escape-with return
    (let* ((n ::int (infix-candidate:text-length))
	   (m ::int (subject:text-length))
	   (d ::int (- m n)))
      (when (is n <= m)
	(for k::int from 0 to d
	     (escape-with continue
	       (for i::int from 0 below n
		    (when (isnt (infix-candidate:char-ref i)
				eq? (subject:char-ref (+ i k)))
		      (continue)))
	       (return k))))
      (return #!null))))

(define-object (Simple)::Indexable
  (define (typename)::String "Simple")
  (define (part-at index::Index)::Indexable* (this))

  (define (first-index)::Index 0)
  (define (last-index)::Index 0)

  (define (next-index index::Index)::Index 0)
  (define (previous-index index::Index)::Index 0)

  (define (index< a::Index b::Index)::boolean #f)

  (Base))

(define-object (DelegateIndexing target::Indexable)::Indexable
  (define (typename)::String
    (string-append "DelegateIndexing"))
  
  (define (part-at index::Index)::Indexable*
    (let ((result (target:part-at index)))
      (if (eq? result target)
	  (this)
	  result)))

  (define (first-index)::Index
    (target:first-index))
  
  (define (last-index)::Index
    (target:last-index))

  (define (next-index index::Index)::Index
    (target:next-index index))
  
  (define (previous-index index::Index)::Index
    (target:previous-index index))

  (define (index< a::Index b::Index)::boolean
    (target:index< a b))

  (Base))

(define-interface ExpandableTextualElement (Expandable
					    Textual
					    Element))

(define-interface Extensive ()
  (extent)::Extent
  )


#|
`the-cursor` and `the-document` are parameters
that provide the default values to some functions that
operate on cursors.
|#

(define-parameter (the-cursor) ::Cursor '(#\[ 1))

(define-parameter (the-document) ::pair
  (cons (cons '() '()) '()))

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

(define (cursor<= a::Cursor b::Cursor
		  #!optional (document (the-document)))
  (or (equal? a b)
      (cursor< a b document)))

(define-parameter (the-highlights)::(list-of Highlight)
  '())

(define-parameter (the-selection)::Highlight
  (Highlight start: '() end: '()
	     type: HighlightType:Selection))

;; A Keeper is needed to obtain permissions on Android
;; - otherwise it does nothing special
(define-interface Keeper ()
  (with-read-permission action::procedure)::Object
  (with-write-permission action::procedure)::Object
  (initial-directory)::java.io.File
  (file-system-roots)::(list-of File)
  )

(define-object (PermissiveKeeper)::Keeper
  (define (with-read-permission action::procedure)::Object
    (action))
  
  (define (with-write-permission action::procedure)::Object
    (action))
  
  (define (initial-directory)::java.io.File
    (let ((wd ::java.io.File (java.io.File ".")))
      (wd:getAbsoluteFile)))

  (define (file-system-roots)::(list-of File)
    (let ((roots (only (lambda (root::java.io.File)
			  (and (root:canRead)
			       (root:canExecute)))
		       (java.io.File:listRoots))))
      (if (null? roots)
	  (let ((base (java.lang.System:getProperty
		       "user.dir")))
	    (list
	     (java.io.File base)))
	  roots)))
   )

(define-mapping (file-display-name file ::java.io.File)::String
  (file:getName))

(define-parameter (the-keeper)::Keeper
  (PermissiveKeeper))

(define-parameter (the-gravity) ::(sequence-of real)
  (vector 0.0 0.04 0.0))

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

(define-interface Renderable ()
  (render!)::void
  )

(define-interface Animate (Renderable Animation)
  )

(define-interface Pane (Renderable Interactive))

(define-interface Layer (Indexable Pane)
  (close!)::void
  (permanent?)::boolean
  )

(define-object (DeadLayer)::Layer
  (define (render!)::void
    #!abstract)

  (define (permanent?)::boolean #f)
  
  (define (close!)::void
    (values))
  (IgnoreInput))

(define-object (DelegatingLayer target::Layer)::Layer
  (define (close!)::void
    (target:close!))

  (define (permanent?)::boolean
    (target:permanent?))
  
  (define (tap! finger::byte #;at x::real y::real)::boolean
    (target:tap! finger x y))
  (define (press! finger::byte #;at x::real y::real)::boolean
    (target:press! finger #;at x y))
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (target:second-press! finger #;at x y))
  (define (double-tap! finger::byte x::real y::real)::boolean
    (target:double-tap! finger x y))
  (define (long-press! finger::byte x::real y::real)::boolean
    (target:long-press! finger x y))
  (define (key-typed! key-code::long context::Cursor)::boolean
    (target:key-typed! key-code context))
  
  (define (scroll-up! left::real top::real)::boolean
    (target:scroll-up! left top))
  (define (scroll-down! left::real top::real)::boolean
    (target:scroll-down! left top))
  (define (scroll-left! left::real top::real)::boolean
    (target:scroll-left! left top))
  (define (scroll-right! left::real top::real)::boolean
    (target:scroll-right! left top))
  
  (define (zoom-in! left::real top::real)::boolean
    (target:zoom-in! left top))
  (define (zoom-out! left::real top::real)::boolean
    (target:zoom-out! left top))
  
  (define (rotate-left! left::real top::real)::boolean
    (target:rotate-left! left top))
  (define (rotate-right! left::real top::real)::boolean
    (target:rotate-right! left top))

  (define (render!)::void
    (target:render!))
  
  (DelegateIndexing target))

(define-syntax-rule (HijackLayerInput target methods ...)
  (object (DelegatingLayer)
     ((*init*)
      (invoke-special DelegatingLayer (this)
		      '*init* target))
     methods
     ...))

(define-interface Embeddable (Pane Map2D)
  (close-document! document)::void
  (drop-at! x::real y::real expression::pair)::boolean)

(define-interface Splittable (Embeddable)
  (pane-under x::real y::real)::Embeddable

  (can-split-beside? line::Area)::boolean
  (split-beside! line::Area)::Embeddable
  
  (can-split-below? line::Area)::boolean
  (split-below! line::Area)::Embeddable

  (active)::Embeddable)

(define-interface Editor (Splittable
			  java.lang.Cloneable
			  WithCursor)
  (add-post-draw-action! action::(maps () to: void))
  ::void
  (move-cursor-left!)::void
  (move-cursor-right!)::void
  (move-cursor-up!)::void
  (move-cursor-down!)::void
  (unnest-cursor-right!)::void
  
  (expand-selection-right!)::void
  (expand-selection-left!)::void
  (update-cursor-column!)::void
  )

(define-object (NullPane)::Embeddable
  (define (drop-at! x::real y::real expression::pair)::boolean
    #f)

  (define (active)::Embeddable
    (this))
  
  (define (render!)::void (values))
  
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

  (define (close-document! document)::void
    (values))
  
  (IgnoreInput))

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real vy::real)::void
  )

(define-object (NoDrop)::Drag
  (define (move! x::real y::real dx::real dy::real)::void #!abstract)
  (define (drop! x::real y::real vx::real vy::real)::void (values))
  )

(define-alias ResizeAnchor java.lang.Object)

(define-interface Resizable (Extensive)
  (can-be-resized?)::boolean
  (resize-anchor position::real)::ResizeAnchor
  (set-size! width::real height::real anchor::ResizeAnchor)::void
  )

(define-interface Tile (Extensive Element))

(define-interface ShadowedTile (Shadowed Tile))

(define-interface ResizableShadowedTile
  (Resizable ShadowedTile))

(define-interface TextualTile (Textual Tile))

(define-interface ShadowedTextualTile (Shadowed TextualTile))

(define-interface MatchableResizableTile
  (Matchable Resizable Tile))

(define-interface MatchableShadowedTextualTile
  (Matchable ShadowedTextualTile))

(define-interface Comment (Expandable Tile)
  (breaks-line?)::boolean
  (print out::gnu.lists.Consumer)::void)

(define-interface TextualComment (Textual Comment)
  (removable?)::boolean
  (remove-from! fragments::list)::list
  )

(define-interface Enchanted (Interactive ShadowedTile)
  ;; in the case of extension, the "value" method
  ;; of the Shadowed interface should return a cons-cell
  )

(define-interface Alive (Enchanted Animation))

(define-interface Maximizable (Embeddable
			       Resizable
			       Enchanted)
  (can-be-maximized?)::boolean)

(define-interface World (Resizable Alive))

(define-interface Playable ()
  (rewind!)::void
  (back!)::void
  (play!)::void
  (pause!)::void
  (next!)::void
  (fast-forward!)::void
  (playing?)::boolean)

(define-interface Player (Enchanted Playable Animation))

(define-interface WorldPlayer (World Player))

(define-interface Screen (Resizable
			  Splittable
			  Interactive)
  (drag! finger::byte action::Drag)::void
  (undrag! finger::byte)::void
  
  (add-overlay! layer::Layer)::void
  (pop-overlay!)::Layer
  (contains-overlay? satisfying?::predicate)::(maybe Layer)
  (remove-overlay! layer::Layer)::boolean
  (remove-overlay-if! satisfying?::(maps (Layer) to: boolean))::boolean

  (clear-overlay!)::void
  (overlay-cursor layer::Layer)::Cursor
  (set-overlay-cursor! layer::Layer cursor::Cursor)::void
  (has-layer satisfying::predicate)::Layer

  (after-tap action::(maps (byte real real) to: void))::void

  (maximize! tile ::Maximizable)::void
  (unmaximize!)::void
  
  (content)::Embeddable
  (set-content! content::Embeddable)::void
  (width)::real
  (height)::real

  )

(define-interface EnchantedDragLayer (Enchanted Drag Layer))

(define-attribute+ (screen-position editor::Embeddable)::Position
  (Position))

(define-attribute+ (screen-extent editor::Embeddable)::Extent
  (Extent))
