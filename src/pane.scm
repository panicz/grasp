(import (srfi :17))
(import (hash-table))
(import (define-property))
(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (default-value))
(import (define-parameter))
(import (define-cache))
(import (keyword-arguments))
(import (mapping))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (fundamental))
(import (indexable))
(import (cursor))
(import (interactive))
(import (primitive))
(import (document))
(import (extension))
(import (extent))
(import (parse))
(import (conversions))
(import (painter))
(import (print))
(import (parameterize-up))
(import (document-operations))
(import (space))
(import (input))
(import (history))
(import (button))
(import (document))
(import (combinators))

(define-alias Array java.util.Arrays)

(define-interface Drawable ()
  (draw!)::void
  )

(define-interface Pane (Drawable Interactive))

(define-interface Layer (Indexable Pane))

(define-object (NullPane)::Pane
  (define (draw!)::void (values))
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

(define-object (Screen)::ResizablePane
  (define overlay ::Overlay (Overlay))
  (define dragging ::(maps byte to: Drag)
    (mapping (finger::byte)::Drag #!null))

  (define top ::Pane (NullPane))

  ;; this parameter must be set by the
  ;; graphical framework (Lanterna, AWT, ...)
  ;; and changed every time the hosting
  ;; window is resized
  (define extent ::Extent (Extent width: 0 height: 0))

  (define (drag! finger::byte action::Drag)::void
    (set! (dragging finger) action))

  (define (set-size! width::real height::real)::void
    (set! extent:width width)
    (set! extent:height height))

  (define (size)::Extent extent)

  (define (set-content! content::Pane)::void
    (set! top content))

  (define (content)::Pane top)

  (define (draw!)::void
    (top:draw!)
    (overlay:draw!))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (or (overlay:tap! finger x y)
	(top:tap! finger x y)))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (or (overlay:press! finger x y)
	(top:press! finger x y)))

  (define (release! finger::byte x::real y::real
		    vx::real vy::real)
    ::boolean
    (and-let* ((drag ::Drag (dragging finger)))
      (drag:drop! x y vx vy)
      (unset! (dragging finger))
      #t))

  (define (move! finger::byte x::real y::real
		 dx::real dy::real)
    ::boolean
    (and-let* ((drag ::Drag (dragging finger)))
      (drag:move! x y dx dy)
      #t))

  (define (second-press! finger::byte #;at x::real y::real)
    ::boolean
    (or (overlay:second-press! finger x y)
	(top:second-press! finger x y)))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (or (overlay:double-tap! finger x y)
	(top:double-tap! finger x y)))

  (define (long-press! finger::byte x::real y::real)::boolean
    (or (overlay:long-press! finger x y)
	(top:long-press! finger x y)))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (assert (empty? context))
    (or (overlay:key-typed! key-code context)
	(top:key-typed! key-code context)))
  )


(define-object (Point x y)::Drawable
  (define (draw!)
    (let ((painter ::Painter (the-painter)))
      (painter:draw-point! x y #xff0000))))

(define-object (Stroke)::Layer
  (define points ::List[Point] (ArrayList[Point]))

  (define source-pane ::Pane #!null)

  (define (draw!)::void
    (let ((painter ::Painter (the-painter)))
      (for i from 1 below (points:size)
        (let ((p0 ::Point (points (- i 1)))
	      (p1 ::Point (points i)))
          (painter:draw-line! p0:x p0:y p1:x p1:y)))))
  
  (IgnoreInput))

(define-object (Drawing stroke::Stroke)::Drag

  (define (move! x::real y::real dx::real dy::real)::void
    (stroke:points:add (Point x y)))

  (define (drop! x::real y::real vx::real vy::real)::void
    (screen:overlay:remove! stroke))

  (screen:overlay:add! stroke))

(define-object (Selected items::cons position::Position)::Layer

  (define (draw!)::void
    (parameterize ((the-document items))
      #;(when (pair? (car items))
	(let ((items-position (screen-position (car items))))
	  (set! items-position:left 0)
	  (set! items-position:top 0)))
      (with-translation (position:left position:top)
	(draw-sequence! items))))
  
  (IgnoreInput))

(define-object (DragAround selected::Selected)::Drag

  (define (move! x::real y::real dx::real dy::real)::void
    (let ((position ::Position selected:position))
      (set! position:left (+ position:left dx))
      (set! position:top (+ position:top dy))))

  (define (drop! x::real y::real vx::real vy::real)::void
    (and-let* ((cursor (cursor-under x y))
	       (`(,tip . ,precursor) cursor)
	       (parent ::Element (the-expression at: precursor))
	       (location ::Element (parent:part-at tip)))
      (cond
       ((isnt parent eq? location)
	(WARN "reached "location" in "parent" at "cursor))

       ((is parent Space?)
	(let* ((action ::Insert (Insert element: selected:items
					at: cursor))
	       (document (the-document))
	       (history ::History (history document)))
	  (history:record! action)
	  (set! (the-cursor) (action:apply! document))
	  (set! (the-selection-anchor) (the-cursor))))

       ((is parent cons?)
	(cond
	 ((eqv? tip (parent:first-index))
	  (insert! selected:items
		   at: (recons (parent:next-index tip)
			       cursor)))
	 ((eqv? tip (parent:last-index))
	  (insert! selected:items
		   at: (recons
			(parent:previous-index tip)
			cursor)))
	 (else
	  (WARN "unhandled "tip" in "parent)))))

      (screen:overlay:remove! selected)))

  (screen:overlay:add! selected))

(define-object (Resize box::cons path::Cursor anchor::real)::Drag

  (define position ::Position (screen-position box))

  (define initial ::Extent (copy (extent box)))

  (define ending ::LineEnding
    (line-ending-embracing anchor #;from box))

  (define (move! x::real y::real dx::real dy::real)::void
    (safely
     (let* ((target-width ::real (- x position:left))
	    (target-height ::real (+ initial:height
				     (- y position:top anchor))))
       (resize! box target-width target-height ending))))

  (define (drop! x::real y::real vx::real vy::real)::void
    (let ((final ::Extent (extent box))
	  (history ::History (history (the-document))))
      (when (isnt final equal? initial)
	(history:record! (ResizeBox at: path
				    from: initial
				    to: (copy final)
				    with-anchor: anchor)))))
  )

(define-object (Overlay)::Pane
  (define layers ::($bracket-apply$ List Layer)
    (($bracket-apply$ ArrayList Layer)))

  (define cursor ::(maps (Layer) to: Cursor)
    (property+ (layer::Layer)::Cursor
	       (cursor-climb-front '() layer)))
  
  (define (draw!)::void
    (for layer::Layer in-reverse layers
      (parameterize ((the-cursor (cursor layer)))
	(layer:draw!))))

  (define (add! element::Layer)::void
    (layers:add 0 element))

  (define (remove! element::Layer)::void
    (layers:remove element))

  (define (clear!)::void
    (layers:clear))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:tap! finger x y))
	 layers))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:press! finger x y))
	 layers))

  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:second-press! finger x y))
	 layers))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:double-tap! finger x y))
	 layers))

  (define (long-press! finger::byte x::real y::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:long-press! finger x y))
	 layers))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (let ((n ::int (+ 1 (* 2 (length layers)))))
      (call/cc
       (lambda (return)
	 (for layer::Layer in layers
	   (parameterize/update-sources ((the-cursor (cursor
						      layer)))
	     (when (layer:key-typed! key-code context)
	       (return #t))
	   (set! n (- n 2))))
	 #f))))
  )

(define-object (WrappedPane content ::Pane)::Pane

  (define (draw!)::void
    (content:draw!))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (content:tap! finger x y))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (content:press! finger x y))

  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (content:second-press! finger x y))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (content:double-tap! finger x y))

  (define (long-press! finger::byte x::real y::real)::boolean
    (content:long-press! finger x y))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (content:key-typed! key-code context))
  )

(define-enum HorizontalSplitFocus (Left Right))

(define-type (HorizontalSplit at: rational
			      left: Pane
			      right: Pane
			      focus: HorizontalSplitFocus
			      := HorizontalSplitFocus:Left)
  implementing Pane
  with
  ((draw!)::void
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (with-clip (left-width extent:height)
       (parameterize ((the-pane-extent
		       (Extent
			width: left-width
			height: extent:height)))
	 (left:draw!)))
     (with-translation (left-width 0)
       (invoke painter 'draw-vertical-split! 0)
       (with-translation (line-width 0)
	 (with-clip (right-width extent:height)
	   (parameterize ((the-pane-extent
			   (Extent
			    width: right-width
			    height: extent:height)))
	     (right:draw!)))))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:tap! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:tap! finger #;at (- x left-width line-width) y
			)))))

  ((press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:press! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:press! finger #;at (- x left-width line-width) y
			  )))))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:second-press! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:second-press! finger
			 #;at (- x left-width line-width) y)))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:double-tap! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:double-tap! finger
			 #;at (- x left-width line-width) y)))))

  ((long-press! finger::byte x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-split-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:long-press! finger #;at x y))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:long-press! finger
			 #;at (- x left-width line-width) y)))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-typed! key-code
		       (recons HorizontalSplitFocus:Left
			       context)))
     (,HorizontalSplitFocus:Right
      (right:key-typed! key-code
			(recons HorizontalSplitFocus:Right
				context)))))
  )


(define/kw (pop-up-action pop-up::PopUp finger::byte x::real y::real
			  inside: inner-action
			  ::(maps (Enchanted byte real real)
				  to: boolean)
			  := never
			  outside: outer-action
			  ::(maps (PopUp byte real real) to: boolean)
			  := always
			  on-the-edge: boundary-action
			  ::(maps (PopUp byte real real) to: boolean)
			  := always)
  (let* ((painter ::Painter (the-painter))
         (content ::Enchanted pop-up:content)
	 (left ::real pop-up:left)
	 (top ::real pop-up:top)
         (inner ::Extent (content:extent))
	 (horizontal ::real (painter:horizontal-popup-margin))
	 (vertical ::real (painter:vertical-popup-margin))
	 (inner-left ::real (+ left horizontal))
	 (inner-top ::real (+ top vertical))
	 (inner-right ::real (+ inner-left inner:width))
	 (inner-bottom ::real (+ inner-top inner:height))
	 (right ::real (+ inner-right horizontal))
	 (bottom ::real (+ inner-bottom vertical)))
    (cond ((and (is inner-left <= x < inner-right)
                (is inner-top <= y < inner-bottom))
	   (inner-action content finger
	          (- x inner-left) (- y inner-top)))
	  ((or (is x < left) (is x > right)
	       (is y < top) (is y > bottom))
	   (outer-action pop-up finger x y))
	  (else
	   (boundary-action pop-up finger x y)))))

(define-type (PopUp left: real := 0 top: real := 0
                    content: Enchanted)
  implementing Layer
  with
  ((draw!)::void
   (let ((tile ::Tile (as Tile (this))))
     (tile:draw! '())))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:press! finger x y))
     on-the-edge:
     (lambda (pop-up::PopUp finger::byte x::real y::real)
       ::boolean
       (screen:drag! finger pop-up))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:tap! finger x y))
     outside:
     (lambda (pop-up::PopUp finger::byte x::real y::real)
       ::boolean
       (screen:overlay:remove! pop-up))))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:second-press! finger x y))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (pop-up-action (this) finger x y
    inside:
    (lambda (content::Enchanted finger::byte x::real y::real)
      ::boolean
      (content:double-tap! finger x y))))

  ((long-press! finger::byte x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:long-press! finger x y))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (content:key-typed! key-code (recons (first-index) context)))

  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (content:extent))
	  (horizontal ::real (painter:horizontal-popup-margin))
	  (vertical ::real (painter:vertical-popup-margin)))
     (with-translation (left top)
       (painter:draw-popup! (+ inner:width (* 2 horizontal))
			    (+ inner:height (* 2 vertical)))
       (with-translation (horizontal vertical)
	 (content:draw! (recons 'content context))))))

  ((part-at index::Index)::Indexable*
   (match index
    ('edge (this))
    ('content content)))

  ((first-index)::Index 'content)
  ((last-index)::Index 'edge)

  ((next-index index::Index)::Index 'edge)
  ((previous-index index::Index)::Index 'content)

  ((index< a::Index b::Index)::boolean ;>
   (and (eq? a 'edge) (eq? b 'content)))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (call/cc
    (lambda (return)
     (pop-up-action (this) 0 x y
      inside:
      (lambda (content::Tile finger::byte x::real y::real)::boolean
        (return
	 (otherwise #!null
	   (and path
	     (content:cursor-under*
	      x y (recons 'content path))))))
      outside:
      (lambda (pop-up::PopUp finger::byte x::real y::real)::boolean
        (return #!null))
      on-the-edge:
      (lambda (pop-up::PopUp finger::byte x::real y::real)::boolean
        (return
	  (otherwise #!null
	    (and path (recons 'edge path)))))))))

  ((extent)::Extent
   (let ((painter ::Painter (the-painter))
	 (inner ::Extent (content:extent)))
     (Extent width: (+ inner:width
		       (* 2 (painter:horizontal-popup-margin)))
	     height: (+ inner:height
			(* 2 (painter:vertical-popup-margin))))))

  implementing Drag
  with
  ((move! x::real y::real dx::real dy::real)::void
   (set! left (+ left dx))
   (set! top (+ top dy)))

  ((drop! x::real y::real vx::real vy::real)::void
   ;; sprawdzic czy v jest wieksza niz prog,
   ;; i iesli tak - usunac (this) ze screen:overlay
   (values))

  implementing Enchanted
  with
  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression)))

(define-type (Scroll width: real
                     height: real
		     left: real := 0
		     top: real := 0
		     content: Enchanted)
  implementing Drag
  with
  ((move! x::real y::real dx::real dy::real)::void
   (let ((inner ::Extent (content:extent)))
     (set! left (max 0 (min (- inner:width width) (- left dx))))
     (set! top (max 0 (min (- inner:height height) (- top dy))))))

  ((drop! x::real y::real vx::real vy::real)::void
   (values))

  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:fill-background! width height)
     (with-clip (width height)
       (with-translation ((- left) (- top))
	 (content:draw! (recons 0 context))))))

  ((tap! finger::byte  x::real y::real)::boolean
   (content:tap! finger (+ x left) (+ y top)))

  ((press! finger::byte x::real y::real)::boolean
   (screen:drag! finger (this)))

  ((second-press! finger::byte #;at x::real y::real)::boolean
    (content:press! finger (+ x left) (+ y top)))

  ((double-tap! finger::byte x::real y::real)::boolean
    (content:double-tap! finger (+ x left) (+ y top)))

  ((long-press! finger::byte x::real y::real)::boolean
    (content:long-press! finger (+ x left) (+ y top)))

  ((key-typed! key-code::long context::Cursor)::boolean
    (content:key-typed! key-code (recons (first-index) context)))

  ((extent)::Extent
   (Extent width: width
           height: height))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (content:cursor-under* (- x left) (- y top) (recons 0 path)))

  ((part-at index::Index)::Indexable* content)

  ((first-index)::Index 0)

  ((last-index)::Index 0)

  ((next-index index::Index)::Index 0)

  ((previous-index index::Index)::Index 0)

  ((index< a::Index b::Index)::boolean #f)

  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression)))

(define (text-field width::real content::CharSequence)::Scroll
  (let* ((input ::TextInput (text-input content))
	 (inner ::Extent (input:extent))
	 (scroll ::Scroll (Scroll width: width
				  height: inner:height
				  content: input)))
    scroll))

(define (popup-scroll content::Enchanted)::PopUp
  (let* ((content ::Enchanted content)
	 (inner ::Extent (content:extent))
	 (scroll ::Scroll (Scroll width: inner:width
				  height: inner:height
				  content: content))
         (popup (PopUp content: scroll))
	 (outer ::Extent (popup:extent))
	 (available ::Extent (screen:size)))
    (set! scroll:width (- scroll:width
                          (max 0 (- outer:width
			            available:width))))
    (set! scroll:height (- scroll:height
                           (max 0 (- outer:height
			             available:height))))
    popup))

(define (file-list directory::java.io.File
                   file-action::(maps (java.io.File) to: void)
		   directory-action::(maps (java.io.File) to: void))
  ::Enchanted
  (let* ((filenames ::($bracket-apply$ String)
		    (directory:list))
         (n ::int (length filenames))
         (buttons ::($bracket-apply$ FileButton)
		 (($bracket-apply$ FileButton)
		  length: (+ n 1))))
    (set! (buttons 0) (ParentDirectoryButton
                       target: (directory:getParentFile)
		       action: directory-action))
    (for i from 0 below n
      (let ((file (java.io.File directory (filenames i))))
        (set! (buttons (+ i 1))
	     (if (file:isDirectory)
	       (DirectoryButton target: file
	                        action: directory-action)
               (FileButton target: file
                            action: file-action)))))
    (Array:sort buttons)
    (ColumnGrid buttons)))

(define (open-file-browser directory::java.io.File editor::Editor)
  ::PopUp
  (let ((window ::PopUp #!null))
    (set! window
	  (popup-scroll
	   (file-list directory
		      (lambda (file::java.io.File)
			::void
			(screen:overlay:clear!)
			(editor:load-file file))
		      (lambda (directory::java.io.File)
			::void
			(screen:overlay:remove! window)
			(screen:overlay:add!
			 (open-file-browser directory
					    editor))))))
    window))

(define (save-file-browser directory::java.io.File
                           name-hint::string
			                     editor::Editor)
  ::PopUp
  (let* ((window ::PopUp #!null)
         (text-field ::Scroll (text-field 0 name-hint))
         (button (Button label: "Save"
	                 action: (lambda _
			           (screen:overlay:clear!)
				   (save-document!
				    editor:document
				    (java.io.File
				     directory
				     text-field:content)))))
	 (files (file-list directory
	                   (lambda (file::java.io.File)::void
				   (set! text-field:content
					 (text-input
					  (file:getName))))
			   (lambda (dir::java.io.File)::void
				   (screen:overlay:remove!
				    window)
				   (screen:overlay:add!
				    (save-file-browser
				     dir
			             text-field:content
                                     editor)))))
	 (inner ::Extent (files:extent))
	 (browser ::Scroll (Scroll content: files
				   width: inner:width
				   height: inner:height))
	 (top (Beside left: text-field right: button))
	 (upper ::Extent (top:extent))
	 (content (Below top: top
                         bottom: browser))
         (popup (PopUp content: content))
	 (outer ::Extent (popup:extent))
	 (available ::Extent (screen:size))
	 (button-size ::Extent (button:extent)))
    (set! browser:width (- browser:width
                           (max 0 (- outer:width
			             available:width))))
    (set! browser:height (- browser:height
                            (max 0 (- outer:height
				      (- upper:height)
			              available:height))))
    (set! text-field:width (- browser:width
			      button-size:width))
    (set! window popup)
    (and-let* ((`(,tip . ,root) (screen:overlay:cursor popup)))
      (set! (screen:overlay:cursor popup)
	    (recons (text-field:content:last-index) root)))
    window))

(define (document-switcher editor::Editor)
  (let* ((choices (map (lambda (document::Document)
			 (Link content:
			       (Caption
				(if document:source
				    (document:source:getName)
				    "(unnamed)"))
			       on-tap: (lambda _
					 (screen:overlay:clear!)
					 (editor:switch-to!
					  document)
					 #t)))
		       (open-documents))))
    (popup-scroll (ColumnGrid choices))))

(define-object (Editor)::Pane
  (define document ::Document (Document (empty) #!null))
  (define cursor ::Cursor '())

  (define selection-anchor ::Cursor '())

  (define previously-edited
    (property (document::Document)
      ::Document
      (or (and-let* ((`(,_ ,next . ,_)
		      (first-cell (is (car _) eq? document)
				  (open-documents))))
	    next)
	  (and-let* ((`(,first . ,_) (open-documents))
		     ((isnt first eq? document)))
	    first)
	  document)))

  (define (switch-to! target::Document)::void
    (unless (eq? target document)
      (set! (previously-edited target) document)
      (set! document target)))
  
  (define (load-file file::java.io.File)::void
    (let ((opened ::Document (open-document file)))
      (set! (previously-edited opened) document)
      (set! document opened)))

  (define (draw!)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-selection-anchor selection-anchor))
      (document:draw! '())
      #;(draw-sequence! (head document))))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (WARN "tap! "finger" "x" "y)
    (parameterize/update-sources ((the-document document))

      (let* ((target-cursor (cursor-under x y))
	     (target (the-expression at: target-cursor)))
	(DUMP target)
	(match target
	  (enchanted::Interactive
	   (enchanted:tap! finger x y))
	  (else
	   (set! cursor target-cursor)
	   (set! selection-anchor cursor)

	   (display cursor)
	   (display (the-expression at: cursor))
	   (newline)
	   #t)))))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (WARN "press! "finger" "x" "y)
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (let-values (((selection-start selection-end)
		    (the-selection)))
	(and-let* ((path (cursor-under x y))
		   (`(,tip . ,subpath) path)
		   (parent ::Element (the-expression
				      at: subpath))
		   (target ::Element (parent:part-at tip))
		   (position ::Position (screen-position
					 target)))
	  (cond
	   #;((isnt parent eq? target)
	    (WARN "reached non-final item on press"))

	   #;((isnt dragging clean?)
	    (WARN "should start scrolling or zooming "
		  (keys dragging)))

	   ((is target Space?)
	    (WARN "drawing a stroke")
	    (screen:drag! finger
			  (Drawing (Stroke source-pane: (this)))))

	   ((is selection-start cursor< path
		cursor< selection-end)
	    (WARN "should move selection"))

	   ((or (is target Atom?)
		(and (is target cons?)
		     (eqv? tip (target:first-index)))
		(is target EmptyListProxy?))
	    ;; powinnismy powiekszyc spacje poprzedzajaca
	    ;; wydobywany element o szerokosc tego elementu
	    ;; podzielona przez (painter:space-width)
	    (set! (the-cursor) (cursor-climb-back
				(cursor-retreat (tail path))))
	    (set! (the-selection-anchor) (the-cursor))
	    (let* ((removed ::Remove (remove-element! at: subpath))
		   (position (screen-position (head removed:element)))
		   (selection (Selected removed:element
					(copy position))))
	      (unset! (screen-position removed:element))
	      (unset! (screen-position (head removed:element)))
	      (screen:drag! finger (DragAround selection))))

	   ((and (is target cons?)
		 (eqv? tip (target:last-index)))
	    (let ((extent ::Extent (extent target)))
	      (screen:drag! finger
			    (Resize target subpath
				    (- y position:top)))))
	   (else
	    (WARN "setting the cursor to "path)
	    (set! (the-cursor) path)
	    (set! (the-selection-anchor) path)
	     )))
	#t)))

  (define (second-press! finger::byte #;at x::real y::real)
    ::boolean
    (WARN "second-press! "finger" "x" "y)
    ;; powinnismy sobie skopiowac dany element
    ;; albo zaczac scrollowanie
    #f)

  (define (double-tap! finger::byte x::real y::real)::boolean
    (WARN "double-tap! "finger" "x" "y)
    ;; centrowanie na dokumencie/maksymalizacja wyrazenia
    #f)

  (define (long-press! finger::byte x::real y::real)::boolean
    (safely
     (invoke (current-message-handler) 'clear-messages!)
     (let* ((content
	     ::Enchanted
	     (ColumnGrid
	      (list
	       (Link content: (Caption "New")
		     on-tap: (lambda _ (WARN "New") #t))
	       (Link content: (Caption "Open...")
		     on-tap: (lambda _
			       (let ((keeper ::Keeper
					     (the-keeper)))
				 (keeper:with-read-permission
				  (lambda ()
				    (screen:overlay:add!
				     (open-file-browser
				      (keeper:initial-directory)
				      (this))))))))
	       (Link content: (Caption "Switch to...")
		     on-tap: (lambda _
			       (screen:overlay:add!
				(document-switcher (this)))
				#t))
	       (Link content: (Caption "Save as...")
		     on-tap: (lambda _
			       (let ((keeper ::Keeper
					     (the-keeper)))
				 (keeper:with-write-permission
				  (lambda ()
				    (safely
				     (screen:overlay:add!
				      (save-file-browser
				       (keeper:initial-directory)
				       "filename.scm"
				       (this)))))))
			       #t))
	       (Link content: (Caption "Close")
		     on-tap: (lambda _ (WARN "Close") #t))
	       )))
	    (inner ::Extent (content:extent))
	    (window ::PopUp (PopUp content: content))
	    (inner ::Extent (window:extent))
	    (outer ::Extent (screen:size)))
       (set! window:left
	     (max 0 (min (- outer:width inner:width)
			 (- x (quotient inner:width 2)))))
       (set! window:top
	     (max 0 (min (- outer:height inner:height)
			 (- y (quotient inner:height 2)))))
       (screen:overlay:add! window)))
    ;; dodanie menu kontekstowego
    #t)

  (define (key-typed! key-code::long context::Cursor)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((keymap key-code))))

  )


(define-early-constant screen ::Screen
  (Screen))

;; At the top level, (the-pane-extent)
;; must be bound to the same object
;; as (screen:size)
;;
(define-parameter (the-pane-extent)::Extent
  (screen:size))
