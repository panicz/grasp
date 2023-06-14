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

(define-interface Drawable ()
  (draw!)::void
  )

(define-interface Pane (Drawable Interactive))

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
  
(define-interface Screen (Resizable Pane)
  (release! finger::byte x::real y::real vx::real vy::real)::boolean
  (move! finger::byte x::real y::real dx::real dy::real)::boolean
  (overlay! element::Pane)::void
  (remove-overlay! element::Pane)::void
  (drag! finger::byte action::Drag)::void
  (set-content! content::Pane)::void
  (content)::Pane
  )


(define-object (Point x y)::Drawable
  (define (draw!)
    (let ((painter ::Painter (the-painter)))
      (painter:draw-point! x y #xff0000))))

(define-object (Stroke)::Pane
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
    (screen:remove-overlay! stroke))

  (screen:overlay! stroke))

(define-object (Selected items::cons position::Position)::Pane
  
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
      
      (screen:remove-overlay! selected)))

  (screen:overlay! selected))

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
  (define elements :: List[Pane] (ArrayList[Pane]))
  
  (define (draw!)::void
    (for element::Pane in-reverse elements
      (element:draw!)))
  
  (define (add! element::Pane)::void
    (elements:add 0 element))
  
  (define (remove! element::Pane)::void
    (elements:remove element))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (any (lambda (element::Pane)
	   (element:tap! finger x y))
	 elements))
  
  (define (press! finger::byte #;at x::real y::real)::boolean
    (any (lambda (element::Pane)
	   (element:press! finger x y))
	 elements))
  
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (any (lambda (element::Pane)
	   (element:second-press! finger x y))
	 elements))
  
  (define (double-tap! finger::byte x::real y::real)::boolean
    (any (lambda (element::Pane)
	   (element:double-tap! finger x y))
	 elements))
  
  (define (long-press! finger::byte x::real y::real)::boolean
    (any (lambda (element::Pane)
	   (element:long-press! finger x y))
	 elements))
  
  (define (key-typed! key-code::long)::boolean
    (any (lambda (element::Pane)
	   (element:key-typed! key-code))
	 elements))
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
  
  (define (key-typed! key-code::long)::boolean
    (content:key-typed! key-code))
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

  ((key-typed! key-code::long)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-typed! key-code))
     (,HorizontalSplitFocus:Right
      (right:key-typed! key-code))))
  )

(define-object (ActualScreen)::Screen
  (define overlay ::Overlay (Overlay))
  (define dragging ::(maps byte to: Drag)
    (mapping (finger::byte)::Drag #!null))

  (define top ::Pane (NullPane))

  ;; this parameter must be set by the
  ;; graphical framework (Lanterna, AWT, ...)
  ;; and changed every time the hosting
  ;; window is resized
  (define extent ::Extent (Extent width: 0 height: 0))

  (define (overlay! element::Pane)::void
    (overlay:add! element))
  
  (define (remove-overlay! element::Pane)::void
    (overlay:remove! element))
  
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
  
  (define (release! finger::byte x::real y::real vx::real vy::real)
    ::boolean
    (and-let* ((drag ::Drag (dragging finger)))
      (drag:drop! x y vx vy)
      (unset! (dragging finger))
      #t))
    
  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean
    (and-let* ((drag ::Drag (dragging finger)))
      (drag:move! x y dx dy)
      #t))
    
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (or (overlay:second-press! finger x y)
	(top:second-press! finger x y)))
  
  (define (double-tap! finger::byte x::real y::real)::boolean
    (or (overlay:double-tap! finger x y)
	(top:double-tap! finger x y)))
    
  (define (long-press! finger::byte x::real y::real)::boolean
    (or (overlay:long-press! finger x y)
	(top:long-press! finger x y)))
  
  (define (key-typed! key-code::long)::boolean
    (or (overlay:key-typed! key-code)
	(top:key-typed! key-code)))
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
  implementing Pane
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
       (screen:remove-overlay! pop-up))))
  
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
  
  ((key-typed! key-code::long)::boolean
   (content:key-typed! key-code))
		    
  implementing Tile
  with
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
  
  ((first-index)::Index 'edge)
  ((last-index)::Index 'content)
  
  ((next-index index::Index)::Index 'content)
  ((previous-index index::Index)::Index 'edge)

  ((index< a::Index b::Index)::boolean ;>
   (and (eq? a 'content) (eq? b 'edge)))

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
   (with-clip (width height)
     (with-translation ((- left) (- top))
       (content:draw! (recons 0 context)))))

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
    
  ((key-typed! key-code::long)::boolean
    (content:key-typed! key-code))

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


(define-object (Editor)::Pane
  (define document (cons '() '()))
  (define cursor :: Cursor '())

  (define selection-anchor :: Cursor '())
  
  (define (draw!)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-selection-anchor selection-anchor))
      (draw-sequence! (head document))))
  
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
     (let* ((content ::Enchanted
		     (ColumnGrid
		      (list
		       (Link content: (Caption "New")
			     on-tap: (lambda _ (WARN "New") #t))
		       (Link content: (Caption "Open...")
			     on-tap: (lambda _ (WARN "Open") #t))
		       (Link content: (Caption "Switch to...")
			     on-tap: (lambda _ (WARN "Switch to...")
					     #t))
		       (Link content: (Caption "Save as...")
			     on-tap: (lambda _ (WARN "Save") #t))
		       (Link content: (Caption "Close")
			     on-tap: (lambda _ (WARN "Close") #t))
		       )))
	    (inner ::Extent (content:extent))
	    (content ::Enchanted (Scroll width:
					 inner:width
					 height: (quotient
						  inner:height
						  2)
					 content: content))
	    (window ::PopUp (PopUp content: content))
	    (inner ::Extent (window:extent))
	    (outer ::Extent (screen:size)))
       (set! window:left
	     (max 0 (min (- outer:width inner:width)
			 (- x (quotient inner:width 2)))))
       (set! window:top
	     (max 0 (min (- outer:height inner:height)
			 (- y (quotient inner:height 2)))))
       (screen:overlay! window)))
     ;; dodanie menu kontekstowego
     #t)
  
  (define (key-typed! key-code::long)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((keymap key-code))))

  )


(define-early-constant screen ::Screen
  (ActualScreen))

;; At the top level, (the-pane-extent)
;; must be bound to the same object
;; as (screen:size)
;;
(define-parameter (the-pane-extent)::Extent
  (screen:size))
