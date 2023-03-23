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

(define-alias List java.util.List)
(define-alias ArrayList java.util.ArrayList)

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real vy::real)::void
  )

(define-interface Drawable ()
  (draw!)::void
  )

(define-object (Point x y)::Drawable
  (define (draw!)
    (let ((painter ::Painter (the-painter)))
      (painter:draw-point! x y #xff0000))))

(define-object (Stroke)::Drawable
  (define points ::List[Point] (ArrayList[Point]))
  
  (define (draw!)::void
    (let ((painter ::Painter (the-painter)))
      (for i from 1 below (points:size)
        (let ((p0 ::Point (points (- i 1)))
	      (p1 ::Point (points i)))
          (painter:draw-line! p0:x p0:y p1:x p1:y)))))
  
  (define (add-point! p::Point)::void
    (points:add p)))

(define-object (Gesture)::Drawable
  (define strokes ::List[Stroke] (ArrayList[Stroke]))

  (define (draw!)::void
    (for stroke::Stroke in strokes
      (stroke:draw!)))

  (define (clear!)::void
    (strokes:clear))
  
  (define (add-stroke! s::Stroke)::void
    (strokes:add s)))

(define-object (Overlay)::Drawable
  (define elements :: List[Drawable] (ArrayList[Drawable]))
  
  (define (draw!)::void
    (for element::Drawable in elements
      (element:draw!)))
  
  (define (add! element::Drawable)::void
    (elements:add element))
  
  (define (remove! element::Drawable)::void
    (elements:remove element))
  )

(define-early-constant the-overlay ::Overlay (Overlay))

(define-object (Selected items::cons position::Position)::Drawable
  
  (define (draw!)::void
    (parameterize ((the-document items))
      #;(when (pair? (car items))
	(let ((items-position (screen-position (car items))))
	  (set! items-position:left 0)
	  (set! items-position:top 0)))
      (with-translation (position:left position:top)
	  (draw-sequence! items)))))

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
      
      (the-overlay:remove! selected)))

  (the-overlay:add! selected))

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

(define-mapping (dragging finger::byte)::Drag #!null)

(define-interface Pane ()
  (draw! context::Cursor)::void
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean

  (release! finger::byte #;at x::real y::real
	    #;with vx::real vy::real)
  ::boolean
  
  (move! finger::byte #;to x::real y::real
	 #;by vx::real vy::real)
  ::boolean
  
  (key-typed! key-code::long)::boolean
  )


;; this parameter must be set by the
;; graphical framework (Lanterna, AWT, ...)
;; and changed every time the hosting
;; window is resized

(define-parameter (the-screen-extent)::Extent
  (Extent width: 0
	  height: 0))

;; At the top level, (the-pane-extent)
;; must be bound to the same object
;; as (the-screen-extent).
;;
(define-parameter (the-pane-extent)::Extent
  (the-screen-extent))

(define-parameter (the-focus)::Cursor '())

(define-enum HorizontalSplitFocus (Left Right))

(define-type (HorizontalSplit at: rational
			      left: Pane
			      right: Pane
			      focus: HorizontalSplitFocus
			      := HorizontalSplitFocus:Left)
  implementing Pane
  with
  ((draw! context::Cursor)::void
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (with-clip (left-width extent:height)
       (parameterize ((the-pane-extent
		       (Extent
			width: left-width
			height: extent:height)))
	 (invoke left 'draw!
		 (recons 'left context))))
     (with-translation (left-width 0)
       (invoke painter 'draw-vertical-line! 0)
       (with-translation (line-width 0)
	 (with-clip (right-width extent:height)
	   (parameterize ((the-pane-extent
			   (Extent
			    width: right-width
			    height: extent:height)))
	     (invoke right 'draw!
		     (recons 'right context))))))))
  ((tap! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-line-width))
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
	  (line-width (invoke painter 'vertical-line-width))
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

  ((release! finger::byte #;at x::real y::real
	     #;with vx::real vy::real)
   ::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:release! finger #;at x y #;with vx vy))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:release! finger
			    #;at (- x left-width line-width) y
				 #;with vx vy)))))

  ((move! finger::byte #;to x::real y::real
	  #;by dx::real dy::real)
   ::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width
			  line-width))
          (left-width (* at inner-width))
          (right-width (- inner-width left-width)))
     (cond ((is x < left-width)
	    (set! focus HorizontalSplitFocus:Left)
	    (left:move! finger #;to x y #;by dx dy))
	   ((is (+ left-width line-width) < x)
	    (set! focus HorizontalSplitFocus:Right)
	    (right:move! finger
			 #;to (- x left-width line-width) y
			      #;by dx dy)))))
  
  ((key-typed! key-code::long)::boolean
   (match focus
     (,HorizontalSplitFocus:Left
      (left:key-typed! key-code))
     (,HorizontalSplitFocus:Right
      (right:key-typed! key-code))))
  )

(define-object (Editor)::Pane
  (define document (cons '() '()))
  (define cursor :: Cursor '())

  (define selection-anchor :: Cursor '())
  
  (define (draw! context::Cursor)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-selection-anchor selection-anchor))
      (draw-sequence! (head document))))
  
  (define (tap! finger::byte #;at x::real y::real)::boolean
    (parameterize/update-sources ((the-document document))
      (set! cursor (cursor-under x y))
      (set! selection-anchor cursor)
      (display cursor)
      (display (the-expression at: cursor))
      (newline)
      #t))

  (define (press! finger::byte #;at x::real y::real)::boolean
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
	   
	   ((isnt dragging clean?)
	    (WARN "should start scrolling or zooming"))
	   
	   ((is target Space?)
	    (WARN "should start drawing a gesture"))
	   
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
	      (set! (dragging 0) (DragAround selection))))

	   ((and (is target cons?)
		 (eqv? tip (target:last-index)))
	    (let ((extent ::Extent (extent target)))
	      (set! (dragging 0)
		    (Resize target subpath (- y position:top)))))
	   (else
	    (WARN "setting the cursor to "path)
	    (set! (the-cursor) path)
	    (set! (the-selection-anchor) path)
	     )))
	#t)))

  (define (release! finger::byte #;at x::real y::real
		    #;with vx::real vy::real)
    ::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (or
       (and-let* ((drag ::Drag (dragging 0)))
	 (drag:drop! x y vx vy)
	 (unset! (dragging 0))
	 #t)
       (and-let* ((cursor (cursor-under x y)))
	 (set! (the-cursor) cursor)
	 (set! (the-selection-anchor) cursor)))))

  (define (move! finger::byte #;to x::real y::real
		 #;by dx::real dy::real)
    ::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (and-let* ((drag ::Drag (dragging 0)))
	(drag:move! x y dx dy)
	#t)))

  (define (key-typed! key-code::long)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      ((keymap key-code))))

  )
  
(define-parameter (the-screen) ::Pane
  (Editor))
