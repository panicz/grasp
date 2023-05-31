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

(define-interface Drawable ()
  (draw!)::void
  )

(define-interface InputReceiver ()
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean
  (move! finger::byte x::real y::real dx::real dy::real)::boolean
  (release! finger::byte x::real y::real vx::real vy::real)::boolean
  (second-press! finger::byte #;at x::real y::real)::boolean
  (double-tap! finger::byte x::real y::real)::boolean
  (long-press! finger::byte x::real y::real)::boolean
  
  (key-typed! key-code::long)::boolean
  )

(define-interface Pane (Drawable InputReceiver))

(define-interface Drag ()
  (move! x::real y::real dx::real dy::real)::void
  (drop! x::real y::real vx::real vy::real)::void
  )

(define-object (IgnoreInput)::InputReceiver
  (define (tap! finger::byte #;at x::real y::real)::boolean #f)
  (define (press! finger::byte #;at x::real y::real)::boolean #f)
  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean #f)
  (define (release! finger::byte x::real y::real vx::real vy::real)
    ::boolean #f)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #f)
  (define (double-tap! finger::byte x::real y::real)::boolean #f)
  (define (long-press! finger::byte x::real y::real)::boolean #f)
  
  (define (key-typed! key-code::long)::boolean #f))

(define-object (ConsumeInput)::InputReceiver
  (define (tap! finger::byte #;at x::real y::real)::boolean #t)
  (define (press! finger::byte #;at x::real y::real)::boolean #t)
  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean #t)
  (define (release! finger::byte x::real y::real vx::real vy::real)
    ::boolean #t)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #t)
  (define (double-tap! finger::byte x::real y::real)::boolean #t)
  (define (long-press! finger::byte x::real y::real)::boolean #t)
  (define (key-typed! key-code::long)::boolean #t))

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
    (screen:overlay:remove! stroke))

  (screen:overlay:add! stroke))

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

  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean
    (any (lambda (element::Pane)
	   (element:move! finger x y dx dy))
	 elements))
  
  (define (release! finger::byte x::real y::real vx::real vy::real)
    ::boolean
    (any (lambda (element::Pane)
	   (element:release! finger x y vx vy))
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
  
(define-object (WrappedPane content ::Pane)::Pane

  (define (draw!)::void
    (content:draw!))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (content:tap! finger x y))
  
  (define (press! finger::byte #;at x::real y::real)::boolean
    (content:press! finger x y))
  
  (define (release! finger::byte x::real y::real
		    vx::real vy::real)
    ::boolean
    (content:release! finger x y vx vy))
  
  (define (move! finger::byte x::real y::real
		 dx::real dy::real)
    ::boolean
    (content:move! finger x y dx dy))
  
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (content:second-press! finger x y))
  
  (define (double-tap! finger::byte x::real y::real)::boolean
    (content:double-tap! finger x y))
  
  (define (long-press! finger::byte x::real y::real)::boolean
    (content:long-press! finger x y))
  
  (define (key-typed! key-code::long)::boolean
    (content:key-typed! key-code))
  )

(define-parameter (the-focus)::Cursor '())

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
	 (left:draw!)))
     (with-translation (left-width 0)
       (invoke painter 'draw-vertical-line! 0)
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

  ((move! finger::byte #;to x::real y::real
          #;by dx::real dy::real)
   ::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
          (line-width (invoke painter 'vertical-line-width))
          (inner-width (- extent:width line-width))
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

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter (the-painter))
	  (extent (the-pane-extent))
	  (line-width (invoke painter 'vertical-line-width))
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
	  (line-width (invoke painter 'vertical-line-width))
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
	  (line-width (invoke painter 'vertical-line-width))
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

(define-object (Screen)::Pane
  (define overlay ::Overlay (Overlay))
  (define dragging ::(maps byte to: Drag)
    (mapping (finger::byte)::Drag #!null))

  (define top ::Pane (Editor))

  ;; this parameter must be set by the
  ;; graphical framework (Lanterna, AWT, ...)
  ;; and changed every time the hosting
  ;; window is resized
  (define extent ::Extent (Extent width: 0 height: 0))
  
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
    (or (and-let* ((drag ::Drag (dragging 0)))
	  (drag:drop! x y vx vy)
	  (unset! (dragging finger))
	  #t)
	(overlay:release! finger x y vx vy)
	(top:release! finger x y vx vy)))
    
  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean
    (let ((result ::boolean #f))
      (for finger from 0 below 10
	   (set! result
		 (or (and-let* ((drag ::Drag (dragging finger)))
		       (drag:move! x y dx dy)
		       #t)
		     result)))
      result))
    
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
	   (enchanted:tapped x y))
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
		   (dragging ::(maps finger to: Drag)
			     screen:dragging)
		   (position ::Position (screen-position
					 target)))
	  (cond
	   #;((isnt parent eq? target)
	    (WARN "reached non-final item on press"))
	   
	   ((isnt dragging clean?)
	    (WARN "should start scrolling or zooming"))
	   
	   ((is target Space?)
	    (set! (dragging finger)
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
	      (set! (dragging finger) (DragAround selection))))

	   ((and (is target cons?)
		 (eqv? tip (target:last-index)))
	    (let ((extent ::Extent (extent target)))
	      (set! (dragging finger)
		    (Resize target subpath (- y position:top)))))
	   (else
	    (WARN "setting the cursor to "path)
	    (set! (the-cursor) path)
	    (set! (the-selection-anchor) path)
	     )))
	#t)))

  (define (move! finger::byte x::real y::real dx::real dy::real)
    ::boolean
    #f)
  
  (define (release! finger::byte #;at x::real y::real
		    #;with vx::real vy::real)
    ::boolean
    ;;(WARN "release! "finger" "x" "y" "vx" "vy)
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-selection-anchor
				   selection-anchor))
      (or
       #;(and-let* ((drag ::Drag (dragging 0)))
	 (drag:drop! x y vx vy)
	 (unset! (dragging finger))
	 #t)
       (and-let* ((cursor (cursor-under x y)))
	 (set! (the-cursor) cursor)
	 (set! (the-selection-anchor) cursor)))))

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
    (invoke (current-message-handler) 'clear-messages!)
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
  (Screen))

;; At the top level, (the-pane-extent)
;; must be bound to the same object
;; as screen:extent.
;;
(define-parameter (the-pane-extent)::Extent
  screen:extent)
