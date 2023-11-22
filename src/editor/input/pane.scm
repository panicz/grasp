(module-name (editor input pane))

(import (srfi :17))
(import (srfi :11))
(import (utils hash-table))
(import (language define-property))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-parameter))
(import (language define-cache))
(import (language keyword-arguments))
(import (language mapping))
(import (language infix))
(import (language match))
(import (utils functions))
(import (language for))
(import (language while))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor document cursor))
(import (editor types extensions interactions))
(import (editor types primitive))
(import (editor document documents))
(import (editor types extensions extensions))

(import (editor document parse))
(import (utils conversions))
(import (editor interfaces painting))
(import (utils print))
(import (language parameterize-up))
(import (editor document document-operations))
(import (editor types spaces))
(import (editor input input))
(import (editor document history-tracking))
(import (editor types extensions widgets))
(import (editor document documents))
(import (editor types extensions combinators))
(import (editor input transforms))
(import (editor input gestures))

(define-alias Array java.util.Arrays)

(define-object (Screen)::ResizablePane
  (define overlay ::Overlay (Overlay))
  (define dragging ::(maps byte to: Drag)
    (mapping (finger::byte)::Drag #!null))

  (define top ::Embeddable (NullPane))
  
  ;; this parameter must be set by the
  ;; graphical framework (Lanterna, AWT, ...)
  ;; and changed every time the hosting
  ;; window is resized
  (define extent ::Extent (Extent width: 0 height: 0))

  (define (set-painter! p::Painter)::void
    (set! painter p))
  
  (define (drag! finger::byte action::Drag)::void
    (set! (dragging finger) action))

  (define (set-size! width::real height::real)::void
    (set! extent:width width)
    (set! extent:height height)
    (set! (the-pane-width) width)
    (set! (the-pane-height) height)
    )

  (define (size)::Extent extent)

  (define (set-content! content::Embeddable)::void
    (set! top content))

  (define (content)::Pane top)

  (define (draw!)::void
    (reset! extent-cached?)
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (top:draw!)
      (overlay:draw!)))

  (define after-tap ::(list-of (maps (byte real real) to: void)) '())
  
  (define (tap! finger::byte #;at x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (let ((result (or (overlay:tap! finger x y)
			(top:tap! finger x y))))
	(for hook::(maps (byte real real) to: void) in after-tap
	     (hook finger x y))
	result)))
  
  (define (press! finger::byte #;at x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:press! finger x y)
	  (top:press! finger x y))))

  (define (release! finger::byte x::real y::real
		    vx::real vy::real)
    ::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (and-let* ((drag ::Drag (dragging finger)))
	(drag:drop! x y vx vy)
	(unset! (dragging finger))
	#t)))

  (define (move! finger::byte x::real y::real
		 dx::real dy::real)
    ::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (and-let* ((drag ::Drag (dragging finger)))
	(drag:move! x y dx dy)
	#t)))

  (define (second-press! finger::byte #;at x::real y::real)
    ::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:second-press! finger x y)
	  (top:second-press! finger x y))))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:double-tap! finger x y)
	  (top:double-tap! finger x y))))

  (define (long-press! finger::byte x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:long-press! finger x y)
	  (top:long-press! finger x y))))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (assert (empty? context))
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:key-typed! key-code context)
	  (top:key-typed! key-code context))))

  (define (can-split-beside? line::Area)::boolean
    (top:can-split-beside? line))
  
  (define (split-beside! line::Area)::void
    (set! top (top:split-beside! line)))
  
  (define (can-split-below? line::Area)::boolean
    (top:can-split-below? line))
  
  (define (split-below! line::Area)::void
    (set! top (top:split-below! line)))

  (define (scroll-up! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:scroll-up! x y)
	  (top:scroll-up! x y))))
  
  (define (scroll-down! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:scroll-down! x y)
	  (top:scroll-down! x y))))
  
  (define (scroll-left! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:scroll-left! x y)
	  (top:scroll-left! x y))))
  
  (define (scroll-right! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:scroll-right! x y)
	  (top:scroll-right! x y))))

  (define (zoom-in! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:zoom-in! x y)
	  (top:zoom-in! x y))))
  
  (define (zoom-out! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:zoom-out! x y)
	  (top:zoom-out! x y))))

  (define (rotate-left! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:rotate-left! x y)
	  (top:rotate-left! x y))))
  
  (define (rotate-right! x::real y::real)::boolean
    (parameterize ((the-pane-width extent:width)
		   (the-pane-height extent:height))
      (or (overlay:rotate-right! x y)
	  (top:rotate-right! x y))))
  )

(define-object (Stroke finger ::byte source-pane ::Pane)::Layer
  (define points ::List[Position] (ArrayList[Position]))

  (define (add-point! p::Position)::void
    (points:add p))
  
  (define (draw!)::void
    (for i from 1 below (points:size)
        (let ((p0 ::Position (points (- i 1)))
	      (p1 ::Position (points i)))
          (painter:draw-line! p0:left p0:top p1:left p1:top))))
  
  (IgnoreInput))

(define-object (Drawing stroke::Stroke)::Drag

  (define (move! x::real y::real dx::real dy::real)::void
    (stroke:add-point! (Position left: x top: y)))

  (define (drop! x::real y::real vx::real vy::real)::void
    (safely
     (escape-with break
       (for recognizer::Recognizer in (the-recognizers)
	 (call-with-values
	     (lambda ()
	       (recognizer:recognizes stroke:points))
	   (lambda result
	     (and-let* ((`(,value . ,rest) result)
			(value))
	       (apply recognizer:action recognizer
		      stroke:points result)
	       (break)))))))
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
    ;; musimy sobie przetransformowac wspolrzedne
    ;; do wspolrzednych edytora oraz wybrac dokument
    (and-let* ((editor ::Editor (screen:top:pane-under x y))
	       (xe ye (screen:top:outside-in x y))
	       (xd yd (editor:transform:outside-in xe ye))
	       (cursor (cursor-under xd yd editor:document context: '()))
	       (`(,tip . ,precursor) cursor)
	       (parent ::Element (the-expression
				  at: precursor in: editor:document))
	       (location ::Element (parent:part-at tip)))
      (parameterize/update-sources ((the-document editor:document)
				    (the-cursor editor:cursor))
	(cond
	 ((isnt parent eq? location)
	  (WARN "reached "location" in "parent" at "cursor))

	 ((is parent Space?)
	  (let* ((action ::Insert (Insert element: selected:items
					  at: cursor))
		 (history ::History (history editor:document)))
	    (history:record! action)
	    (set! (the-cursor) (action:apply! editor:document))
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

	(screen:overlay:remove! selected))))

  (screen:overlay:add! selected))

(define-object (Resize box::cons path::Cursor anchor::real
		       editor::Editor)::Drag

  (define position ::Position (screen-position box))

  (define initial ::Extent (copy (extent+ box)))

  (define ending ::LineEnding
    (line-ending-embracing anchor #;from box))

  (define (move! x::real y::real dx::real dy::real)::void
    (safely
     (let*-values (((x y) (editor:transform:outside-in x y))
		   ((target-width target-height)
		    (values (- x position:left)
			    (+ initial:height
			       (- y position:top anchor)))))
       (resize! box target-width target-height ending))))

  (define (drop! x::real y::real vx::real vy::real)::void
    (let ((final ::Extent (extent+ box))
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
      (escape-with return
	(for layer::Layer in layers
	  (parameterize/update-sources ((the-cursor (cursor
						     layer)))
	    (when (layer:key-typed! key-code context)
	      (return #t))
	    (set! n (- n 2))))
	#f)))

  (define (scroll-up! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:scroll-up! left top))
	 layers))
  
  (define (scroll-down! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:scroll-down! left top))
	 layers))
  
  (define (scroll-left! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:scroll-left! left top))
	 layers))
  
  (define (scroll-right! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:scroll-right! left top))
	 layers))

  (define (zoom-in! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:zoom-in! left top))
	 layers))
  
  (define (zoom-out! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:zoom-out! left top))
	 layers))

  (define (rotate-left! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:rotate-left! left top))
	 layers))
  
  (define (rotate-right! left::real top::real)::boolean
    (any (lambda (layer::Layer)
	   (layer:rotate-right! left top))
	 layers))
  
  )

(define-enum SplitFocus (First Last))

(define-parameter (the-split-path)
  ::(list-of SplitFocus)
  '())

(define-type (Split at: real := 0.5
		    first: Embeddable
		    last: Embeddable
		    focus: SplitFocus := SplitFocus:First)
  implementing Embeddable
  with

  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   #!abstract)
     
  ((part-sizes)::(Values real real real)
    #!abstract)

  ((with-pane-size size::real action::procedure)::Object
   #!abstract)

  ((with-pane-translation shift::real action::procedure)::Object
   #!abstract)
  
  ((area/last original::Area earlier-size::real)::Area
   #!abstract)
  
  ((varying-dimension x::real y::real)::real
   #!abstract)

  ((varying-size w::real h::real size::real)::(Values real real)
   #!abstract)
  
  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   #!abstract)

  ((part focus::SplitFocus)::Embeddable
   (match focus
     (,SplitFocus:First first)
     (,SplitFocus:Last last)))

  ((draw-line!)::void
   #!abstract)

  ((draw!)::void
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-translation first-size
       (lambda ()
	 (draw-line!)
	 (with-pane-translation line-size
	   (lambda ()
	     (with-pane-size last-size
	       (lambda ()
		 (parameterize ((the-split-path (recons
						 SplitFocus:Last
						 (the-split-path))))
		   (with-clip ((the-pane-width) (the-pane-height))
		     (last:draw!)))))))))
     (with-pane-size first-size
       (lambda ()
	 (parameterize ((the-split-path (recons
					 SplitFocus:First
					 (the-split-path))))
	   (with-clip ((the-pane-width) (the-pane-height))
	     (first:draw!)))
	 ))))
  
  ((propagate action::procedure x::real y::real default::procedure)
   (let-values (((first-size line-size last-size) (part-sizes))
		((pos) (varying-dimension x y)))
     (cond ((is pos < first-size)
	    (with-pane-size first-size
	      (lambda ()
		(parameterize ((the-split-path (recons
						SplitFocus:First
						(the-split-path))))
		  (action first x y)))))
	   ((is (+ first-size line-size) < pos)
	    (with-pane-size last-size
	      (lambda ()
		(parameterize ((the-split-path (recons
						SplitFocus:Last
						(the-split-path))))
		  (let-values (((x* y*) (transformed-dimension
					 x y (+ first-size
						line-size))))
		    (action last x* y*))))))
	   (else
	    (with-pane-size line-size
	      (lambda ()
		(let-values (((x* y*) (transformed-dimension
				       x y first-size)))
		  (default (this) x* y*))))))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (match focus
       (,SplitFocus:First
	(with-pane-size first-size
	  (lambda ()
	    (parameterize ((the-split-path (recons
					    SplitFocus:First
					    (the-split-path))))
	      (first:key-typed! key-code
				(recons SplitFocus:First
					context))))))
       (,SplitFocus:Last
	(with-pane-size last-size
	  (lambda ()
	    (parameterize ((the-split-path (recons
					    SplitFocus:Last
					    (the-split-path))))
	      (last:key-typed! key-code
			       (recons SplitFocus:Last
				       context)))))))))
  
  ((pane-under x::real y::real)::Embeddable
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:pane-under x y))
	      x y
	      (lambda _ (this))))

  ((outside-in x::real y::real)::(Values real real)
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:outside-in x y))
	      x y
	      values))

  ((inside-out x::real y::real)::(Values real real)
   (error "inside-out not implemented for Split"))
  
  ((tap! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:tap! finger x y))
	      x y
	      values))

  ((press! finger::byte #;at x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:press! finger x y))
	      x y
	      (lambda (self::Embeddable x::real y::real)
		(screen:drag! finger (ResizeSplitAt
				      (the-split-path)))
		#t)))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:second-press! finger x y))
	      x y
	      never))

  ((double-tap! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:double-tap! finger x y))
	      x y
	      never))

  ((long-press! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:long-press! finger x y))
	      x y
	      never))

  ((scroll-up! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-up! x y))
	      left top
	      never))
  
  ((scroll-down! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-down! x y))
	      left top
	      never))
  
  ((scroll-left! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-left! x y))
	      left top
	      never))
  
  ((scroll-right! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-right! x y))
	      left top
	      never))

  ((zoom-in! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:zoom-in! x y))
	      left top
	      never))
  
  ((zoom-out! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:zoom-out! x y))
	      left top
	      never))

  ((rotate-left! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:rotate-left! x y))
	      left top
	      never))
  
  ((rotate-right! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:rotate-right! x y))
	      left top
	      never))
  
  ((can-split-beside? line::Area)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (or (with-pane-size first-size
	   (lambda ()
	     (first:can-split-beside? line)))
	 (with-pane-size last-size
	   (lambda ()
	     (last:can-split-beside?
	      (area/last line (+ first-size line-size))))))))
  
  ((split-beside! line::Area)::Embeddable
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-size first-size
       (lambda ()
	 (when (first:can-split-beside? line)
	   (set! first (first:split-beside! line)))))
     (let ((line* (area/last line (+ first-size line-size))))
       (with-pane-size last-size
	 (lambda ()
	   (when (last:can-split-beside? line*)
	     (set! last (last:split-beside! line*))))))
     (this)))
  
  ((can-split-below? line::Area)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (or (with-pane-size first-size
	   (lambda ()
	     (first:can-split-below? line)))
	 (with-pane-size last-size
	   (lambda ()
	     (last:can-split-below?
	      (area/last line (+ first-size line-size))))))))

  ((split-below! line::Area)::Embeddable
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-size first-size
       (lambda ()
	 (when (first:can-split-below? line)
	   (set! first (first:split-below! line)))))
     (let ((line* (area/last line (+ first-size line-size))))
       (with-pane-size last-size
	 (lambda ()
	   (when (last:can-split-below? line*)
	     (set! last (last:split-below! line*))))))
     (this)))

  )

(define (split-ref split-path)::Embeddable
  (match split-path
    ('() screen:top)
    (`(,head . ,tail)
     (let ((parent ::Split (split-ref tail)))
       (parent:part head)))))

(define/kw (screen-area split-path::(list-of SplitFocus)
			pane::Embeddable := screen:top)
  ::(Values Embeddable real real real real)
  (match split-path
    ('()
     (values pane
             0 0 screen:extent:width screen:extent:height))
    (`(,head . ,tail)
     (let-values (((parent::Embeddable
		    x::real y::real
		    w::real h::real) (screen-area tail pane)))
       (parameterize ((the-pane-width w)
		      (the-pane-height h))
	 (match parent
	   ((Split first: left last: right)
	    (let*-values (((split::Split) parent)
			  ((first-size line-size last-size)
			   (split:part-sizes)))
	      (match head
		(,SplitFocus:First
		 (let-values (((w* h*) (split:varying-size
					w h first-size)))
		   (values left x y w* h*)))
		(,SplitFocus:Last
		 (let-values (((x* y*) (split:transformed-dimension
					x y (- 0 first-size line-size)))
			      ((w* h*) (split:varying-size
					w h last-size)))
		   (values right x* y* w* h*))))))))))))

(define/kw (merge-split! at: split-path with: focus::SplitFocus)
  ::boolean
  (if (null? split-path)
    (and-let* ((split ::Split screen:top))
      (set! screen:top (split:part focus)))
    (and-let* ((`(,tip . ,root) split-path)
               (parent ::Split (split-ref root))
	       (split ::Split (parent:part tip)))
      (match tip
        (,SplitFocus:First (set! parent:first (split:part focus)))
	(,SplitFocus:Last (set! parent:last (split:part focus)))))))

(define-object (ResizeSplitAt split-path::list)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (let-values (((split::Split
		   left::real top::real
		   width::real height::real) (screen-area
					      split-path
					      screen:top)))
      (split:resize! x y left top width height)))
  
  (define (drop! x::real y::real vx::real vy::real)::void
    (and-let* ((target ::Split (split-ref split-path))
               (first-size line-size last-size
	                   (target:part-sizes))
	       (velocity (target:varying-dimension vx vy)))
      (cond
       ((or (is first-size <= (* 3 line-size)) ;>
            (is velocity < -1.5)) ;>
	(merge-split! at: split-path with: SplitFocus:Last))
       ((or (is last-size <= (* 3 line-size))
            (is velocity > 1.5))
	(merge-split! at: split-path with: SplitFocus:First)))))
  )

(define-type (SplitBeside)
  extending Split
  with
  ((part-sizes)::(Values real real real)
   (let* ((pane-width ::real (the-pane-width))
	  (line-width ::real (painter:vertical-split-width))
          (left-width ::real (as int (round (* at pane-width))))
          (right-width ::real (- pane-width left-width line-width)))
     (values left-width line-width right-width)))

  ((draw-line!)::void
   (painter:draw-vertical-split! 0))
  
  ((with-pane-size size::real action::procedure)::Object
   (parameterize ((the-pane-width size))
     (action)))

  ((with-pane-translation shift::real action::procedure)::Object
   (with-translation (shift 0)
     (action)))
  
  ((area/last line::Area earlier-size::real)::Area
   (Area left: (- line:left earlier-size)
	 top: line:top
	 right: (- line:right earlier-size)
	 bottom: line:bottom))

  ((varying-dimension x::real y::real)::real
   x)

  ((varying-size w::real h::real size::real)::(Values real real)
   (values size h))
  
  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   (values (- x shift) y))
  
  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   (set! at (/ (max 0.0 (* 1.0 (- pointer-left pane-left)))
	       pane-width)))
  )


(define-type (SplitBelow)
  extending Split
  with
  ((part-sizes)::(Values real real real)
   (let* ((pane-height ::real (the-pane-height))
	  (line-height ::real (painter:horizontal-split-height))
          (inner-height ::real (- pane-height line-height))
          (left-height ::real (as int (round (* at inner-height))))
          (right-height ::real (- inner-height left-height)))
     (values left-height line-height right-height)))

  ((draw-line!)::void
   (painter:draw-horizontal-split! 0))
  
  ((with-pane-size size::real action::procedure)::Object
   (parameterize ((the-pane-height size))
     (action)))

  ((with-pane-translation shift::real action::procedure)::Object
   (with-translation (0 shift)
     (action)))
  
  ((area/last line::Area earlier-size::real)::Area
   (Area left: line:left
	 top: (- line:top earlier-size)
	 right: line:right
	 bottom: (- line:bottom earlier-size)))

  ((varying-dimension x::real y::real)::real y)

  ((varying-size w::real h::real size::real)::(Values real real)
   (values w size))
  
  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   (values x (- y shift)))

   
  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   (set! at (/ (max 0.0 (* 1.0 (- pointer-top pane-top)))
	       pane-height)))
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
  (let* ((content ::Enchanted pop-up:content)
	 (left ::real pop-up:left)
	 (top ::real pop-up:top)
         (inner ::Extent (extent+ content))
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
   (let* ((inner ::Extent (extent+ content))
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
   (escape-with return
     (pop-up-action
      (this) 0 x y
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
		 (and path (recons 'edge path))))))))

  ((extent)::Extent
   (let ((inner ::Extent (extent+ content)))
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
  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))

  ((center-around! x::real y::real)::void
   (let ((inner ::Extent (extent))
	 (outer ::Extent (screen:size)))
     (set! left (max 0 (min (- outer:width inner:width)
			    (- x (quotient inner:width 2)))))
     (set! top
	   (max 0 (min (- outer:height inner:height)
		       (- y (quotient inner:height 2)))))))
  
  ((scroll-up! left::real top::real)::boolean
   (content:scroll-up! left top))
  
  ((scroll-down! left::real top::real)::boolean
   (content:scroll-down! left top))
  
  ((scroll-left! left::real top::real)::boolean
   (content:scroll-left! left top))
  
  ((scroll-right! left::real top::real)::boolean
   (content:scroll-right! left top))

  ((rotate-left! left::real top::real)::boolean
   (content:rotate-left! left top))
  
  ((rotate-right! left::real top::real)::boolean
   (content:rotate-right! left top))

  ((zoom-in! left::real top::real)::boolean
    (content:zoom-in! left top))
  
  ((zoom-out! left::real top::real)::boolean
   (content:zoom-out! left top))
  )

(define-type (Scroll width: real
                     height: real
		     left: real := 0
		     top: real := 0
		     content: Enchanted)
  implementing Drag
  with
  ((move! x::real y::real dx::real dy::real)::void
   (let ((inner ::Extent (extent+ content)))
     (set! left (max 0 (min (- inner:width width) (- left dx))))
     (set! top (max 0 (min (- inner:height height) (- top dy))))))

  ((drop! x::real y::real vx::real vy::real)::void
   (values))

  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (painter:fill-background! width height)
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

  ((scroll-up! x::real y::real)::boolean
   (and (is top > 0)
	(let* ((h ::real (painter:min-line-height)))
	  (set! top (max 0 (- top h)))
	  #t)))	     
  
  ((scroll-down! x::real y::real)::boolean
   (let ((inner ::Extent (extent+ content)))
     (and (is top < (- inner:height height))
	(let* ((h ::real (painter:min-line-height)))
	  (set! top (min (- inner:height height) (+ top h)))
	  #t))))
  
  ((scroll-left! x::real y::real)::boolean
   (and (is left > 0)
	(let* ((w ::real (painter:space-width)))
	  (set! left (max 0 (- left w)))
	  #t)))
  
  ((scroll-right! x::real y::real)::boolean
   (let ((inner ::Extent (extent+ content)))
     (and (is left < (- inner:width width))
	(let* ((w ::real (painter:space-width)))
	  (set! left (min (- inner:width width) (+ left w)))
	  #t))))

  ((zoom-in! x::real y::real)::boolean
   #f)
  
  ((zoom-out! x::real y::real)::boolean
   #f)

  ((rotate-left! x::real y::real)::boolean
   #f)
  
  ((rotate-right! x::real y::real)::boolean
   #f)
  
  ((value)::Object
   (invoke-special Base 'to-list cons to-expression)))

(define (text-field width::real content::CharSequence)::Scroll
  (let* ((input ::TextInput (text-input content))
	 (inner ::Extent (extent+ input))
	 (scroll ::Scroll (Scroll width: width
				  height: inner:height
				  content: input)))
    scroll))

(define (popup-scroll content::Enchanted)::PopUp
  (let* ((content ::Enchanted content)
	 (inner ::Extent (extent+ content))
	 (scroll ::Scroll (Scroll width: inner:width
				  height: inner:height
				  content: content))
         (popup (PopUp content: scroll))
	 (outer ::Extent (extent+ popup))
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
			(let ((new-window (open-file-browser directory
							     editor))
			      (position ::Position
					(last-known-pointer-position 0)))
			  (new-window:center-around! position:left position:top)
			  (screen:overlay:add! new-window))))))
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
				   (let ((new-window (save-file-browser
						      dir
						      text-field:content
						      editor))
					 (position ::Position
						   (last-known-pointer-position
						    0)))
				     (new-window:center-around! position:left
								position:top)
				     (screen:overlay:add! new-window)))))
	 (inner ::Extent (extent+ files))
	 (browser ::Scroll (Scroll content: files
				   width: inner:width
				   height: inner:height))
	 (top (Beside left: text-field right: button))
	 (upper ::Extent (extent+ top))
	 (content (Below top: top
                         bottom: browser))
         (popup (PopUp content: content))
	 (outer ::Extent (extent+ popup))
	 (available ::Extent (screen:size))
	 (button-size ::Extent (extent+ button)))
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

(define (builtin-open-file finger::byte editor::Editor)
  (lambda _
    (let ((keeper ::Keeper (the-keeper)))
      (keeper:with-read-permission
       (lambda _
	 (let ((window ::PopUp (open-file-browser
				(keeper:initial-directory)
				editor))
	       (position ::Position
			 (last-known-pointer-position
			  finger)))
	   (window:center-around! position:left position:top)
	   (screen:overlay:add! window)))))))
  
(define-parameter (open-file)::(maps (byte java.io.File Editor)
				     to: (maps _ to: void))
  builtin-open-file)

(define (builtin-save-file finger::byte editor::Editor)
  (lambda _
      (let ((keeper ::Keeper
		    (the-keeper)))
	(keeper:with-write-permission
	 (lambda _
	   (safely
	    (let ((window ::PopUp (save-file-browser
				   (keeper:initial-directory)
				   "filename.scm"
				   editor))
		  (position ::Position
			    (last-known-pointer-position
			     finger)))
	      (window:center-around! position:left position:top)
	      (screen:overlay:add! window))))))
      #t))
  
(define-parameter (save-file)::(maps (byte java.io.File Editor)
				     to: (maps _ to: void))
  builtin-save-file)

(define (document-switcher editor::Editor)::PopUp
  (let* ((choices (map (lambda (document::Document)
			 (Link content:
			       (Caption
				(if document:source
				    (document:source:getName)
				    "(unnamed)"))
			       on-tap:
			       (lambda _
				 (screen:overlay:clear!)
				 (editor:switch-to!
				  document)
				 #t)))
		       (open-documents))))
    (popup-scroll (ColumnGrid choices))))


(define-object (Translate target::Transform)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (target:translate! dx dy))

  (define (drop! x::real y::real vx::real vy::real)::void
    (values)))

(define-interface Embeddable+ (Embeddable java.lang.Cloneable))

(define-object (Editor)::Embeddable+

  (define (pane-under x::real y::real)::Embeddable
    (this))
  
  (define document ::Document (Document (empty) #!null))
  (define cursor ::Cursor '())
  
  (define transform ::Transform ((default-transform)))

  (define (outside-in x::real y::real)::(Values real real)
    (transform:outside-in x y))
  
  (define (inside-out x::real y::real)::(Values real real)
    (transform:inside-out x y))
  
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

  (define document-transform
    (property+ (document::Document)
	       ::Transform
	       ((default-transform))))
  
  (define (clone)
    (let* ((new-document-transform (copy document-transform))
	   (new-transform ::Transform (new-document-transform
				       document)))
      (Editor document: document
	      cursor: cursor
	      selection-anchor: selection-anchor
	      previously-edited: (copy previously-edited)
	      transform: new-transform
	      document-transform: new-document-transform)))
    
  (define (switch-to! target::Document)::void
    (unless (eq? target document)
      (set! (previously-edited target) document)
      (set! (document-transform document) transform)
      (set! document target)
      (set! transform (document-transform target))))
  
  (define (load-file file::java.io.File)::void
    (safely
     (select-document! (open-document-file file))))

  (define (load-from-port port::gnu.kawa.io.InPort source)::void
    (safely
     (select-document! (load-document-from-port port source))))
    
  (define (select-document! doc::Document)::void
    (set! (document-transform document) transform)
    (set! (previously-edited doc) document)
    (set! transform (document-transform doc))
    (set! document doc))
    
  (define (draw!)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-editor (this))
		   (the-selection-anchor selection-anchor))
      (transform:within painter
			  (lambda ()
			    (document:draw! '())))))

  (define (tap! finger::byte #;at xe::real ye::real)::boolean
    (parameterize/update-sources ((the-document document))
      (let-values (((x y) (transform:outside-in xe ye)))
	(let* ((target-cursor (cursor-under x y))
	       (target (the-expression at: target-cursor)))
	  (match target
	    (enchanted::Interactive
	     (enchanted:tap! finger x y))
	    (else
	     (set! cursor target-cursor)
	     (set! selection-anchor cursor)

	     #t))))))

  (define (press! finger::byte #;at xe::real ye::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let-values (((selection-start selection-end)
		    (the-selection))
		   ((x y) (transform:outside-in xe ye)))
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

	   ((any (lambda (layer::Layer)
		   (and-let* (((Stroke source-pane: ,(this))
			       layer))
		     layer))
		 screen:overlay:layers)
	    => (lambda (stroke::Stroke)
		 (screen:overlay:remove! stroke)
		 (unset! (screen:dragging stroke:finger))
		 (let ((p0 ::Position (Position left: xe top: ye))
		       (p1 ::Position (stroke:points
				       (- (length stroke:points)
					  1)))
		       (editor ::Editor (this)))
		   (set! (screen:dragging stroke:finger)
			 (object (Drag)
			   ((move! x::real y::real
				   dx::real dy::real)
			    ::void
			    (let ((p1x ::real (+ p1:left dx))
				  (p1y ::real (+ p1:top dy)))
			      (editor:transform:stretch!
			       p0:left p0:top p1:left p1:top
			       p0:left p0:top p1x  p1y)
			      (set! p1:left p1x)
			      (set! p1:top p1y)))
			   ((drop! x::real y::real
				   dx::real dy::real)
			    ::void
			    (values))))

		   (set! (screen:dragging finger)
			 (object (Drag)
			   ((move! x::real y::real
				   dx::real dy::real)
			    ::void
			    (let ((p0x ::real (+ p0:left dx))
				  (p0y ::real (+ p0:top dy)))
			      (editor:transform:stretch!
			       p0:left p0:top p1:left p1:top
			       p0x  p0y  p1:left p1:top)
			      (set! p0:left p0x)
			      (set! p0:top p0y)))
			   ((drop! x::real y::real
				   dx::real dy::real)
			    ::void
			    (values)))))))

	   
	   ((is target Space?)
	    (screen:drag! finger
			  (Drawing (Stroke finger (this)))))

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
	    (let* ((removed ::Remove (remove-element!
				      at: subpath))
		   (position (screen-position
			      (head removed:element)))
		  
		   (selection (Selected removed:element
					(copy position))))
	      (unset! (screen-position removed:element))
	      (unset! (screen-position (head removed:element)))
	      (screen:drag! finger (DragAround selection))))

	   ((and (is target cons?)
		 (eqv? tip (target:last-index)))
	    (let-values (;((x y) (transform:outside-in xe ye))
			 ((left top) (transform:inside-out position:left
							   position:top)))
	      (let ((extent ::Extent (extent+ target)))
		(screen:drag! finger
			      (Resize target subpath
				      (- ye top)
				      (this))))))
	   (else
	    (screen:drag! finger
			  (Drawing (Stroke finger (this))))
	    ;;(set! (the-cursor) path)
	    ;;(set! (the-selection-anchor) path)
	    )))
	#t)))

  (define (second-press! finger::byte #;at xe::real ye::real)
    ::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let-values (((selection-start selection-end)
		    (the-selection))
		   ((x y) (transform:outside-in xe ye)))
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
	    (screen:drag! finger
			  (Translate transform)))))
	#t)))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (cond
       ((isnt (transform:get-angle) = 0.0)
	(painter:play!
	 (Transition of: transform
		     from: (copy transform)
		     to: (let ((target ::Transform (copy transform)))
			   (target:set-angle! 0.0)
			   target)
		     around: (Position left: x top: y)
		     duration/ms: 500))
	#t)
       ((or (isnt (transform:get-left) = 0)
	    (is (transform:get-top) > 0))
	(painter:play!
	 (Transition of: transform
		     from: (copy transform)
		     to: (let ((target ::Transform (copy transform))
			       (document ::Extent (extent+ document))
			       (screen ::Extent (screen:size)))
			   (target:set-left! 0.0)
			   (target:set-top! 0.0)
			   (target:set-scale!
			    (min (/ screen:width document:width)
				 (/ screen:height document:height)))
			   target)
		     duration/ms: 500))
	#t)
       (else
	#f))))

  (define (long-press! finger::byte x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (safely
       (invoke (current-message-handler) 'clear-messages!)
       (let* ((content
	       ::Enchanted
	       (ColumnGrid
		`(,(Link content: (Caption "New")
			 on-tap: (lambda _ (WARN "New") #t))
		  ,(Link content: (Caption "Open...")
			 on-tap: ((open-file) finger (this)))
		  ,@(if (is (length (open-documents)) < 1)
			'()
			`(,(Link
			    content:
			    (Caption "Switch to...")
			    on-tap:
			    (lambda _
			      (safely
			       (screen:overlay:add!
				(document-switcher (this))))
			      #t))))
		  ,(Link content: (Caption "Save as...")
			 on-tap: ((save-file) finger (this)))
		  ,(Link content: (Caption "Close")
			 on-tap: (lambda _ (WARN "Close") #t))
		  )))
	      (window ::PopUp (PopUp content: content)))
	 (window:center-around! x y)
	 (screen:overlay:add! window))))
    ;; dodanie menu kontekstowego
    #t)

  (define (scroll-up! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let* ((h ::real (painter:min-line-height)))
	(transform:translate! 0 h)
	#t)))
  
  (define (scroll-down! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let* ((h ::real (painter:min-line-height)))
	(transform:translate! 0 (- h))
	#t)))
  
  (define (scroll-left! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let* ((w ::real (painter:space-width)))
	(transform:translate! w 0)
	#t)))
  
  (define (scroll-right! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (let* ((w ::real (painter:space-width)))
	(transform:translate! (- w) 0))
      #t))

  (define (zoom-in! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (transform:scale! 1.25 x y)
      #t))
  
  (define (zoom-out! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (transform:scale! 0.8 x y)
      #t))

  (define (rotate-left! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (transform:rotate! -5.0 x y)
      #t))
  
  (define (rotate-right! x::real y::real)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      (transform:rotate! 5.0 x y)
      #t))
  
  (define (key-typed! key-code::long context::Cursor)::boolean
    (parameterize/update-sources ((the-document document)
				  (the-cursor cursor)
				  (the-editor (this))
				  (the-selection-anchor
				   selection-anchor))
      ((keymap key-code))
      #t))

  (define (can-split-beside? line::Area)::boolean
    (let* ((vicinity ::real
		     (painter:line-simplification-resolution))
	   (3vicinity (* vicinity 3)))
      (and (is 0 < line:left <= line:right < (the-pane-width))
	   (is line:top <= 3vicinity)
	   (is (- (the-pane-height) line:bottom) <= 3vicinity))))
  
  (define (split-beside! line::Area)::Embeddable
    (if (can-split-beside? line)
	(let*-values (((ratio::real) (/ (/ (+ line:left line:right)
					   2)
					(the-pane-width)))
		      ((new::Editor) (copy (this)))
		      ((split::Split) (SplitBeside first: (this)
						   last: new
						   at: ratio))
		      ((left line right) (split:part-sizes))
		      ((shift) (+ left line)))
	  (new:transform:translate! (- shift) 0)
	  split)
	(this)))
  
  (define (can-split-below? line::Area)::boolean
    (let* ((vicinity ::real
		     (painter:line-simplification-resolution))
	   (3vicinity (* vicinity 3)))
      (and (is 0 < line:top <= line:bottom < (the-pane-height))
	   (is line:left <= 3vicinity)
	   (is (- (the-pane-width) line:right) <= 3vicinity))))

  (define (split-below! line::Area)::Embeddable
    (if (can-split-below? line)
	(let*-values (((ratio::real) (/ (/ (+ line:top line:bottom)
					  2)
				       (the-pane-height)))
		      ((new::Editor) (copy (this)))
		      ((split::Split) (SplitBelow first: (this)
						  last: new
						  at: ratio))
		      ((top line bottom) (split:part-sizes))
		      ((shift) (+ top line)))
	  (new:transform:translate! 0 (- shift))
	  split)
	(this)))
  )

(define-early-constant screen ::Screen
  (Screen))

(define-parameter (the-editor)::Editor
  #!null)

(define (adjust-view!)
  (let* ((editor ::Editor (the-editor))
	 (cursor ::Position (painter:cursor-position))
	 (cursor-height ::real (painter:cursor-height))
	 (editor-left ::real (the-pane-left))
	 (editor-top ::real (the-pane-top))
	 (editor-width ::real (the-pane-width))
	 (editor-height ::real (the-pane-height))
	 (xe ::real (- cursor:left editor-left))
	 (ye ::real (- cursor:top editor-top)))
    (unless (is 0 <= ye < (- editor-height cursor-height))
      (painter:play!
       (Transition of: editor:transform
		   from: (copy editor:transform)
		   to: (let ((target ::Transform
				     (copy editor:transform)))
			 (target:translate! 0 (- (/ (- editor-height
						       cursor-height)
						    2) ye))
			 target)
		   duration/ms: 500)))))
