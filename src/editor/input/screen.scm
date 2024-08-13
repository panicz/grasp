(module-name (editor input screen))

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
(import (language for))
(import (language while))
(import (language fundamental))
(import (language parameterize-up))
(import (utils functions))
(import (utils conversions))
(import (utils print))
(import (utils hash-table))

(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor document cursor))
(import (editor input input))
(import (editor types extensions extensions))

(import (editor input overlays))

(define-alias Array java.util.Arrays)

(define-type (Recognizer name: string
			 recognizes: (maps ((sequence-of Position)
					    Screen)
					   to: list)
			 action: (maps (Recognizer
					(sequence-of Position)
					Screen
					. list)
				       to: void)))

(define-early-constant the-recognizers
  ::java.util.List
  (java.util.ArrayList))

(define-object (Point x::real y::real color::long)::Layer
  (define (render!)
    (painter:draw-point! x y color))
  (IgnoreInput))

(define-object (ActualScreen)::Screen
  (define overlay ::Overlay (Overlay))

  (define (overlay-cursor layer::Layer)::Cursor
    (overlay:cursor layer))

  (define (set-overlay-cursor! layer::Layer cursor::Cursor)::void
    (set! (overlay:cursor layer) cursor))

  (define (has-layer satisfying::predicate)::Layer
    (let ((result (any satisfying overlay:layers)))
      (if result
	  (as Layer result)
	  #!null)))
  
  (define dragging ::(maps byte to: Drag)
    (mapping (finger::byte)::Drag #!null))

  (define top ::Embeddable (NullPane))

  (define content-stack ::java.util.Stack
    (java.util.Stack))

  (define (maximize! tile ::Maximizable)::void
    (content-stack:push `(,(copy (tile:extent)) . ,top))
    (tile:set-size! size:width size:height)
    (set! top tile))

  (define (unmaximize!)::void
    (unless (content-stack:empty)
      (and-let* ((`(,previous::Extent . ,origin)
		  (content-stack:pop))
		 (widget ::Maximizable top))
	(widget:set-size! previous:width previous:height)
	(set! top origin))))
  
  ;; this parameter must be set by the
  ;; graphical framework (Lanterna, AWT, ...)
  ;; and changed every time the hosting
  ;; window is resized
  (define size ::Extent (Extent width: 0 height: 0))

  (define (drag! finger::byte action::Drag)::void
    (set! (dragging finger) action))

  (define (undrag! finger::byte)::void
    (unset! (dragging finger)))

  (define (set-size! width::real height::real)::void
    (set! size:width width)
    (set! size:height height)
    (set! (the-pane-width) width)
    (set! (the-pane-height) height)
    (and-let* ((widget ::Maximizable top))
      (widget:set-size! width height))
    )

  (define (width)::real
    size:width)
    
  (define (height)::real
    size:height)

  (define (extent)::Extent size)

  (define (set-content! content::Embeddable)::void
    (set! top content))

  (define (content)::Embeddable top)

  (define (render!)::void
    (reset! extent-cached?)
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (top:render!)
      (overlay:render!)))

  (define after-tap-hooks ::(list-of (maps (byte real real) to: void))
    '())
  
  (define (after-tap action::(maps (byte real real) to: void))::void
    (set! after-tap-hooks (cons action after-tap-hooks)))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
     (let ((result (or (overlay:tap! finger x y)
			(top:tap! finger x y))))
	(for hook::(maps (byte real real) to: void) in after-tap-hooks
	     (hook finger x y))
	result)))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:press! finger x y)
	  (top:press! finger x y))))

  (define (release! finger::byte x::real y::real
		    vx::real vy::real)
    ::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (and-let* ((drag ::Drag (dragging finger)))
	(drag:drop! x y vx vy)
	(unset! (dragging finger))
	#t)))

  (define (move! finger::byte x::real y::real
		 dx::real dy::real)
    ::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (and-let* ((drag ::Drag (dragging finger)))
	(drag:move! x y dx dy)
	#t)))

  (define (second-press! finger::byte #;at x::real y::real)
    ::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:second-press! finger x y)
	  (top:second-press! finger x y))))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:double-tap! finger x y)
	  (top:double-tap! finger x y))))

  (define (long-press! finger::byte x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:long-press! finger x y)
	  (top:long-press! finger x y))))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (assert (empty? context))
    (if (and (is top Maximizable?)
	     (is (key-code-name key-code) in '(escape back)))
	(screen:unmaximize!)
	(parameterize ((the-pane-width size:width)
		       (the-pane-height size:height))
	  (or (overlay:key-typed! key-code context)
	      (top:key-typed! key-code context)))))

  (define (can-split-beside? line::Area)::boolean
    (top:can-split-beside? line))

  (define (split-beside! line::Area)::Embeddable
    (set! top (top:split-beside! line))
    (this))

  (define (can-split-below? line::Area)::boolean
    (top:can-split-below? line))

  (define (split-below! line::Area)::Embeddable
    (set! top (top:split-below! line))
    (this))

  (define (scroll-up! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:scroll-up! x y)
	  (top:scroll-up! x y))))

  (define (scroll-down! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:scroll-down! x y)
	  (top:scroll-down! x y))))

  (define (scroll-left! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:scroll-left! x y)
	  (top:scroll-left! x y))))

  (define (scroll-right! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:scroll-right! x y)
	  (top:scroll-right! x y))))

  (define (zoom-in! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:zoom-in! x y)
	  (top:zoom-in! x y))))

  (define (zoom-out! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:zoom-out! x y)
	  (top:zoom-out! x y))))

  (define (rotate-left! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:rotate-left! x y)
	  (top:rotate-left! x y))))

  (define (rotate-right! x::real y::real)::boolean
    (parameterize ((the-pane-width size:width)
		   (the-pane-height size:height))
      (or (overlay:rotate-right! x y)
	  (top:rotate-right! x y))))

  (define (outside-in x::real y::real)::(Values real real)
    (values x y))

  (define (pane-under x::real y::real)::Embeddable
    (this))

  (define (drop-at! x::real y::real expression::pair)::boolean
    (top:drop-at! x y expression))

  (define (add-overlay! layer::Layer)::void
    (overlay:add! layer))
    
  (define (remove-overlay! layer::Layer)::void
    (overlay:remove! layer))

  (define (clear-overlay!)::void
    (overlay:clear!))
  
  )

(define-early-constant screen ::Screen (ActualScreen))
