(module-name (editor input overlays))

(import (language define-object))
(import (language define-property))
(import (language define-type))
(import (language fundamental))
(import (language while))
(import (language for))
(import (language parameterize-up))
(import (utils functions))
(import (editor document cursor))
(import (editor interfaces elements))

(define-object (Overlay)::Pane
  (define layers ::($bracket-apply$ List Layer)
    (($bracket-apply$ ArrayList Layer)))

  (define cursor ::(maps (Layer) to: Cursor)
    (property+ (layer::Layer)::Cursor
	       (cursor-climb-front '() layer)))

  (define (render!)::void
    (for layer::Layer in-reverse layers
      (parameterize ((the-cursor (cursor layer)))
	(layer:render!))))

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
