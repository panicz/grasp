(import (define-interface))
(import (define-type))
(import (define-object))
(import (fundamental))
(import (indexable))

(define-interface Interactive ()
  ;; by convention, the return value of #true means
  ;; that the event was handled by the object,
  ;; and the value of #false means that it was ignored
  ;;
  ;; This is used by the event system to find out
  ;; whether the event has been consumed or not.
  (key-pressed key::int)::boolean
  (key-released key::int)::boolean

  (key-typed unicode::int)::boolean
  
  (tapped x::real y::real)::boolean
  (pressed x::real y::real)::boolean
  (released x::real y::real)::boolean
  
  (dragged-over x::real y::real item::Tile*)::boolean
  (dragged-out x::real y::real item::Tile*)::boolean
  (dropped x::real y::real item::Tile*)::boolean
  
  (held x::real y::real)::boolean
  (double-tapped x::real y::real)::boolean
  (second-pressed x::real y::real)::boolean

  )



(define-object (Passive)::Interactive
  (define (key-pressed key::int)::boolean #f)
  (define (key-released key::int)::boolean #f)
  (define (key-typed unicode::int)::boolean #f)

  
  (define (tapped x::real y::real)::boolean #f)
  (define (pressed x::real y::real)::boolean #f)
  (define (released x::real y::real)::boolean #f)
  
  (define (dragged-over x::real y::real item::Tile*)::boolean #f)
  (define (dragged-out x::real y::real item::Tile*)::boolean #f)
  (define (dropped x::real y::real item::Tile*)::boolean #f)
  
  (define (held x::real y::real)::boolean #f)
  (define (double-tapped x::real y::real)::boolean #f)
  (define (second-pressed x::real y::real)::boolean #f)

  )
