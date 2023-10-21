(module-name (editor types extensions interactions))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language fundamental))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor input transforms))


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
  (pane-under x::real y::real)::Embeddable

  (can-split-beside? line::Area)::boolean
  (split-beside! line::Area)::Embeddable
  
  (can-split-below? line::Area)::boolean
  (split-below! line::Area)::Embeddable
  )

(define-object (NullPane)::Embeddable
  (define (draw!)::void (values))
  
  (define (pane-under x::real y::real)::Embeddable
    (this))
  
  (define (map x::real y::real)::(Values real real)
    (values x y))
  
  (define (unmap x::real y::real)::(Values real real)
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
