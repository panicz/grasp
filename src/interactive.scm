(import (define-interface))
(import (define-type))
(import (define-object))
(import (fundamental))
(import (indexable))

(define-interface Interactive ()
  (tap! finger::byte #;at x::real y::real)::boolean
  (press! finger::byte #;at x::real y::real)::boolean
  (second-press! finger::byte #;at x::real y::real)::boolean
  (double-tap! finger::byte x::real y::real)::boolean
  (long-press! finger::byte x::real y::real)::boolean
  (key-typed! key-code::long context::Cursor)::boolean
  )

(define-object (IgnoreInput)::Interactive
  (define (tap! finger::byte #;at x::real y::real)::boolean #f)
  (define (press! finger::byte #;at x::real y::real)::boolean #f)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #f)
  (define (double-tap! finger::byte x::real y::real)::boolean #f)
  (define (long-press! finger::byte x::real y::real)::boolean #f)
  
  (define (key-typed! key-code::long context::Cursor)::boolean #f)
  )

(define-object (ConsumeInput)::Interactive
  (define (tap! finger::byte #;at x::real y::real)::boolean #t)
  (define (press! finger::byte #;at x::real y::real)::boolean #t)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #t)
  (define (double-tap! finger::byte x::real y::real)::boolean #t)
  (define (long-press! finger::byte x::real y::real)::boolean #t)
  (define (key-typed! key-code::long context::Cursor)::boolean #t)
  )
