(import (define-interface))
(import (define-object))

(define-interface Cancellable ()
  (cancel)::Cancellable)

(define-object (CancellableNothing)::Cancellable
  (define (cancel)::Cancellable
    (this)))

(define-early-constant cancellable-nothing ::Cancellable
  (CancellableNothing))

(define-interface Postponed ()
  (after delay-ms::long action::procedure)::Cancellable)

(define (current-time-ms)::long
  (java.lang.System:currentTimeMillis))
