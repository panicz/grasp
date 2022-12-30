(import (define-interface))

(define-alias StringBuilder java.lang.StringBuilder)

(define-interface StringBuilding ()
  (buildString out::StringBuilder)::StringBuilder)
