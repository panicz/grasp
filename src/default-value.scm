(import (define-syntax-rule))
(import (define-property))

;; "default-value" is meant to produce a default value
;; for a given type

(define-property (default-value type) #!null)

(set! (default-value real) 0.0)
(set! (default-value int) 0)
(set! (default-value string) "")
(set! (default-value list) '())
(set! (default-value boolean) #f)
