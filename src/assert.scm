(import (define-syntax-rule))

(define-syntax-rule (assert proposition)
  (or proposition (error "Assertion failed: " 'proposition)))

