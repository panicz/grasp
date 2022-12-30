(import (define-syntax-rule))

(define-syntax-rule (assert proposition)
  (unless proposition (error "Assertion failed: " 'proposition)))

