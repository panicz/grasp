(module-name (language assert))
(import (language define-syntax-rule))

(define-syntax-rule (assert proposition)
  (or proposition (error "Assertion failed: " 'proposition)))

