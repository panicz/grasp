(import (define-syntax-rule))

(define-syntax-rule (while condition actions ...)
  (let ()
    (define (loop)
      (when condition
	actions ... (loop)))
    (loop)))
