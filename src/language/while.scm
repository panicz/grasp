(module-name (language while))
(import (language define-syntax-rule))

(define-syntax-rule (escape-with label . commands)
  (call/cc (lambda (label) . commands)))

(define-syntax-rule (while condition actions ...)
  (let ()
    (define (loop)
      (when condition
	actions ... (loop)))
    (loop)))
