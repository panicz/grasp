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

(define-syntax with
  (lambda (stx)
    (syntax-case stx ()
      ((with ((variable type* ... value) ...) . actions)
       (with-syntax (((previous-value ...)
		      (generate-temporaries #'(variable ...))))
		    #'(let ((previous-value variable) ...)
			(set! variable value)
			...
			(try-finally
			 (begin . actions)
			 (begin
			   (set! variable previous-value)
			   ...))))))))
