(module-name (language parameterize-up))

(import (language define-syntax-rule))

;; like `parameterize`, but updates parameter sources
;; after the execution of the inner block

(define-syntax parameterize/update-sources
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((param source) ...) body + ...)
       (with-syntax (((previous-value ...)
		      (generate-temporaries
		       #'(source ...))))
	 #'(parameterize ((param source) ...)
	     (let ((previous-value source) ...)
	       (try-finally
		(begin body + ...)
		(begin
		  (when (eqv? previous-value source)
		    (set! source (param)))
		  ...)))))))))
