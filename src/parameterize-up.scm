(import (define-syntax-rule))

(define-syntax update-parameter-value-sources!
  (lambda (stx)
    (syntax-case stx ($lookup$)
      ((update-parameter-value-sources!)
       #'(begin))
      ((update-parameter-value-sources! (parameter source) . rest)
       (identifier? #'source)
       #'(begin
	   (set! source (parameter))
	   (update-parameter-value-sources! . rest)))

      ((update-parameter-value-sources! (parameter ($lookup$ k v)) . rest)
       #'(begin
	   (set! ($lookup$ k v) (parameter))
	   (update-parameter-value-sources! . rest)))

      ((update-parameter-value-sources! (parameter source) . rest)
       #'(update-parameter-value-sources! . rest)))))

;; like `parameterize`, but updates parameter sources
;; after the execution of the inner block

(define-syntax-rule (parameterize/update-sources ((param source) ...)
		      body + ...)
  (parameterize ((param source) ...)
    (call-with-values (lambda () body + ...)
      (lambda result
	(update-parameter-value-sources! (param source) ...)
        (apply values result)))))

