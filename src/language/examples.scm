(module-name (language examples))

(import (language define-parameter))

;; the (in-example-context?) parameter can be used
;; by certain test facilities to  suppress
;; diagnostic messages in some derivative testing
;; facilities (see the definition of (snapshot)
;; in the (utils test) module and various test
;; suites that use it). It should NEVER be used
;; for making behavioral changes in production code
;; (where by "production code" I mean anything
;; that is not a test utility)
(define-parameter (in-example-context?) #f)

(define (bad-example form expected-value obtained-value)
  (call-with-output-string
   (lambda (port)
     (display "while evaluating\n" port)
     (display form port)
     (display "\n\nExpected:\n" port)
     (display expected-value port)
     (display "\nGot: \n" port)
     (display obtained-value port))))
  
(define-syntax e.g.
  (syntax-rules (===> $bracket-list$ $string$)
    ((_ example)
     (parameterize ((in-example-context? #t))
       (or example
	   (error 'example))))
    
    ((_ example ===> ($bracket-list$ . value))
     (parameterize ((in-example-context? #t))
       (let ((result example))
	 (if (equal? result value)
	     result
	     (error (bad-example 'example 'value result))
	     ))))

    ((_ example ===> ($string$ . value))
     (parameterize ((in-example-context? #t))
       (let ((result example))
	 (if (equal? result ($string$ . value))
	     result
	     (error(bad-example 'example ($string$ value) result))))))
    
    ((_ example ===> value)
     (parameterize ((in-example-context? #t))
       (let ((result example))
	 (if (equal? result 'value)
	     result
	     (error (bad-example 'example 'value result))))))

    ((_ example ===> value ...)
     (parameterize ((in-example-context? #t))
       (call-with-values (lambda () example)
	 (lambda results
	   (if (equal? results '(value ...))
	       (apply values results)
	       (error '(example ===> value ...) results))))))
    ))
