(module-name (language mapping))

(import (srfi :17))
(import (language define-syntax-rule))
(import (utils hash-table))
(import (utils conversions))

(define-syntax mapping
  (syntax-rules (::)
    ((mapping (object::key-type)::value-type default)
     (let* ((entries (make-hash-table[key-type value-type]))
            (getter (lambda (object::key-type)
                      (hash-ref entries object
				(lambda () default)))))
       (set! (setter getter) (lambda (arg value)
                               (hash-set! entries arg value)))
       (set-procedure-property! getter 'table entries)
       getter))
    ((mapping (object::key-type) default)
     (mapping (object::key-type)::java.lang.Object
	       default))

    ((mapping (object)::value-type default)
     (mapping (object::java.lang.Object)::value-type
	       default))

    ((mapping (object) default)
     (mapping (object::java.lang.Object)::java.lang.Object
	      default))
    ))

(define-syntax bimapping
  (syntax-rules (::)
    ((bimapping (object::key-type)::value-type default)
     (let* ((entries (make-hash-table[key-type value-type]))
	    (inverse-entries (make-hash-table[value-type key-type]))
            (getter (lambda (object)
                      (hash-ref entries object
				(lambda () default))))
            (inverse-getter (lambda (object)
			      (hash-ref inverse-entries object
					(lambda ()
					  (hash-ref entries object
						    (lambda () default)))))))
       (set! (setter getter) (lambda (arg value)
                               (hash-set! entries arg value)
			       (hash-set! inverse-entries value arg)))
       (set! (setter inverse-getter) (lambda (arg value)
				       (hash-set! entries arg value)
				       (hash-set! inverse-entries value arg)))
       (set-procedure-property! getter 'table entries)
       (set-procedure-property! inverse-getter 'table inverse-entries)
       (set-procedure-property! getter 'inverse inverse-getter)
       (set-procedure-property! inverse-getter 'inverse getter)
       getter))
    ((bimapping (object::key-type) default)
     (bimapping (object::key-type)::java.lang.Object
	       default))

    ((bimapping (object)::value-type default)
     (bimapping (object::java.lang.Object)::value-type
	       default))

    ((bimapping (object) default)
     (bimapping (object::java.lang.Object)::java.lang.Object
	      default))
    ))

(define (keys dict)
  (let ((table (procedure-property dict 'table)))
    (invoke (as java.util.Map table) 'keySet)))

(define-syntax define-mapping
  (syntax-rules (::)
    ((define-mapping (mapping-name object::key-type)::value-type
       default)
     (define-early-constant mapping-name
       (with-procedure-properties ((name 'mapping-name))
	 (mapping (object::key-type)::value-type default))))

    ((define-mapping (mapping-name object::key-type) default)
     (define-mapping (mapping-name object::key-type)
       ::java.lang.Object
       default))

    ((define-mapping (mapping-name object)::value-type default)
     (define-mapping (mapping-name object::java.lang.Object)
       ::value-type
       default))

    ((define-mapping (mapping-name object) default)
     (define-mapping (mapping-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

(define-syntax define-bimapping
  (syntax-rules (::)
    ((define-bimapping (bimapping-name object::key-type)::value-type
       default)
     (define-early-constant bimapping-name
       (with-procedure-properties ((name 'bimapping-name))
	 (bimapping (object::key-type)::value-type default))))

    ((define-bimapping (bimapping-name object::key-type) default)
     (define-bimapping (bimapping-name object::key-type)
       ::java.lang.Object
       default))

    ((define-bimapping (bimapping-name object)::value-type default)
     (define-bimapping (bimapping-name object::java.lang.Object)
       ::value-type
       default))

    ((define-bimapping (bimapping-name object) default)
     (define-bimapping (bimapping-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

;; note that there's some sort of trickery going on here:
;; we rely on Kawa's native equal?-like cons cells equality
;; for handling the parameter list

(define (memoize proc)
  (let ((table ::Map (make-hash-table)))
    (with-procedure-properties ((table table))
      (lambda args
	(if (table:contains-key args)
	    (apply values (table:get args))
	    (call-with-values (lambda () (apply proc args))
	      (lambda result
		(table:put args result)
		(apply values result))))))))

(define-syntax-rule (define/memoized (name . args) . body)
  (define name (memoize (lambda args . body))))
