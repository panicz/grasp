(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))

(define-syntax property
  (syntax-rules (::)
    ((property (object::key-type)::value-type default)
     (let* ((table (($bracket-apply$ make-weak-key-hash-table
				     key-type value-type)))
            (getter (lambda (object::key-type)
                      (hash-ref table object
				(lambda () default)))))
       (set! (setter getter)
	 (lambda (arg::key-type value::value-type)
           (hash-set! table arg value)))
       (set-procedure-property! getter 'table table)
       getter))

    ((property (object::key-type) default)
     (property (object::key-type)::java.lang.Object
	       default))

    ((property (object)::value-type default)
     (property (object::java.lang.Object)::value-type
	       default))

    ((property (object) default)
     (property (object::java.lang.Object)::java.lang.Object
	       default))
    ))

;; property+ is like property but it stores the default
;; value for every enquired object
(define-syntax property+
  (syntax-rules (::)
    ((property+ (object::key-type)::value-type default)
     (let* ((table (($bracket-apply$ make-weak-key-hash-table
				     key-type value-type)))
            (getter (lambda (object::key-type)
                      (hash-ref+ table object
				 (lambda () default)))))
       (set! (setter getter)
	 (lambda (arg::key-type value::value-type)
           (hash-set! table arg value)))
       (set-procedure-property! getter 'table table)
       getter))

    ((property+ (object::key-type) default)
     (property+ (object::key-type)::java.lang.Object
		default))

    ((property+ (object)::value-type default)
     (property+ (object::java.lang.Object)::value-type
		default))

    ((property+ (object) default)
     (property+ (object::java.lang.Object)::java.lang.Object
		default))
    ))

(define-syntax define-property
  (syntax-rules (::)
    ((define-property (property-name object::key-type)
       ::value-type
       default)
     (define-early-constant property-name
       (with-procedure-properties
	((name 'property-name))
	(property (object::key-type)::value-type default))))

    ((define-property (property-name object::key-type) default)
     (define-property (property-name object::key-type)
       ::java.lang.Object
       default))

    ((define-property (property-name object)::value-type default)
     (define-property (property-name object::java.lang.Object)
       ::value-type
       default))

    ((define-property (property-name object) default)
     (define-property (property-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

(define-syntax define-property+
  (syntax-rules (::)
    ((define-property+ (property-name object::key-type)
       ::value-type
       default)
     (define-early-constant property-name
       (with-procedure-properties
	((name 'property-name))
	(property+ (object::key-type)::value-type default))))

    ((define-property+ (property-name object::key-type) default)
     (define-property+ (property-name object::key-type)
       ::java.lang.Object
       default))

    ((define-property+ (property-name object)::value-type
       default)
     (define-property+ (property-name object::java.lang.Object)
       ::value-type
       default))

    ((define-property+ (property-name object) default)
     (define-property+ (property-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

