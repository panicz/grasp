(module-name (language define-property))

(import (srfi :17))
(import (language define-syntax-rule))
(import (utils hash-table))
(import (language fundamental))

(define-syntax property
  (syntax-rules (::)
    ((property (object::key-type)::value-type default)
     (let ((table ::java.util.WeakHashMap
		  (($bracket-apply$ make-weak-key-hash-table
				    key-type value-type))))
       (define (create table::java.util.WeakHashMap)
         (let ((getter (lambda (object::key-type)
                         (hash-ref table object
				   (lambda () default)))))
           (set! (setter getter)
		 (lambda (arg::key-type value::value-type)
                   (hash-set! table arg value)))
	   (set-procedure-property! getter 'table table)
	   (set-procedure-property! getter 'clone
				    (lambda ()
				      (create (copy table))))
	   getter))

       (create table)))

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
     (let ((table ::java.util.WeakHashMap
		  (($bracket-apply$ make-weak-key-hash-table
				    key-type value-type))))
       (define (create table::java.util.WeakHashMap)
         (let ((getter (lambda (object::key-type)
                         (hash-ref+ table object
				    (lambda () default)))))
           (set! (setter getter)
		 (lambda (arg::key-type value::value-type)
                   (hash-set! table arg value)))
	   (set-procedure-property! getter 'table table)
	   (set-procedure-property! getter 'clone
				    (lambda ()
				      (create (copy table))))
	   getter))

       (create table)))

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

