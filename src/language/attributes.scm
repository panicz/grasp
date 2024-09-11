(module-name (language attributes))

(import (srfi :17))
(import (language define-syntax-rule))
(import (utils hash-table))
(import (language fundamental))

(define-syntax attribute
  (syntax-rules (::)
    ((attribute (object::key-type)::value-type default)
     (let ((table ::java.util.WeakHashMap
		  (($bracket-apply$ make-weak-key-hash-table
				    key-type value-type))))
       (define (create table::java.util.WeakHashMap)
         (let ((getter ::procedure
		       (lambda (object::key-type)
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

    ((attribute (object::key-type) default)
     (attribute (object::key-type)::java.lang.Object
	       default))

    ((attribute (object)::value-type default)
     (attribute (object::java.lang.Object)::value-type
	       default))

    ((attribute (object) default)
     (attribute (object::java.lang.Object)::java.lang.Object
	       default))
    ))

;; attribute+ is like attribute but it stores the default
;; value for every enquired object
(define-syntax attribute+
  (syntax-rules (::)
    ((attribute+ (object::key-type)::value-type default)
     (let ((table ::java.util.WeakHashMap
		  (($bracket-apply$ make-weak-key-hash-table
				    key-type value-type))))
       (define (create table::java.util.WeakHashMap)
         (let ((getter ::procedure
		       (lambda (object::key-type)
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

    ((attribute+ (object::key-type) default)
     (attribute+ (object::key-type)::java.lang.Object
		default))

    ((attribute+ (object)::value-type default)
     (attribute+ (object::java.lang.Object)::value-type
		default))

    ((attribute+ (object) default)
     (attribute+ (object::java.lang.Object)::java.lang.Object
		default))
    ))

(define-syntax define-attribute
  (syntax-rules (::)
    ((define-attribute (attribute-name object::key-type)
       ::value-type
       default)
     (define-early-constant attribute-name
       (with-procedure-properties
	((name 'attribute-name))
	(attribute (object::key-type)::value-type default))))

    ((define-attribute (attribute-name object::key-type) default)
     (define-attribute (attribute-name object::key-type)
       ::java.lang.Object
       default))

    ((define-attribute (attribute-name object)::value-type default)
     (define-attribute (attribute-name object::java.lang.Object)
       ::value-type
       default))

    ((define-attribute (attribute-name object) default)
     (define-attribute (attribute-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

(define-syntax define-attribute+
  (syntax-rules (::)
    ((define-attribute+ (attribute-name object::key-type)
       ::value-type
       default)
     (define-early-constant attribute-name
       (with-procedure-properties
	((name 'attribute-name))
	(attribute+ (object::key-type)::value-type default))))

    ((define-attribute+ (attribute-name object::key-type) default)
     (define-attribute+ (attribute-name object::key-type)
       ::java.lang.Object
       default))

    ((define-attribute+ (attribute-name object)::value-type
       default)
     (define-attribute+ (attribute-name object::java.lang.Object)
       ::value-type
       default))

    ((define-attribute+ (attribute-name object) default)
     (define-attribute+ (attribute-name object::java.lang.Object)
       ::java.lang.Object
       default))
    ))

