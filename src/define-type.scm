(import (conversions))
(import (define-syntax-rule))
(import (assert))
(import (define-interface))

;;(define-alias Cloneable java.lang.Cloneable)

(define-syntax maps
  (syntax-rules (to:)
    ((_ input-types to: output-type + ...)
     procedure)))

(define-syntax-rule (list-of type)
  list)

(define-syntax-rule (either type ...)
  java.lang.Object)

#|
(define-simple-class Struct ()
  interface: #t
  ((typename) :: String #!abstract)
  ((fields->string) :: String #!abstract)
  ((toString) :: String #!abstract)
  ((assign source :: Struct) :: Struct #!abstract)
  ((clone) :: Struct #!abstract))

|#


(define-interface Struct (java.lang.Cloneable)
  (typename) :: String
  (fields->string) :: String
  (toString) :: String
  ;;(embedded-in? object)::boolean
  (assign source :: Struct) :: Struct
  (clone) :: Struct
  ;;(deep-copy)::Struct
  )

(define (copy struct::Struct)::Struct
  (struct:clone))

(define-simple-class Base (Struct)
  ((typename)::String #!abstract)
  ((fields->string)::String "")
  ((toString)::String
   (string-append "["(typename) (fields->string)"]"))
  ((embedded-in? object)::boolean (instance? object Base))
  ((assign source::Struct)::Struct (this))
  ((clone)::Struct (Base)))

(define-syntax-rule (define-type (type-name . fields) . spec)
  (type-definition type-name Base () fields () spec ()))

(define-syntax type-definition
  (lambda (stx)
    (syntax-case stx (Base extending implementing with :=)

      ((_ type-name parent interfaces
	  () ((slot-symbol . slot-spec) ...) () (methods ...))
       #'(define-simple-class type-name (parent . interfaces)
	   ((typename):: String
	    (symbol->string 'type-name))
	   
	   ((fields->string):: String
	    (string-append
	     (invoke-special parent (this) 'fields->string)
	     (string-append " " (symbol->string 'slot-symbol)
			    ": "(java.lang.String:valueOf slot-symbol))
	     ...))
	   	   
	   ((embedded-in? object):: boolean
	    (and (instance? object type-name)
		 (invoke-special parent (this) 'embedded-in? object)
		 (equal? slot-symbol (field object 'slot-symbol))
		 ...))

	   ((equals object):: boolean
	    (and (instance? object type-name)
		 (invoke (as type-name object) 'embedded-in? (this))))
	   
	   ((clone):: type-name
	    (let ((copy (type-name)))
	      (invoke copy 'assign (as parent (this)))
	      ;;(invoke-special parent copy 'assign (this))
	      (slot-set! copy 'slot-symbol slot-symbol)
	      ...
	      copy))

	   ((assign source :: type-name):: type-name
	    (invoke-special parent (this) 'assign (as parent source))
	    (set! slot-symbol (field source 'slot-symbol))
	    ...
	    (this))

	   (slot-symbol . slot-spec)
	   ...
	   methods ...))

      ((_ type-name parent interfaces
	  (slot-keyword slot-type := value . fields)
	  (slot-definitions ...) spec methods)
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol (datum->syntax stx
                                    (keyword->symbol
                                     (syntax->datum #'slot-keyword)))))
	 #'(type-definition
	    type-name parent interfaces fields
	    (slot-definitions ... (slot-symbol type: slot-type
					       init: value))
	    spec methods)))

      ((_ type-name parent interfaces
	  (slot-keyword slot-type . fields)
	  (slot-definitions ...) spec methods)
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol (datum->syntax
				   stx
				   (keyword->symbol
				    (syntax->datum #'slot-keyword)))))
	 #'(type-definition
	    type-name parent interfaces fields
	    (slot-definitions ... (slot-symbol type: slot-type))
	    spec methods)))

      ((_ type-name parent interfaces
	  () slots (implementing interface
				 with (method . body) . spec)
	  (methods ...))
       #'(type-definition
	  type-name parent (interface . interfaces) () slots
	  spec (methods ... (method . body))))

      ((_ type-name parent interfaces
	  () slots (implementing interface . spec)
	  methods)
       #'(type-definition
	  type-name parent (interface . interfaces) () slots
	  spec methods))
      
      ((_ type-name Base interfaces
	  () slots (extending
		    parent
		    with (method . body) . spec) (methods ...))
       #'(type-definition
	  type-name parent interfaces () slots spec
	  (methods ... (method . body))))

      ((_ type-name Base interfaces
	  () slots (extending parent . spec) methods)
       #'(type-definition
	  type-name parent interfaces () slots spec methods))

      ((_ type-name parent interfaces
	  () slots ((method . body) . spec) (methods ...))
       #'(type-definition
	  type-name parent interfaces () slots
	  spec (methods ... (method . body))))
      )))

(define-syntax set-fields!
  (lambda (stx)
    (syntax-case stx ()
      ((_ instance key value . rest)
       (keyword? (syntax->datum #'key))
       (with-syntax ((symbol (datum->syntax
			      stx
			      (keyword->symbol
			       (syntax->datum #'key)))))
	 #'(begin
	     (slot-set! instance 'symbol value)
	     (set-fields! instance . rest))))
      ((_ instance)
       #'(values)))))
