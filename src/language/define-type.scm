(module-name (language define-type))

(import (utils conversions))
(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language attributes))
(import (language match))
(import (language fundamental))

(define-syntax maps
  (syntax-rules (to:)
    ((_ input-types to: output-type + ...)
     procedure)))

(define-syntax !maps
  (syntax-rules (to:)
    ((_ input-types to: output-type + ...)
     procedure)))

(define-syntax chain
  (syntax-rules (quote unquote _)
    ((chain _ . rest)
     (lambda (x)
       (chain x . rest)))
    ((chain object)
     object)
    ((chain object 'method . rest)
     (chain (invoke object 'method) . rest))
    ((chain object ('method . args) . rest)
     (chain (invoke object 'method . args) . rest))
    ((chain object ,function . rest)
     (chain (function object) . rest))))

#|
(define-simple-class Struct ()
  interface: #t
  ((typename) :: String #!abstract)
  ((fields->string) :: String #!abstract)
  ((toString) :: String #!abstract)
  ((assign source :: Struct) :: Struct #!abstract)
  ((clone) :: Struct #!abstract))
|#

(define-interface ListSerializable ()
  (to-list kons::procedure transform::procedure)::list
  ;; cons here is passed as an argument, because
  ;; we want to use the variant of "cons" from
  ;; the (editor types primitive) module, rather than the built-in.
  ;; (if we remove this duality by patching Kawa,
  ;; this argument can be removed)
  (fields->list kons::procedure transform::procedure)::list
  )

(define-interface Struct (java.lang.Cloneable ListSerializable)
  (typename) :: String
  (fields->string) :: String
  (toString) :: String
  ;;(embedded-in? object)::boolean
  (assign source :: Struct) :: Struct
  (clone) :: Struct
  ;;(deep-copy)::Struct
  )

(define-simple-class Base (Struct)
  ((typename)::String "Base")
  ((fields->string)::String "")
  ((toString)::String
   (string-append "["(typename) (fields->string)"]"))
  ((embedded-in? object)::boolean (instance? object Base))
  ((assign source::Struct)::Struct (this))
  ((fields->list kons::procedure transform::procedure)::list '())
  ((to-list kons::procedure transform::procedure)::list
   (kons (transform (string->symbol (typename)))
		 (fields->list kons transform)))
  ((hashCode)::int (*:hashCode 'Base))
  ((clone)::Struct (Base)))

(define-syntax-rule (define-type (type-name . fields) . spec)
  (type-definition type-name Base () fields () spec () ()))

(define-syntax keyword-value-list
  (syntax-rules ()
    ((_ kons transform final)
     final)
    ((_ kons transform final first . rest)
     (kons
      (transform (symbol->keyword 'first))
      (kons (transform first)
	    (keyword-value-list kons transform final . rest))))
    ))

(define-early-constant constructor
  (attribute (type-name::symbol)
      (lambda (_) #!null)))

(define (construct struct-spec::list)::Struct
  ((constructor (car struct-spec)) (cdr struct-spec)))

(define-syntax type-definition
  (lambda (stx)
    (syntax-case stx (Base extending implementing with :=)

      ((_ type-name parent interfaces
	  () ((slot-symbol . slot-spec) ...) () (methods ...)
	  (initializers ...))
       #'(begin
	   (define-simple-class type-name (parent . interfaces)
	     ((typename):: String
	      (symbol->string 'type-name))

	     ((fields->string)::String
	      (string-append
	       (invoke-special parent (this) 'fields->string)
	       (string-append " " (symbol->string 'slot-symbol)
			      ": "(java.lang.String:valueOf
				   slot-symbol))
	       ...))

	     ((fields->list kons::procedure transform::procedure)
	      ::list
	      (keyword-value-list
	       kons transform
	       (invoke-special parent (this)
			       'fields->list
			       kons transform)
	       slot-symbol ...))

	     ((embedded-in? object)::boolean
	      (and (instance? object type-name)
		   (invoke-special parent (this) 'embedded-in? object)
		   (equal? slot-symbol (field object 'slot-symbol))
		   ...))

	     ((hashCode)::int
	      ;; the hash function could likely be improved
	      (as int
		  (+ (*:hashCode type-name)
		     (if (eq? slot-symbol #!null)
			 0
			 (*:hashCode slot-symbol))
		     ...
		     (invoke-special parent (this) 'hashCode))))

	     ((equals object)::boolean
	      (and (instance? object type-name)
		   (invoke (as type-name object)
			   'embedded-in? (this))))

	     ((clone):: type-name
	      (let ((copy (type-name)))
		(invoke copy 'assign (as parent (this)))
		;;(invoke-special parent copy 'assign (this))
		(slot-set! copy 'slot-symbol slot-symbol)
		...
		copy))

	     ((assign source ::type-name)::type-name
	      (invoke-special parent (this) 'assign
			      (as parent source))
	      (set! slot-symbol (field source 'slot-symbol))
	      ...
	      (this))
	     ((*init*)
	      initializers ...
	      #!void)
	     (slot-symbol . slot-spec)
	     ...
	     methods ...)
	   (set! (constructor 'type-name)
		 (lambda (properties)
		   (with-compile-options
		    warn-unreachable: #f
		   (let ((item (type-name)))
		     (let init ((properties properties))
		       (otherwise item
			 (and-let* ((`(,key ,value . ,rest)
				     properties))
			   (slot-set! item (keyword->symbol key)
				      value)
			   (init rest))))))))))

      ((_ type-name parent interfaces
	  (slot-keyword slot-type := value . fields)
	  (slot-definitions ...) spec methods (initializers ...))
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol
		      (datum->syntax
		       stx
                       (keyword->symbol
                        (syntax->datum #'slot-keyword)))))
	 #'(type-definition
	    type-name parent interfaces fields
	    (slot-definitions ... (slot-symbol type: slot-type))
	    spec methods (initializers ... (set! slot-symbol value)))))

      ((_ type-name parent interfaces
	  (slot-keyword slot-type . fields)
	  (slot-definitions ...) spec methods (initializers ...))
       (keyword? (syntax->datum #'slot-keyword))
       (with-syntax ((slot-symbol (datum->syntax
				   stx
				   (keyword->symbol
				    (syntax->datum #'slot-keyword)))))
	 #'(type-definition
	    type-name parent interfaces fields
	    (slot-definitions ... (slot-symbol type: slot-type))
	    spec methods (initializers ...))))

      ((_ type-name parent interfaces
	  () slots (implementing interface
				 with (method . body) . spec)
	  (methods ...) initializers)
       #'(type-definition
	  type-name parent (interface . interfaces) () slots
	  spec (methods ... (method . body)) initializers))

      ((_ type-name parent interfaces
	  () slots (implementing interface . spec)
	  methods initializers)
       #'(type-definition
	  type-name parent (interface . interfaces) () slots
	  spec methods initializers))

      ((_ type-name Base interfaces
	  () slots (extending
		    parent
		    with (method . body) . spec) (methods ...)
		    initializers)
       #'(type-definition
	  type-name parent interfaces () slots spec
	  (methods ... (method . body)) initializers))

      ((_ type-name Base interfaces
	  () slots (extending parent . spec) methods initializers)
       #'(type-definition
	  type-name parent interfaces () slots spec methods initializers))

      ((_ type-name parent interfaces
	  () slots ((method . body) . spec) (methods ...) initializers)
       #'(type-definition
	  type-name parent interfaces () slots
	  spec (methods ... (method . body)) initializers))
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

(define-type (Extent width: real := 0
                     height: real := 0))
