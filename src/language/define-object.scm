(module-name (language define-object))

(import (language define-syntax-rule))

(define-syntax object-definition
  (lambda (stx)
    (syntax-case stx (::
		      define
		      define-private
		      set!
		      assert
		      invoke
		      $lookup$
		      let
		      let*)
      
      ((object-definition (object-name . args)
			  (arg :: type . rest)
			  supers
			  (slots ...)
			  methods
			  (initializers ...)
			  spec)
       #'(object-definition (object-name . args)
			    rest
			    supers
			    (slots ... (arg :: type))
			    methods
			    (initializers
			     ...
			     (slot-set! (this) 'arg arg))
			    spec))

      ((object-definition (object-name . args)
			  (arg . rest)
			  supers
			  (slots ...)
			  methods
			  (initializers ...)
			  spec)
       #'(object-definition (object-name . args)
			    rest
			    supers
			    (slots ... (arg))
			    methods
			    (initializers
			     ...
			     (slot-set! (this) 'arg arg))
			    spec))

      ((object-definition (object-name . args)
			  rest
			  supers
			  (slots ...)
			  methods
			  (initializers ...)
			  spec)
       (identifier? #'rest)
       #'(object-definition (object-name . args)
			    ()
			    supers
			    (slots ... (rest::list))
			    methods
			    (initializers
			     ...
			     (slot-set! (this) 'rest rest))
			    spec))
      
      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  methods
			  initializers
			  (:: type . spec))
       #'(object-definition (object-name . args)
			    ()
			    (supers ... type)
			    slots
			    methods
			    initializers
			    spec))

      ((object-definition (object-name . args)
			  ()
			  supers
			  slots
			  (methods ...)
			  initializers
			  ((define-private (method . params)
			     . body)
			   . spec))
       #'(object-definition (object-name . args)
			    ()
			    supers
			    slots
			    (methods
			     ...
			     ((method . params)
			      access: 'private . body))
			    initializers
			    spec))

      ((object-definition (object-name . args)
			  ()
			  supers
			  slots
			  (methods ...)
			  initializers
			  ((define (method . params) . body) . spec))
       #'(object-definition (object-name . args)
			    ()
			    supers
			    slots
			    (methods ... ((method . params) . body))
			    initializers
			    spec))

      ((object-definition (object-name . args)
			  ()
			  supers
			  (slots ...)
			  methods
			  (initializers ...)
			  ((define slot :: type value) . spec))
       #'(object-definition (object-name . args)
			    ()
			    supers
			    (slots
			     ...
			     (slot :: type))
			    methods
			    (initializers
			     ...
			     (set! slot value))
			    spec))

      ((object-definition (object-name . args)
			  ()
			  supers
			  (slots ...)
			  methods
			  initializers
			  ((define slot :: type) . spec))
       #'(object-definition (object-name . args)
			    ()
			    supers
			    (slots ... (slot :: type))
			    methods
			    initializers
			    spec))
      
      ((object-definition (object-name . args)
			  ()
			  supers
			  (slots ...)
			  methods
			  (initializers ...)
			  ((define slot value) . spec))
       #'(object-definition (object-name . args)
			    ()
			    supers
			    (slots ... (slot))
			    methods
			    (initializers
			     ...
			     (set! slot value))
			    spec))

      ((object-definition (object-name)
			  ()
			  (supers ...)
			  (slots ...)
			  (methods ...)
			  ()
			  ())
       #'(define-simple-class object-name (supers ...)
	   slots ... methods ...))

      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ())
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...))
			    ()
			    ()))
      
      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((set! property value) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (set! property value) . init))
			    ()
			    ()))

      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((assert . condition) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (assert . condition) . init))
			    ()
			    ()))
      
      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((invoke . some-method) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (invoke . some-method) . init))
			    ()
			    ()))

      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((($lookup$ . some) . thing) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (($lookup$ . some) . thing) . init))
			    ()
			    ()))

      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((let . something) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (let . something) . init))
			    ()
			    ()))

      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((let* . something) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ...)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      initializers ...
			      (let* . something) . init))
			    ()
			    ()))
      
      ((object-definition (object-name . args)
			  ()
			  (supers ...)
			  slots
			  (methods ...)
			  (initializers ...)
			  ((super . args*) . init))
       #'(object-definition (object-name)
			    ()
			    (supers ... super)
			    slots
			    (methods
			     ...
			     ((*init* . args)
			      (invoke-special super (this)
					      '*init*
					      . args*)
			      initializers ...
			      . init))
			    ()
			    ()))
      )))

(define-syntax-rule (define-object (object-name . args) . spec)
  (object-definition (object-name . args)
		     #;args
		     args
		     #;supers
		     ()
		     #;slots
		     ()
		     #;methods
		     ()
		     #;initializers
		     ()
		     #;spec
		     spec))

(define-syntax-rule (invoke-on object (method . args) ...)
  (let ((result object))
    (object:method . args)
    ...
    result))
