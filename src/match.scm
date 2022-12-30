(import (define-syntax-rule))
(import (conversions))

(define-syntax-rule (match expression (pattern actions* ... value) ...)
  (let ((evaluated expression))
    (match/evaluated evaluated (pattern actions* ... value) ...)))

(define-syntax match/evaluated
  (syntax-rules ()
    ((match/evaluated value)
     ;; This behavior is unspecified, and an "unspecified"
     ;; value would also be fine here.
     (error 'no-matching-pattern value))

    ((match/evaluated value (pattern actions ...) . clauses)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))))

(define-syntax match-clause
  (lambda (stx)
    (syntax-case stx (quasiquote
		      unquote quote unquote-splicing
		      and _ %typename)
      ((match-clause () condition bindings actions ... alternative)
       #'(check/unique condition bindings #f () ()
		       actions ... alternative))

      ((match-clause ((`,pattern root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern root) . rest)
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((,value root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (equal? value root))
                       bindings
                       actions ... alternative))

      ((match-clause ((,@predicate root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (predicate root))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((_ root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((variable root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       (identifier? #'variable)
       #'(match-clause rest
                       condition
                       ((variable root) . bindings)
                       actions ... alternative))

      ((match-clause (('datum root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ... (equal? root 'datum))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((`(left . right) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause ((`left (car root)) (`right (cdr root)) . rest)
                       (and conditions ... (pair? root))
                       bindings
                       actions ... alternative))

      ((match-clause ((`datum root) . rest)
                     conditions
                     bindings
                     actions ... alternative)
       #'(match-clause (('datum root) . rest)
                        conditions
                       bindings
                       actions ... alternative))
      
      ((match-clause (((_ . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause (((%typename . fields) root) . rest)
                       (and conditions ...)
                       bindings
                       actions ... alternative))
      
      ((match-clause (((%typename type) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (and conditions ...)
                       bindings
                       actions ... alternative))

      ((match-clause (((%typename type key pat . etc) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (and (keyword? (syntax->datum #'key))
	    (identifier? #'type))
       (with-syntax ((name (datum->syntax
			    stx
			    (keyword->symbol (syntax->datum #'key)))))
	 #'(match-clause (((%typename type . etc) root)
			  (pat (field (as type root) 'name)) . rest)
			 (and conditions ...)
			 bindings
			 actions ... alternative)))
      
      ((match-clause (((typename . fields) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       (identifier? #'typename)
       #'(match-clause (((%typename typename . fields) root) . rest)
                       (and conditions ... (instance? root typename))
                       bindings
                       actions ... alternative))

      ((match-clause ((literal root) . rest)
                     (and conditions ...)
                     bindings
                     actions ...)
       #'(match-clause rest
                       (and conditions ... (equal? literal root))
                       bindings
                       actions ...))
      )))

(define-syntax check/unique
  (lambda (stx)
    "add equality checks for repeated identifiers in patterns and remove them from bindings"
    (syntax-case stx (and)
      ((check/unique condition () #f () bindings actions ... alternative)
       #'(if condition
             (let bindings actions ...)
             alternative))

      ((check/unique condition
                     ((variable path) . bindings)
                     #f
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique condition
                       bindings
                       (variable path)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique (and conditions ...)
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (bound-identifier=? #'variable #'variable+)
       #'(check/unique (and conditions ... (equal? path path+))
                       bindings
                       (variable+ path+)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ((variable path) . bindings)
                     (variable+ path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings
                       (variable+ path+)
                       ((variable path) . bindings/checked)
                       bindings/final
                       actions ... alternative))

      ((check/unique conditions
                     ()
                     (variable path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings/checked
		       #f
		       ()
                       ((variable path) . bindings/final)
                       actions ... alternative))
      )))

(define-syntax match-let*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((pattern value) . rest) . body)
       (identifier? #'pattern)
       #'(let ((pattern value))
           (match-let* rest . body)))
      
      ((_ ((pattern value) . rest) . body)
       #'(match value
           (pattern
            (match-let* rest . body))
           (_
            (error "Value failed to match pattern: "'value 'pattern))))
      ((_ () . body)
       #'(let () . body)))))


(define-syntax and-let*
  (lambda (stx)
    (syntax-case stx (values ::)

      ((_)
       #'#t)

      ((_ ())
       #'#t)

      ((_ () . body)
       #'(let () . body))

      ((_ ((name binding) . rest) . body)
       (identifier? #'name)
       #'(let ((name binding))
           (and name
                (and-let* rest
                  . body))))

      ((_ ((name :: type binding) . rest) . body)
       (identifier? #'name)
       #'(let ((value binding))
	   (and (instance? value type)
		value
		(let ((name ::type value))
                  (and-let* rest
		    . body)))))
	   
      ((_ (((values . structure) binding) . rest) . body)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               (structure
                (and-let* rest . body))
               (_ #f)))))

      ((_ ((value binding) . rest) . body)
       #'(match binding
           (value
            (and-let* rest
              . body))
           (_ #f)))

      ((_ ((condition) . rest) . body)
       #'(and condition
              (and-let* rest . body)))

      ((_ ((value * ... expression) . rest) . body)
       (identifier? #'value)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               ((value * ... . _)
                (and value
                     (and-let* rest . body)))
               (_ #f)))))

      ((_ ((value ... expression) . rest) . body)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               ((value ... . _)
                (and-let* rest . body))
               (_ #f)))))

      )))

(define-syntax otherwise
  (syntax-rules ()
    ((_)
     #f)
    ((_ value)
     value)
    ((_ default . precedents)
     (or (otherwise . precedents) default))))
