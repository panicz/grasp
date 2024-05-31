(module-name (language match))

(import (language define-syntax-rule))
(import (language define-interface))
(import (utils conversions))

(define-interface Matchable ()
  (matches? x)::boolean)

(define-syntax-rule (match expression (pattern actions* ... value) ...)
  (let ((evaluated expression))
    (match/evaluated evaluated (pattern actions* ... value) ...)))

(define (match/equal? a b)
  (or (equal? a b)
      (and (Matchable? a)
	   (let ((a ::Matchable a))
	     (a:matches? b)))
      ;; this extension is used so that the EmptyListProxy
      ;; objects (defined in the (editor types spaces) module) are
      ;; indistinguishable from '() in the pattern context
      (and (gnu.lists.LList? a)
	   (gnu.lists.LList? b)
	   (not (gnu.lists.Pair? a))
	   (not (gnu.lists.Pair? b)))))

(define-syntax match/evaluated
  (syntax-rules (::)
    ((match/evaluated value)
     ;; This behavior is unspecified, and an "unspecified"
     ;; value would also be fine here.
     (error 'no-matching-pattern value))

    ((match/evaluated value (pattern::type actions ...) final-clause)
     (match-clause ((pattern::type value))
                   (and)
                   ()
                   actions ...
		   (with-compile-options
		    warn-unreachable: #f
                    (match/evaluated value final-clause))))
    
    ((match/evaluated value (pattern actions ...) final-clause)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
		   (with-compile-options
		    warn-unreachable: #f		   
                    (match/evaluated value final-clause))))
    
    ((match/evaluated value (pattern::type actions ...) . clauses)
     (match-clause ((pattern::type value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))
    
    ((match/evaluated value (pattern actions ...) . clauses)
     (match-clause ((pattern value))
                   (and)
                   ()
                   actions ...
                   (match/evaluated value . clauses)))
    ))

(define-syntax match-clause
  (lambda (stx)
    (syntax-case stx (quasiquote
		      unquote quote unquote-splicing
		      and _ %typename ::)
      ((match-clause () condition bindings actions ... alternative)
       #'(check/unique condition bindings #f () ()
		       actions ... alternative))

      ((match-clause ((`,pattern::type root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern::type root) . rest)
                       condition
                       bindings
                       actions ... alternative))
      
      ((match-clause ((`,pattern root) . rest)
                     condition
                     bindings
                     actions ... alternative)
       #'(match-clause ((pattern root) . rest)
                       condition
                       bindings
                       actions ... alternative))

      ((match-clause ((,value::type root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions
			...
			(instance? root type)
			(match/equal? value root))
                       bindings
                       actions ... alternative))
      
      ((match-clause ((,value root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (match/equal? value root))
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
      
      ((match-clause ((_::type root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause rest
                       (conditions ... (instance? root type))
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

      ((match-clause ((variable ::type root) . rest)
                     (conditions ...)
                     bindings
                     actions ... alternative)
       (identifier? #'variable)
       #'(match-clause rest
                       (conditions ... (instance? root type))
                       ((variable ::type root) . bindings)
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
                       (and conditions ... (match/equal? root 'datum))
                       bindings
                       actions ... alternative))

      ((match-clause ((`(left::type . right) root) . rest)
                     (and conditions ...)
                     bindings
                     actions ... alternative)
       #'(match-clause ((`left::type (car root))
			(`right (cdr root)) . rest)
                       (and conditions ... (pair? root))
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
                     (bindings ...)
                     actions ... alternative)
       (and (identifier? #'typename) (identifier? #'root))
       #'(match-clause (((%typename typename . fields) root) . rest)
                       (and conditions ... (instance? root typename))
                       (bindings ... (root ::typename root))
                       actions ... alternative))

      
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
                       (and conditions ... (match/equal? literal root))
                       bindings
                       actions ...))
      )))

(define-syntax check/unique
  (lambda (stx)
    "add equality checks for repeated identifiers in patterns and remove them from bindings"
    (syntax-case stx (and)
      ((check/unique condition #;unchecked ()
		     #;currently-checked #f
		     #;checked ()
		     #;final bindings actions ... alternative)
       #'(if condition
             (let bindings actions ...)
             alternative))

      ;; check the next binding from the list
      ((check/unique condition
                     ((variable type ... path) . bindings)
                     #f
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique condition
                       bindings
                       (variable type ... path)
                       bindings/checked
                       bindings/final
                       actions ... alternative))

      ;; the binding is present: add equality check
      ((check/unique (and conditions ...)
                     ((variable type ... path) . bindings)
                     (variable+ type+ ... path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       (bound-identifier=? #'variable #'variable+)
       #'(check/unique (and conditions ... (match/equal? path path+))
                       bindings
                       (variable+ type+ ... path+)
                       bindings/checked
                       bindings/final
                       actions ... alternative))
      
      ;; the binding is absent: go on
      ((check/unique conditions
                     ((variable type ... path) . bindings)
                     (variable+ type+ ... path+)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings
                       (variable+ type+ ... path+)
                       ((variable type ... path) . bindings/checked)
                       bindings/final
                       actions ... alternative))

      ;; add binding to the "checked" list
      ;; (and possibly start over)
      ((check/unique conditions
                     ()
                     (variable type ... path)
                     bindings/checked
                     bindings/final
                     actions ... alternative)
       #'(check/unique conditions
                       bindings/checked
		       #f
		       ()
                       ((variable type ... path) . bindings/final)
                       actions ... alternative))
      )))

(define-syntax match-let*
  (lambda (stx)
    (syntax-case stx ()
      ((_ ((pattern value) . rest) . body)
       (identifier? #'pattern)
       #'(let ((pattern value))
           (match-let* rest . body)))
      
      ((_ ((pattern::type value) . rest) . body)
       #'(match value
           (pattern
            (match-let* rest . body))
           (_
            (error "Value failed to match pattern: "'value 'pattern))))
      
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
    (syntax-case stx (::)

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
               (`(,value ,* ... . ,_)
                (and value
                     (and-let* rest . body)))
               (_ #f)))))

      ((_ ((value ... expression) . rest) . body)
       #'(call-with-values (lambda () expression)
           (lambda args
             (match args
               (`(,value ... . ,_)
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

(define-syntax-rule (begin/or clause ...)
  (let ((result #f))
    (set! result (or clause result))
    ...
    result))

(define-syntax-rule (begin/and clause ...)
  (let ((result #t))
    (set! result (and clause result))
    ...
    result))
