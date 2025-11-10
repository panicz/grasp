(import (language assert))
(import (language infix))
(import (language define-object))
(import (language define-type))
(import (language match))
(import (utils functions))
(import (language examples))
(import (utils print))
(import (kawa pprint))
(import (language for))


;; This is a reference stepper module, from which
;; the actual stepper is derived. The difference
;; is that this module has a really small set of
;; dependencies and it should be easily made to work
;; with.

(define (self-evaluating? x)
  (or (and-let* ((`(lambda ,args ,body) x)))
      (and-let* ((`(quote ,_) x)))
      (and (isnt x list?)
	   (isnt x pair?)
	   #;(isnt x symbol?))))

(define-object (EvaluationContext)
  ;;(define macro-definitions ::)

  (define definitions ::java.util.Map
    (let ((table ::java.util.Map (java.util.HashMap)))
      (table:put '+ +)
      (table:put '- -)
      (table:put 'pred (lambda (x) (- x 1)))
      (table:put '* *)
      (table:put '/ /)
      (table:put 'remainder remainder)
      (table:put '< <)
      (table:put '<= <=)
      (table:put '> >)
      (table:put '>= >=)
      (table:put '= =)
      (table:put 'eq? eq?)		
      (table:put 'eqv? eqv?)
      (table:put 'equal? equal?)
      (table:put 'cons cons)
      (table:put 'car car)
      (table:put 'cdr cdr)
      (table:put 'list list)
      (table:put 'pair? pair?)
      (table:put 'null? null?)
      (table:put 'even? even?)
      (table:put 'odd? odd?)
      (table:put 'symbol? symbol?)
      (table:put 'host-apply apply)
      (table:put 'host-function? procedure?)
      table))

  (define (value symbol)
    (cond ((definitions:contains-key symbol)
	   (definitions:get symbol))
	  (else
	   (error "undefined symbol: "symbol))))

  (define (defines-macro? symbol)
    #f)

  (define (defines? symbol)
    (definitions:contains-key symbol))

  (define (define! name value)
    (definitions:put name value))

  (define (primitive? symbol)
    (and (definitions:contains-key symbol)
	 (let ((value (definitions:get symbol)))
	   (procedure? value))))
  )

(define default-context ::EvaluationContext
  (EvaluationContext))

(default-context:define! '!
  '(lambda (n)
     (if (<= n 1)
	 1
	 (* n (! (- n 1))))))

(default-context:define! 'append
  '(lambda (a b)
     (if (null? a)
	 b
	 (cons (car a) (append (cdr a) b)))))

(define (apply-primitive operator operands)
  (let* ((operands (map (lambda (arg)
			  (match arg
			    (`(quote ,value) value)
			    (_               arg)))
			operands))
	 (result (apply operator operands)))
    (if (or (pair? result)
	    (list? result)
	    (symbol? result))
	`',result
	result)))

(define (reduce expression #!optional (context::EvaluationContext
				       default-context))
  (match expression
    (`(if #f ,then ,else)
     else)
    (`(if ,test ,then ,else)
     (let ((test* (reduce test context)))
       (if (equal? test test*)
	   then
	   `(if ,test* ,then ,else))))
    (`(lambda ,args ,body)
     expression)
    (`(quote ,_)
     expression)
    (`(,operator . ,operands)
     (if (and (symbol? operator)
	      (context:defines-macro? operator))
	 (error "Macros not supported (yet)")
	 (let ((operands* (reduce-operands operands context)))
	   (if (isnt operands equal? operands*)
	       `(,operator . ,operands*)
	       (match operator
		 (,@symbol?
		  (cond ((context:primitive? operator)
			 (apply-primitive
			  (context:value operator)
			  operands))
			((context:defines? operator)
			 (reduce `(,(context:value operator)
				   . ,operands)
				 context))
			(else
			 `(,operator . ,operands))))
		 (`(lambda ,args ,body)
		  (substitute args #;with operands
			      #;in body))
		 (`(,_ . ,_)
		  (let ((operator* (reduce operator
					   context)))
		    `(,operator* . ,operands)))
		 (_
		  `(,operator . ,operands)))))))
    (_
     (if (and (symbol? expression)
	      (context:defines? expression)
	      (isnt expression context:primitive?))
	 (context:value expression)
	 expression))))

(define (reduce-operands operands
			 #!optional
			 (context::EvaluationContext default-context))
  (match operands
    (`(,first . ,rest)
     (let ((first* (reduce first context)))
       (if (equal? first first*)
	   `(,first . ,(reduce-operands rest context))
	   `(,first* . ,rest))))
    ('()
     '())
    (_
     (reduce operands context))))

(define (in. element collection)
  (any. (is _ eq? element) collection))

(define (substitute variables #;with values #;in expression)
  (match expression
    (`(quote ,_)
     expression)
    (`(lambda ,args ,body)
     (let-values (((variables* values*) (only. (lambda (var value)
						 (isnt var in. args))
					       variables values)))
       `(lambda ,args
	  ,(substitute variables* #;with values*
		       #;in body))))
    (`(,operator . ,operands)
     `(,(substitute variables #;with values #;in operator)
       . ,(substitute variables #;with values #;in operands)))
    (_
     (if (symbol? expression)
	 (counterpart #;of expression #;from variables
			   #;in values)
	 expression))))

(define (counterpart #;of variable #;from variables
			  #;in values)
  (match variables
    (`(,,variable . ,_)
     (let ((result (car values)))
       (if (self-evaluating? result)
	   result
	   `',result)))
    (,variable
     `',values)
    (`(,_ . ,rest)
     (counterpart #;of variable #;from rest
		       #;in (cdr values)))
    (_
     variable)))
	       
(e.g.
 (fix-list reduce '(! 5))
 ===>
 ((! 5)
  
  (if (<= 5 1)
      1
      (* 5 (! (- 5 1))))
  
  (if #f
      1
      (* 5 (! (- 5 1))))
  
  (* 5 (! (- 5 1)))
  
  (* 5 (! 4))
  
  (* 5 (if (<= 4 1)
	   1
	   (* 4 (! (- 4 1)))))
  
  (* 5 (if #f
	   1
	   (* 4 (! (- 4 1)))))
  
  (* 5 (* 4 (! (- 4 1))))
  
  (* 5 (* 4 (! 3)))
  
  (* 5 (* 4 (if (<= 3 1)
		1
		(* 3 (! (- 3 1))))))
  
  (* 5 (* 4 (if #f
		1
		(* 3 (! (- 3 1))))))
  
  (* 5 (* 4 (* 3 (! (- 3 1)))))
  
  (* 5 (* 4 (* 3 (! 2))))
  
  (* 5 (* 4 (* 3 (if (<= 2 1)
		     1
		     (* 2 (! (- 2 1)))))))
  
  (* 5 (* 4 (* 3 (if #f
		     1
		     (* 2 (! (- 2 1)))))))
  
  (* 5 (* 4 (* 3 (* 2 (! (- 2 1))))))
  
  (* 5 (* 4 (* 3 (* 2 (! 1)))))
  
  (* 5 (* 4 (* 3 (* 2 (if (<= 1 1)
			  1
			  (* 1 (! (- 1 1))))))))
  
  (* 5 (* 4 (* 3 (* 2 (if #t
			  1
			  (* 1 (! (- 1 1))))))))

  (* 5 (* 4 (* 3 (* 2 1))))

  (* 5 (* 4 (* 3 2)))

  (* 5 (* 4 6))

  (* 5 24)

  120))
	   
(e.g.
 (fix-list reduce '(append '(1 2) '(3 4 5)))
 ===>
 ((append '(1 2) '(3 4 5))

  (if (null? '(1 2))
      '(3 4 5)
      (cons (car '(1 2)) (append (cdr '(1 2)) '(3 4 5))))
  
  (if #f
      '(3 4 5)
      (cons (car '(1 2)) (append (cdr '(1 2)) '(3 4 5))))
  
 (cons (car '(1 2)) (append (cdr '(1 2)) '(3 4 5)))

 (cons 1 (append (cdr '(1 2)) '(3 4 5)))

 (cons 1 (append '(2) '(3 4 5)))

 (cons 1 (if (null? '(2))
	     '(3 4 5)
	     (cons (car '(2)) (append (cdr '(2)) '(3 4 5)))))
 (cons 1 (if #f
	     '(3 4 5)
	     (cons (car '(2)) (append (cdr '(2)) '(3 4 5)))))
 
 (cons 1 (cons (car '(2)) (append (cdr '(2)) '(3 4 5))))
 
 (cons 1 (cons 2 (append (cdr '(2)) '(3 4 5))))
 
 (cons 1 (cons 2 (append '() '(3 4 5))))
 
 (cons 1 (cons 2 (if (null? '())
		     '(3 4 5)
		     (cons (car '()) (append (cdr '()) '(3 4 5))))))
 
 (cons 1 (cons 2 (if #t
		     '(3 4 5)
		     (cons (car '()) (append (cdr '()) '(3 4 5))))))
 
 (cons 1 (cons 2 '(3 4 5)))

 (cons 1 '(2 3 4 5))

 '(1 2 3 4 5)))

(default-context:define! 'each
  '(lambda (f l)
     (if (null? l)
	 '()
	 (cons (f (car l))
	       (each f (cdr l))))))

#;(default-context:define! 'square
  '(lambda (x)
     (* x x)))

(default-context:definitions:put 'square (lambda (x) (* x x)))

(define (pprinting function)
  (lambda args
    (let ((result (apply function args)))
      (pprint result)
      (newline)
      result)))

(fix-list (pprinting reduce) '(each square '(1 2 3)))

(default-context:define! 'lookup
  '(lambda (key dictionary) 
     (if (eq? (car (car dictionary)) key)
	 (cdr (car dictionary)) 
	 (lookup key (cdr dictionary)))))

(default-context:define! 'run
  '(lambda (prog env) 
     (if (eq? (expression-type (car prog)) 'define)
	 (run (cdr prog) 
	      (cons 
	       (cons (second (car prog)) 
		     (third (car prog))) 
	       env)) 
	 (value (car prog) env))))

(default-context:define! 'expression-type
  '(lambda ( expression)
     (if (pair? expression)
	 (car expression)
	 'literal)))

(default-context:define! 'value
  '(lambda (exp env) 
     #| If the expression is a symbol, 
     then we need to look up its value in
     the environment (to make the recursion 
     work, we also calculate the value 
     after the lookup): |#
     (if (symbol? exp) 
	 (value (lookup exp env) env) 
	 #| In case of quote, we simply return 
	 the quoted expression: |#
	 (if (eq? (expression-type exp) 'quote)  
	     (second exp)
	     #| In case of if, we first evaluate 
	     the condition (second subexpression).
	     If it succeeds, we return the value 
	     of the consequent (third subexpression), 
	     and otherwise we return the value of 
	     alternative (fourth subexpression): |#
	     (if (eq? (expression-type exp) 'if)
		 (if (value (second exp) env) 
		     (value (third exp) env) 
		     (value (fourth exp) env))
		 #| We can consider functions to be 
		 self-evaluating: |#
		 (if (eq? (expression-type exp) 'lambda)
		     exp 
		     #| The case of function application 
		     will be explained shortly.  Note that
		     we apply the value of the head of 
		     a function to the values of arguments: |#
		     (if (pair? exp) 
			 (applied (value (car exp) env)
				  (map (lambda (arg)
					 (value arg env))
				       (cdr exp))
				  env)
			 #| If the expression does not belong 
			 to any of the aforementioned types
			 (e.g. it is a number or a boolean value), 
			 then we do not reduce it any further. |#
			 exp)))))))


(default-context:define! 'applied
  '(lambda (operator arguments env) 
     (if (host-function? operator) 
	 (host-apply operator arguments) 
	 (run (cdr (cdr operator)) 
	      (extended env 
			(second operator) 
			arguments)))))

(default-context:define! 'extended
  '(lambda (env names vals) 
     (if (pair? names) 
	 (extended 
	  (cons (cons (car names) 
		      (list 'quote 
			    (car vals))) 
		env) 
	  (cdr names) 
	  (cdr vals)) 
	 (if (symbol? names) 
	     (cons (cons names  
			 (list 'quote  
			       vals))  
		   env) 
	     env))))

(default-context:define! 'initial-environment 
  (list 
   (cons '* *)
   (cons '+ +)
   (cons '- -)
   (cons '= =)
   (cons '< <)
   (cons 'eq? eq?)
   (cons 'list list)
   (cons 'cons cons)
   (cons 'car car)
   (cons 'cdr cdr)
   (cons 'pair? pair?)
   (cons 'symbol? symbol?)
   (cons 'fresh-symbol fresh-symbol)))

(pass
 '(run
   '((define ! (lambda (n)
		 (if (< n 1)
		     1
		     (* n (! (- n 1))))))
     (! 5))
   initial-environment)
 (pprinting reduce)
 (pprinting reduce)

 )
