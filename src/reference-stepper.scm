(import (assert))
(import (infix))
(import (define-object))
(import (define-type))
(import (match))
(import (functions))
(import (examples))

;; This is a reference stepper module, from which
;; the actual stepper is derived. The difference
;; is that this module has a really small set of
;; dependencies and it should be easily made to work
;; with.

(define (self-evaluating? x)
  (and (isnt x list?)
       (isnt x pair?)
       (isnt x symbol?)))

(define-object (EvaluationContext)
  ;;(define macro-definitions ::)

  (define definitions ::java.util.Map
    (let ((table ::java.util.Map (java.util.HashMap)))
      (table:put '+ +)
      (table:put '- -)
      (table:put '* *)
      (table:put '/ /)
      (table:put '< <)
      (table:put '<= <=)
      (table:put '> >)
      (table:put '>= >=)
      (table:put '= =)
      (table:put 'eq? eq?)		
      (table:put 'eqv? eqv?)
      (table:put 'cons (lambda args
			 (match args
			   (`(',a ',b)
			    `',(cons a b))
			   (`(,a ',b)
			    `',(cons a b))
			   (`(',a ,b)
			    `',(cons a b))
			   (`(,a ,b)
			    `',(cons a b)))))
      (table:put 'car (lambda (x)
			(match x
			  (`'(,a . ,b)
			   (if (self-evaluating? a)
			       a
			       `',a)))))
      (table:put 'cdr (lambda (x)
			(match x
			  (`'(,a . ,b)
			   (if (self-evaluating? b)
			       b
			       `',b)))))
      (table:put 'pair? (lambda (x)
			  (and-let* ((`'(,_ . ,_) x)))))
      (table:put 'null? (lambda (x)
			  (and-let* ((`'() x)))))
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
			 (apply (context:value operator)
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
	      (context:defines? expression))
	 (context:value expression)
	 expression))))

(define (reduce-operands operands #!optional (context::EvaluationContext
			      		      default-context))
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
     (let-values (((variables* values*) (only. (isnt _ in. args)
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

 (quote (1 2 3 4 5))))
