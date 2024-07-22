(module-name (editor input evaluation))

(import (srfi :11))
(import (language assert))
(import (language infix))
(import (language define-object))
(import (language define-type))
(import (language define-interface))
(import (language define-property))
(import (language define-parameter))
(import (language define-cache))
(import (language keyword-arguments))
(import (language match))
(import (language for))
(import (language examples))
(import (language fundamental))
(import (language mapping))

(import (utils functions))
(import (utils print))
(import (utils hash-table))

(import (editor interfaces elements))
(import (editor types primitive))
(import (editor interfaces painting))
(import (editor types spaces))
(import (editor types texts))
(import (editor document parse))
(import (editor document documents))
(import (editor document cursor))
(import (editor document history-tracking))
(import (editor types extensions extensions))

(define (self-evaluating? x)
  (or (and (pair? x)
	   (match/equal? (car x) 'lambda))
      (and (isnt x list?)
	   (isnt x pair?)
	   (if (Atom? x)
	       (let* ((x ::Atom (as Atom x)))
		 (isnt (x:value) symbol?))
	       #t))))

(define-property (preserve-identity? item::Tile)::boolean
  #f)

(define-object (EvaluationContext)
  ;;(define macro-definitions ::)

  (define definitions ::java.util.Map
    (let ((table ::java.util.Map (java.util.HashMap)))
      (define (add s::string v)
	(table:put (invoke (s:toString) 'intern) v))
      (add "+" +)
      (add "-" -)
      (add "pred" (lambda (x) (- x 1)))
      (add "*" *)
      (add "/" /)
      (add "<" <)
      (add "<=" <=)
      (add ">" >)
      (add ">=" >=)
      (add "=" =)
      (add "eq?" eq?)		
      (add "eqv?" eqv?)
      (add "cons"
	   (lambda args
	     (match args
	       (`(',a ',b)
		(cons (Atom "quote") (cons a b)))
	       (`(,a ',b)
		(cons (Atom "quote") (cons a b)))
	       (`(',a ,b)
		(cons (Atom "quote") (cons a b)))
	       (`(,a ,b)
		(cons (Atom "quote") (cons a b))))))
      (add "car"
	   (lambda (x)
	     (match x
	       (`'(,a . ,b)
		(if (self-evaluating? a)
		    a
		    (cons (Atom "quote") a))))))
      (add "cdr"
	   (lambda (x)
	     (match x
	       (`'(,a . ,b)
		(if (self-evaluating? b)
		    b
		    (cons (Atom "quote") b))))))
      (add "pair?"
	   (lambda (x)
	     (and-let* ((`'(,_ . ,_) x)))))
      (add "null?"
	   (lambda (x)
	     (and-let* ((`'() x)))))
      table))

  (define (value atom::Atom)
    (cond ((definitions:contains-key atom:name)
	   (definitions:get atom:name))
	  (else
	   (error "undefined symbol: "atom))))

  (define (defines-macro? symbol)
    #f)

  (define (defines? atom::Atom)
    (definitions:contains-key atom:name))

  (define (define! atom::Atom value)
    (definitions:put atom:name value))

  (define (primitive? atom::Atom)
    (and (definitions:contains-key atom:name)
	 (let ((value (definitions:get atom:name)))
	   (procedure? value))))
  )

(define-parameter (default-context) ::EvaluationContext
  (EvaluationContext))

(define (grasp expression)
  (cond
   ((and (Tile? expression)
	 (preserve-identity? expression))
    expression)
   ((pair? expression)
    (cons (grasp (car expression))
	  (grasp (cdr expression))))
   ((empty? expression)
    (empty))
   ((string? expression)
    (text expression))
   ((Atom? expression)
    (copy expression))
   ((symbol? expression)
    (Atom (symbol->string expression)))
   ((number? expression)
    (Atom (number->string expression)))
   ((boolean? expression)
    (if expression
	(Atom "#true")
	(Atom "#false")))
   ((Enchanted? expression)
    expression)
   (else
    (WARN "dont know what to do with "expression)
    (Atom (show->string expression)))))
  
(define/kw (evaluate-expression! at: source ::Cursor := (the-cursor)
				 in: document ::Document := (the-document))
  (let*-values (((terminal stem) (cursor-terminal+stem source
						       document))
		((cursor next)
		 (if (Space? terminal)
		     (let-values (((previous stem)
				   (cursor-terminal+stem
				    (cursor-retreat stem
						    document)
				    document)))
		       (values stem (cursor-advance stem
						    document)))
		     (values stem (cursor-advance stem
						  document)))))
    (safely
     (match (the-expression at: cursor in: document)
       (`(define (,name . ,args) . ,value)
	(invoke (default-context) 'define! name
		(cons (Atom "lambda") (cons args value))))
       
       (`(define ,name ,value)
	(invoke (default-context) 'define! name value))
       
       (_
	(values))))
    (safely
     (with-eval-access
      (let* ((expression (the-expression at: cursor in: document))
	     (definition (and-let* ((`(define ,head . ,_) expression))
			   (let bind ((head head))
			     (match head
			       (`(,head . ,_)
				(bind head))
			       (,@symbol?
				(eval `(define ,head #f))
				head))))))
	(future
	 (call-with-values (lambda ()
			     (if definition
				 (eval `(set! ,definition
					      (let ()
						,expression
						,definition)))
				 (eval expression)))
	   (lambda result
	     (unless (null? result)
	       (with-edit-access
		(let* ((result+ (grasp result))
		       (operation ::Insert (Insert element: result+
						   at: next))
		       (history ::History (history document)))
		  (history:record! operation)
		  (set! (the-cursor)
			(operation:apply! document)))))))))))))
