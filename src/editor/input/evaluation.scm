(module-name (editor input evaluation))

(import (srfi :11))
(import (language assert))
(import (language infix))
(import (language define-object))
(import (language define-type))
(import (language define-interface))
(import (language attributes))
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
(import (editor document universe))
(import (editor document cursor))
(import (editor document history-tracking))
(import (editor types extensions extensions))

(define (self-evaluating? x)
  (or (and (pair? x)
	   (or (match/equal? (car x) 'lambda)
	       (match/equal? (car x) 'quote)))
      (and (isnt x list?)
	   (isnt x pair?)
	   (if (Atom? x)
	       (let* ((x ::Atom (as Atom x)))
		 (isnt (x:value) symbol?))
	       #t))))

(define-attribute (preserve-identity? item::Tile)::boolean
  #f)

(define-object (EvaluationContext)::java.lang.Cloneable
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
      (add "remainder" remainder)
      (add "<" <)
      (add "<=" <=)
      (add ">" >)
      (add ">=" >=)
      (add "=" =)
      (add "eq?" eq?)		
      (add "eqv?" eqv?)
      (add "equal?" equal?)
      (add "cons" pair)
      (add "car" car)
      (add "cdr" cdr)
      (add "pair?" pair?)
      (add "null?" null?)
      (add "even?" even?)
      (add "odd?" odd?)
      (add "host-apply" apply)
      (add "host-function?" procedure?)
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
	   (or (procedure? value)
	       (type? value)))))

  (define (clone)
    (let ((twin ::EvaluationContext (EvaluationContext)))
      (set! twin:definitions (copy definitions))
      twin))
  )

(define-parameter (default-context) ::EvaluationContext
  (EvaluationContext))

(define (grasp expression)
  (cond
   ((and (Tile? expression)
	 (preserve-identity? expression))
    expression)
   ((pair? expression)
    (let ((grasped (cons (grasp (car expression))
			 (grasp (cdr expression)))))
      (or
       (parameterize ((cell-access-mode
		       CellAccessMode:Evaluating))
	 (and-let* ((`(,keyword::symbol . ,data) grasped)
		    (magic ::Extension (extension keyword)))
	   (magic:enchant grasped)))
       grasped)))
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
   ((eq? expression #!null)
    (Atom "#!null"))
   (else
    (error "dont know what to do with "
	   expression(expression:getClass)))))

(define (apply-primitive operator operands)
  (grasp
   (parameterize ((cell-access-mode CellAccessMode:Evaluating))
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
	   result)))))

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
			     (try
			      (lambda ()
				(if definition
				    (eval `(set! ,definition
						 (let ()
						   ,expression
						   ,definition)))
				    (eval expression)))))
	   (lambda result
	     (unless (null? result)
	       (with-edit-access
		(let* ((result+ (grasp result))
		       (operation ::Insert
				  (Insert element: result+
					  at: next))
		       (history ::History (history
					   document)))
		  (history:record! operation)
		  (set! (the-cursor)
			(operation:apply! document)))))))))))))
