(module-name (editor input evaluation))

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
(import (utils functions))
(import (language examples))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor interfaces painting))
(import (editor types spaces))
(import (editor types texts))
(import (editor document parse))
(import (editor document documents))
(import (editor document cursor))
(import (language mapping))
(import (utils print))
(import (utils hash-table))

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

(invoke (default-context) 'define! (Atom "!")
  (car (parse-string "\
(lambda (n)
  (if (<= n 1)
     1 #| BASE CASE |#
     (* n (! (- n 1)))))")))

(invoke (default-context) 'define! (Atom "append")
  (car (parse-string "\
(lambda (a b)
  (if (null? a)
     b
     (cons (car a) (append (cdr a) b))))")))

(invoke
 (default-context) 'define! (Atom "ack")
  (car (parse-string "\
(lambda (m n)
  (if (<= m 0)
  (+ n 1)
  (if (= n 0)
  (ack (- m 1) 1)
  (ack (- m 1) (ack m (- n 1))))))")))

(define (grasp expression)
  (cond ((pair? expression)
	 (cons (grasp (car expression))
	       (grasp (cdr expression))))
	((null? expression)
	 (empty))
	((string? expression)
	 (text expression))
	((Atom? expression)
	 (copy expression))
	((symbol? expression)
	 (Atom (symbol->string expression)))
	((number? expression)
	 (Atom (number->string expression)))
	(else
	 (Atom (show->string expression)))))

#|
(define/kw (evaluate-expression! at: cursor ::Cursor := (the-cursor)
				 in: document ::Document := (the-document))
|#
