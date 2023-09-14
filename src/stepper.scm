(import (assert))
(import (infix))
(import (define-object))
(import (define-type))
(import (define-property))
(import (define-cache))
(import (keyword-arguments))
(import (extent))
(import (match))
(import (for))
(import (functions))
(import (examples))
(import (fundamental))
(import (indexable))
(import (primitive))
(import (painter))
(import (space))
(import (text))
(import (comments))
(import (interactive))
(import (extension))

(define (render-foreground! expression::Element
			    counterparts::(maps (Element)
						to: (list-of
						     Element))
			    source-position::(maps (Element)
						   to: Position)
			    target-position::(maps (Element)
						   to: Position)
			    intensity::float)
  ::void
  (let ((links (counterparts expression))
	(painter ::Painter (the-painter)))
    (cond
     ((empty? links)
      (draw-emerging! expression
		      (source-position expression)
		      intensity)
      (when (gnu.lists.LList? expression)
	(traverse
	 expression
	 doing:
	 (lambda (sub::Element t::Traversal)
	   (render-foreground! sub counterparts
			       source-position
			       target-position
			       intensity)))))

     (else
      (for x in links
	(draw-morph! expression x counterparts
		     source-position
		     target-position
		     intensity))))))


(define (draw-morph! foreground::Element
		     background::Element
		     counterparts::(maps (Element)
					 to: (list-of
					      Element))
		     source-position::(maps (Element)
					    to: Position)
		     target-position::(maps (Element)
					    to: Position)
		     progress::float)
  ::void
  (let* ((p0 ::Position (source-position foreground))
	 (p1 ::Position (target-position background))
	 (painter ::Painter (the-painter))
	 (left ::real (linear-interpolation
		       from: p0:left to: p1:left
		       at: progress))
	 (top ::real (linear-interpolation
		      from: p0:top to: p1:top
		      at: progress)))
    (cond
     ((equal? foreground background)
      ;; here we just draw the foreground
      ;; with full intensity
      (with-translation (left top)
	(draw! foreground)))

     ((or (isnt foreground Tile?)
	  (isnt background Tile?))
      ;; at least one of the elements is (presumably)
      ;; a space, so the only way we can morph them
      ;; is by fading
      (with-translation (left top)
	(painter:with-intensity (- 1.0 progress)
	  (lambda ()
	    (draw! background)))
	(painter:with-intensity progress
	  (lambda ()
	    (draw! foreground)))))

     ((and (gnu.lists.LList? foreground)
	   (gnu.lists.LList? background))
      (let* ((e0 ::Extent (extent foreground))
	     (e1 ::Extent (extent background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: progress))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: progress)))
	(with-translation (left top)
	  (painter:draw-box! width height '()))
	(traverse
	 foreground
	 doing:
	 (lambda (item::Element t::Traversal)
	   (render-foreground! item
			       counterparts
			       source-position
			       target-position
			       progress)))))
     ((and (Tile? foreground)
	   (Tile? background))
      (let* ((e0 ::Extent (extent foreground))
	     (e1 ::Extent (extent background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: progress))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: progress)))
	(with-translation (left top)
	  (painter:with-intensity (- 1.0 progress)
	    (lambda ()
	      (painter:with-stretch
		  (/ width e1:width)
		  (/ height e1:height)
		(lambda ()
		  (draw! background)))))
	  (painter:with-intensity progress
	    (lambda ()
	      (painter:with-stretch
		  (/ width e0:width)
		  (/ height e0:height)
		(draw! foreground)))))))
     )))

(define (draw-emerging! expression::Element p::Position
			intensity::float)
  ::void
  (let ((painter ::Painter (the-painter)))
    (painter:with-intensity intensity
      (lambda ()
	(with-translation (p:left p:top)
	  (if (gnu.lists.LList? expression)
	      (let ((outer ::Extent (extent expression)))
		(painter:draw-box! outer:width outer:height '()))
	      (draw! expression)))))))

(define (render-background! expression::Element
			    counterparts::(maps (Element)
						to: (list-of
						     Element))
			    position::(maps (Element)
					    to: Position)
			    intensity::float)
  ::void
  (when (empty? (counterparts expression))
    (draw-emerging! expression (position expression) intensity))
  (when (gnu.lists.LList? expression)
    (traverse
     expression
     doing:
     (lambda (sub::Element t::Traversal)
       (render-background! sub counterparts position
			   intensity)))))

(define/kw (measure-positions!
	    expression
	    left::real := 0
	    top::real := 0
	    into:
	    measurements::(!maps (Element) to: Position)
	    := (property+ (element::Element)::Position
			  (Position left: 0 top: 0)))
  ::(maps (Element) to: Position)
  (when (list? expression)
    (traverse
     expression
     doing:
     (lambda (item::Element t::Traversal)
       (let ((p ::Position (measurements item)))
	 (set! p:left (+ t:left left))
	 (set! p:top (+ t:top top))
	 (when (list? item)
	   (measure-positions! item p:left p:top
			       into: measurements))))
     returning:
     (lambda (t::Traversal)
       measurements))))

(define-object (Morph initial::Tile
		      final::Tile
		      origin::(maps (Element) to: (list-of
						   Element))
		      progeny::(maps (Element) to: (list-of
						    Element)))
  ::Enchanted
  (define progress ::float 0.0)

  (define initial-position ::(maps (Element) to: (list-of Element))
    (measure-positions! initial))

  (define initial-extent ::Extent
    (initial:extent))

  (define final-position ::(maps (Element) to: (list-of Element))
    (measure-positions! final))

  (define final-extent ::Extent
    (final:extent))

  (define maximum-extent ::Extent
    (Extent width: (max initial-extent:width
			final-extent:width)
	    height: (max initial-extent:height
			 final-extent:height)))

  (define (extent) ::Extent maximum-extent)

  (define shift ::(maps (Element) to: Position)
    (property+ (element::Element)::Position
	       (Position left: 0 top: 0)))

  (define (draw! context::Cursor)::void
    (cond ((is progress <= 0.5) ;>
	   (render-background! final origin final-position
			       progress)
	   (render-foreground! initial
			       progeny
			       initial-position
			       final-position
			       (- 1.0 progress)))
	  (else
	   (render-background! initial progeny
			       initial-position
			       (- 1.0 progress))
	   (render-foreground! final
			       origin
			       final-position
			       initial-position
			       progress))))
  (Magic))

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

(define (reduce expression
		origin::(maps (Element) to: (list-of Element))
		progeny::(maps (Element) to: (list-of Element))
		#!optional
		(context::EvaluationContext default-context))
  (define (mark-origin! newborn parent)
    (set! (origin newborn) (recons parent '()))
    (set! (progeny parent) (recons newborn '())))

  (define (dissolve! item)
    (set! (origin item) '())
    (when (gnu.mapping.LList? item)
      (traverse
       item
       doing:
       (lambda (e::Element t::Traversal)
	 (dissolve! e)))))

  (define (substitute variables #;with values #;in expression)
    (match expression
      (`(quote ,_)
       expression)
      (`(lambda ,args ,body)
       (let-values (((variables* values*) (only. (isnt _ in. args)
						 variables values))
		    ((lambda*) (car expression)))
	 (recons* lambda* args
		    (substitute variables* #;with values*
				 #;in body))))
      (`(,operator . ,operands)
       `(,(substitute variables #;with values #;in operator)
	 . ,(substitute variables #;with values #;in operands)))
      (_
       (if (symbol? expression)
	   (let* ((result (counterpart #;of expression #;from variables
					    #;in values)))
	     result)
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
  
  (define (reduce-operands operands)
    (match operands
      (`(,first . ,rest)
       (let ((first* (reduce first)))
	 (if (equal? first first*)
	     (let ((result (cons first (reduce-operands rest))))
	       (mark-origin! result operands)
	       result)
	     (let ((result (cons first* rest)))
	       (mark-origin! result operands)
	       (mark-origin! first* first)
	       result))))
      ('()
       operands)
      (_
       (reduce operands))))

  (define (reduce expression)
    (match expression
      (`(if #f ,then ,else)
       (dissolve! expression)
       (mark-origin! else expression)
       else)
      (`(if ,test ,then ,else)
       (let ((test* (reduce test))
	     (if* (car expression)))
	 (cond ((equal? test test*)
		(dissolve! expression)
		(mark-origin! then expression)
		then)
	       (else
		(let ((result (cons* if* test* then else '())))
		  ;; trzeba tez prxekopiowac cell-display-properties
		  (mark-origin! result expression)
		  (mark-origin! test* test)
		  result)))))
      (`(lambda ,args ,body)
       expression)
      (`(quote ,_)
       expression)
      (`(,operator . ,operands)
       (if (and (symbol? operator)
		(context:defines-macro? operator))
	   (error "Macros not supported (yet)")
	   (let ((operands* (reduce-operands operands)))
	     (if (isnt operands equal? operands*)
		 (let ((result (cons operator operands*)))
		   (mark-origin! operands* operands)
		   (mark-origin! result expression)
		   result)
		 (match operator
		   (,@symbol?
		    (cond ((context:primitive? operator)
			   (let ((result (apply (context:value operator)
						operands)))
			     (dissolve! expression)
			     (mark-origin! result expression)
			     result))
			  ((context:defines? operator)
			   (reduce (cons (context:value operator)
					 operands)))
			  (else
			   expression)))
		   (`(lambda ,args ,body)
		    (substitute args #;with operands
				#;in body))
		   (`(,_ . ,_)
		    (let* ((operator* (reduce operator))
			   (result (cons operator* operands)))
		      (mark-origin! result expression)
		      (mark-origin! operator* operator)
		      result))
		   (_
		    expression))))))
      (_
       (if (and (symbol? expression)
		(context:defines? expression))
	   (let ((result (context:value expression)))
	     (dissolve! expression)
	     (mark-origin! result expression)
	     result)
	   expression))))

  (reduce expression))


(define (in. element collection)
  (any. (is _ equal? element) collection))

