(import (assert))
(import (infix))
(import (define-object))
(import (define-type))
(import (define-interface))
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
(import (parse))
(import (mapping))
(import (combinators))
(import (print))
(import (button))
(import (hash-table))
(import (text-painter))

(define-alias id java.lang.System:identityHashCode)

(define text-painter ::Painter (TextPainter))

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
  ;;(WARN "drawing morph from "foreground" to "background)
  (let* ((p0 ::Position (source-position foreground))
	 (p1 ::Position (target-position background))
	 (painter ::Painter (the-painter))
	 (left ::real (linear-interpolation
		       from: p0:left to: p1:left
		       at: (- 1 progress)))
	 (top ::real (linear-interpolation
		      from: p0:top to: p1:top
		      at: (- 1 progress))))
    (cond
     ((match/equal? foreground background)
      (WARN "drawing identical "foreground
	    (java.lang.System:identityHashCode foreground)
	    (java.lang.System:identityHashCode background)
	    (counterparts foreground)
	    (map java.lang.System:identityHashCode (counterparts foreground))
	    p0 p1 left top)
      ;; here we just draw the foreground
      ;; with full intensity
      (with-translation (left top)
	(draw! foreground)))

     ((or (isnt foreground Tile?)
	  (isnt background Tile?))
      (WARN "drawing spaces "foreground" and "background)
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
      (WARN "drawing listst "foreground" and "background)
      (let* ((e0 ::Extent (extent foreground))
	     (e1 ::Extent (extent background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: (- 1 progress)))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: (- 1 progress))))
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
      (WARN "drawing tiles "foreground" and "background)
      (when (and (pair? foreground) (pair? (cdr foreground))
		 (equal? (car foreground) 'pred))
	(WARN (java.lang.System:identityHashCode (cadr foreground))
	      (counterparts (cadr foreground))
	      (map java.lang.System:identityHashCode (counterparts
						      (cadr foreground)))))
	  
      
      (let* ((e0 ::Extent (extent foreground))
	     (e1 ::Extent (extent background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: (- 1 progress)))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: (- 1 progress))))
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
	       (lambda ()
		 (draw! foreground))))))))
     )))

(define (draw-emerging! expression::Element p::Position
			intensity::float)
  ::void
  (let ((painter ::Painter (the-painter)))
    (painter:with-intensity
     intensity
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
  (let* ((p ::Position (measurements expression))
	 (painter ::Painter (the-painter))
	 (paren-width ::real (painter:paren-width)))
    (set! p:left left)
    (set! p:top top)
    (if (gnu.lists.LList? expression)
	(traverse
	 expression
	 doing:
	 (lambda (item::Element t::Traversal)
	   (let ((p ::Position (measurements item)))
	     (set! p:left (+ t:left left paren-width))
	     (set! p:top (+ t:top top))
	     (when (gnu.lists.LList? item)
	       (measure-positions! item
				   (+ p:left paren-width)
				   p:top
				   into: measurements))))
	 returning:
	 (lambda (t::Traversal)
	   measurements))
	measurements)))

(define-object (Morph initial::Tile
		      final::Tile
		      origin::(maps (Element) to: (list-of
						   Element))
		      progeny::(maps (Element) to: (list-of
						    Element)))
  ::Enchanted
  (define progress ::float 0.0)

  (define initial-position ::(maps (Element) to: Position)
    (measure-positions! initial))

  (define initial-extent ::Extent
    (initial:extent))

  (define final-position ::(maps (Element) to: Position)
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
       (if (Atom? x)
	   (isnt (x:value) symbol?)
	   #t)))

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
    (WARN "LOOKING FOR "atom)
    (definitions:contains-key atom:name))

  (define (define! atom::Atom value)
    (definitions:put atom:name value))

  (define (primitive? atom::Atom)
    (and (definitions:contains-key atom:name)
	 (let ((value (definitions:get atom:name)))
	   (procedure? value))))
  )

(define default-context ::EvaluationContext
  (EvaluationContext))

(default-context:define! (Atom "!")
  (car (parse-string "\
(lambda (n)
  (if (<= n 1)
     1 #| BASE CASE |#
     (* n (! (- n 1)))))")))

(default-context:define! (Atom "append")
  (car (parse-string "\
(lambda (a b)
  (if (null? a)
     b
     (cons (car a) (append (cdr a) b))))")))

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

(define (reduce expression
		#!optional
		(origin::(!maps (Element) to: (list-of Element))
			 (property (e::Element)::(list-of Element)
				   (recons e '())))
		(progeny::(!maps (Element) to: (list-of Element))
			  (property (e::Element)::(list-of Element)
				    (recons e '())))
		#!key
		(context::EvaluationContext default-context))
  
  (define (mark-origin! newborn parent)
    (WARN "marking the origin of "newborn" as "parent)
    (set! (origin newborn) (recons parent '()))
    (set! (progeny parent) (recons newborn '())))

  (define (add-origin! newborn parent)
    (and-let* ((`(,default) (origin newborn))
	       ((eq? newborn default)))
      (set! (origin newborn) '()))
    (and-let* ((`(,default) (progeny parent))
	       ((eq? parent default)))
      (set! (progeny parent) '()))
    (set! (origin newborn) (recons parent (origin newborn)))
    (set! (progeny parent) (cons newborn (progeny parent))))
  
  (define (dissolve! item)
    (WARN "dissolving "item)
    (set! (progeny item) '())
    (when (gnu.lists.LList? item)
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
       (let*-values (((variables* values*) (only. (isnt _ in. args)
						  variables values))
		     ((lambda*) (car expression))
		     ((result) (cons* lambda* args
				      (substitute variables* #;with values*
						  #;in body))))
	 (copy-properties cell-display-properties
			  (cdr expression) (cdr result))
	 (copy-properties cell-display-properties
			  expression result)
	 result))
      (`(,operator . ,operands)
       (let ((result (cons (substitute variables #;with values
				       #;in operator)
			   (substitute variables #;with values
				       #;in operands))))
	 (mark-origin! result expression)
	 (copy-properties cell-display-properties expression
			  result)))
      (_
       (if (Atom? expression)
	   (let* ((result (counterpart #;of expression #;from variables
					    #;in values)))
	     (let ((result (copy result)))
	       (add-origin! result expression)
	       (WARN "ORIGIN of "result (id result)": "(origin result)
		     (map id (origin result)))
	       result))
	   expression
	   #;(let ((result (copy expression)))
	       (add-origin! result expression)
	       result)))))

  (define (counterpart #;of variable #;from variables
			    #;in values)
    (match variables
      (`(,,variable . ,_)
       (let ((result (car values)))
	 (if (self-evaluating? result)
	     result
	     (cons (Atom "quote") result))))
      (,variable
       (cons (Atom "quote") values))
      (`(,_ . ,rest)
       (counterpart #;of variable #;from rest
			 #;in (cdr values)))
      (_
       variable)))
  
  (define (reduce-operands operands)
    (match operands
      (`(,first . ,rest)
       (let ((first* (reduce first)))
	 (if (match/equal? first first*)
	     (let* (#;(first+ (if (eq? first first*)
				(copy first)
				first*))
		    (result (cons first (reduce-operands rest))))
	       (WARN "by remaining operand reduction")
	       (mark-origin! result operands)
	       (copy-properties cell-display-properties operands result))
	     (let ((result (cons first* rest)))
	       (WARN "by first operand reduction")
	       (mark-origin! result operands)
	       (mark-origin! first* first)
	       (copy-properties cell-display-properties operands result)))))
      (`()
       operands)
      (_
       (reduce operands))))

  (define (reduce expression)
    (match expression
      (`(if #f ,then ,else)
       (WARN "by if-false")
       (dissolve! expression)
       (let ((result (substitute '() '() else)))
	 (mark-origin! result else)
	 result))
      (`(if ,test ,then ,else)
       (let ((test* (reduce test))
	     (if* (car expression)))
	 (cond ((match/equal? test test*)
		(WARN "by if-true")
		(dissolve! expression)
		(let ((result (substitute '() '() then)))
		  (mark-origin! result then)
		  result))
	       (else
		(let ((result (cons* if* test* then else '())))
		  (WARN "by if-test")
		  (mark-origin! result expression)
		  (mark-origin! test* test)
		  (copy-properties* cell-display-properties expression result)
		  result)))))
      (`(lambda ,args ,body)
       expression)
      (`(quote ,_)
       expression)
      (`(,operator . ,operands)
       (if (and (Atom? operator)
		(context:defines-macro? operator))
	   (error "Macros not supported (yet)")
	   (let ((operands* (reduce-operands operands)))
	     (if (isnt operands match/equal? operands*)
		 (let* ((operator* (copy operator))
			(result (cons operator* operands*)))
		   (WARN "by operand reduction: "operands
			 " are not equal to "operands*)
		   (mark-origin! operator* operator)
		   (mark-origin! operands* operands)
		   (mark-origin! result expression)
		   (copy-properties cell-display-properties expression
				    result))
		 (match operator
		   (,@Atom?		    
		    (cond ((context:primitive? operator)
			   (let* ((result
				   (grasp
				    (parameterize ((cell-access-mode
						    CellAccessMode:Evaluating))
				      (apply (context:value operator)
					     (map (lambda (x) x) operands))))))
			     (WARN "by apply")
			     (mark-origin! result expression)
			     result))
			  ((context:defines? operator)
			   (let ((result (reduce (cons (context:value operator)
						       operands))))
			     (WARN "by beta")
			     (dissolve! expression)
			     (and-let* ((`(,,expression) (origin expression)))
			       (WARN "siabadaba")
			       (mark-origin! result operator))
			     result))
			  (else
			   expression)))
		   (`(lambda ,args ,body)
		    (substitute args #;with operands
				#;in body))
		   (`(,_ . ,_)
		    (let* ((operator* (reduce operator))
			   (result (cons operator* operands)))
		      (WARN "by operator reduction")
		      (mark-origin! result expression)
		      (mark-origin! operator* operator)
		      (copy-properties cell-display-properties expression
				       result)))
		   (_
		    expression))))))
      (_
       (if (and (Atom? expression)
		(context:defines? expression))
	   (let ((result (copy (context:value expression))))
	     (WARN "by dereference of "expression" as "result)
	     (dissolve! expression)
	     (mark-origin! result expression)
	     result)
	   expression))))

  (values (reduce expression)
	  origin
	  progeny))

(define (in. element collection)
  (any. (is _ match/equal? element) collection))

(define-mapping (morph-to expression::Tile)::Morph
  (Morph expression expression
	 (property (e::Element)::(list-of Element)
		   (recons e '()))
	 (property (e::Element)::(list-of Element)
				    (recons e '()))))

(define/memoized (morph-from expression::Tile)::Morph
  (let*-values (((reduced origins progenies) (reduce expression))
		((result) (Morph expression reduced origins progenies)))
    
    (parameterize ((the-painter text-painter))
      (text-painter:clear!)
      (WARN "vvvvvv")
      (draw! expression)
      (WARN (text-painter:toString))
      (text-painter:clear!)
      (WARN "======")
      (draw! reduced)
      (WARN (text-painter:toString))
      (WARN "^^^^^^"))

    (set! (morph-to reduced) result)
    result))

(define-interface Playable ()
  (rewind!)::void
  (back!)::void
  (play!)::void
  (pause!)::void
  (next!)::void
  (fast-forward!)::void
  (playing?)::boolean)

(define-interface Player (Enchanted Playable Animation))


(define-object (Stepper initial-expression::Tile)::Player

  (define (typename)::String "Stepper")

  (define duration/ms ::float 700.0)
  
  (define current-morph ::Morph
    (morph-from initial-expression))

  (define playing-backwards? ::boolean #f)
  
  (define (advance! timestep/ms::int)::boolean
    (let ((change (/ timestep/ms duration/ms)))
      (WARN "advance by "change)
      (if playing-backwards?
	  (let ((new-progress (- current-morph:progress change)))
	    (WARN "new progress "new-progress)
	    (cond
	     ((is new-progress <= 0.0)
	      (let ((initial current-morph:initial))
		(set! current-morph (morph-to initial))
		(set! current-morph:progress 1.0)
		(when (match/equal? initial current-morph:initial)
		  (set! now-playing? #f)))
	      now-playing?)
	     (else
	      (set! current-morph:progress new-progress)
	      #t)))
	  (let ((new-progress (+ current-morph:progress change)))
	    (WARN "new progress "new-progress)
	    (cond
	     ((is new-progress >= 1.0)
	      (let ((final current-morph:final))
		(set! current-morph (morph-from final))
		(set! current-morph:progress 0.0)
		(when (match/equal? final current-morph:final)
		  (set! now-playing? #f)))
	      now-playing?)
	     (else
	      (set! current-morph:progress new-progress)
	      #t))))))
  
  (define (rewind!)::void
    (WARN "rewind!")
    #;(set! current-morph (morph-from initial-expression)))
  
  (define (back!)::void
    (WARN "back!")
    (set! playing-backwards? #t)
    (set! now-playing? #f)
    (let ((painter ::Painter (the-painter)))
      (painter:play! (this))))

  (define (play!)::void
    (WARN "play!")
    (set! now-playing? #t)
    (set! playing-backwards? #f)
    (let ((painter ::Painter (the-painter)))
      (painter:play! (this))))
  
  (define (pause!)::void
    (set! now-playing? #f))
  
  (define (next!)::void
    (WARN "next!")
    ;;(set! current-morph:origin (lambda _ '()))
    ;;(set! current-morph:progeny (lambda _ '()))
    ;;(WARN "origin: "(procedure-property current-morph:origin 'table))
    ;;(WARN "progeny: "(procedure-property current-morph:progeny 'table))
    (WARN (current-morph:progeny current-morph:initial))
    
    (set! current-morph:progress (+ current-morph:progress 0.2))
    (WARN current-morph:progress)
    (when (is current-morph:progress > 1.0)
      (let ((final current-morph:final))
	(set! current-morph (morph-from final))
	(set! current-morph:progress 0.0)

	))
    
    #|
    (set! playing-backwards? #f)
    (set! now-playing? #f)
    (let ((painter ::Painter (the-painter)))
      (painter:play! (this)))
|#
    )
      
  (define (fast-forward!)::void
    (WARN "fast-forward!")
    ;; to zasadniczo nie wiemy, jak zaimplementowac
    (values))

  (define now-playing? ::boolean #f)
  
  (define (playing?)::boolean now-playing?)

  (define (draw! context::Cursor)::void
    (current-morph:draw! context))

  (define (as-expression)::cons
    (cons (Atom "Stepper") (cons initial-expression '())))

  (define max-extent ::Extent
    (current-morph:extent))
  
  (define (extent)::Extent
    (let ((current ::Extent (current-morph:extent)))
      (set! max-extent:width (max max-extent:width current:width))
      (set! max-extent:height (max max-extent:height current:height))
      max-extent))
      

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    #!null)
  
  (Magic))

(define (PlayerWithControls player::Player)::Enchanted
  (Below
   top: player
   bottom:
   (Beside
    left:
    (Beside left: (Button label: "▮◀◀"
			  action: (lambda () (player:rewind!)))
	    right: (Button label: "▮◀ "
			   action: (lambda () (player:back!))))
    right:
    (Beside
     left: (Button label: " ▶ "
		   action: (lambda () (player:play!)))
     right:
     (Beside left: (Button label: " ▶▮"
			   action: (lambda () (player:next!)))
	     right: (Button label: "▶▶▮"
			    action: (lambda ()
				      (player:fast-forward!))))))))

(set! (extension 'Stepper)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (WARN "invoking stepper")
	 (otherwise (begin
		      (WARN "Unable to create Stepper from "source)
		      #!null)
	   (parameterize ((cell-access-mode CellAccessMode:Editing))
	     (and-let* ((`(Stepper ,expression) source))
	       (PlayerWithControls (Stepper expression))))))))
