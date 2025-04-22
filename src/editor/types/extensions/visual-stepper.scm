(module-name (editor types extensions visual-stepper))

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
(import (utils functions))
(import (language examples))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor interfaces painting))
(import (editor types spaces))
(import (editor types texts))
(import (editor input evaluation))
(import (editor types comments))

(import (editor types extensions extensions))
(import (editor document parse))
(import (language mapping))
(import (editor types extensions combinators))
(import (utils print))
(import (editor types extensions widgets))
(import (utils hash-table))
(import (editor text-painter))

(define text-painter ::Painter (TextPainter))

(define (draw-tween! expression::Element
		     counterparts::(maps (Element)
					 to: (list-of
					      Element))
		     source-position::(maps (Element)
					    to: Position)
		     target-position::(maps (Element)
					    to: Position)
		     intensity::float
		     #!key (only-with-relatives ::boolean #f))
  ::void
  (let ((links (counterparts expression)))
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
	   (draw-tween! sub counterparts
			       source-position
			       target-position
			       intensity
			       only-with-relatives:
			       only-with-relatives)))))
     (else
      (for x in links
	(draw-morph! expression x counterparts
		     source-position
		     target-position
		     intensity
		     only-with-relatives: only-with-relatives))))))

(define (from-primary/secondary-table primary-table
				      primary-key
				      secondary-table
				      secondary-key)
  (if (or (is primary-table overridden-at? primary-key)
	  (isnt secondary-table overridden-at? secondary-key))
      (primary-table primary-key)
      (secondary-table secondary-key)))

(define (draw-morph! foreground::Element
		     background::Element
		     counterparts::(maps (Element)
					 to: (list-of
					      Element))
		     source-position::(maps (Element)
					    to: Position)
		     target-position::(maps (Element)
					    to: Position)
		     progress::float
		     #!key (only-with-relatives ::boolean #f))
  ::void	     
  (let* ((p0 ::Position (from-primary/secondary-table
			 source-position foreground
			 target-position background))
	 (p1 ::Position (from-primary/secondary-table
			 target-position background
			 source-position foreground))
	 (left ::real (linear-interpolation
		       from: p0:left to: p1:left
		       at: (- 1 progress)))
	 (top ::real (linear-interpolation
		      from: p0:top to: p1:top
		      at: (- 1 progress))))
    (cond
     ((match/equal? foreground background)
      ;; here we just draw the foreground
      ;; with full intensity
      (unless (and only-with-relatives
		   (eq? foreground background))
	(with-translation (left top)
	  (draw! foreground))))

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
      (let* ((e0 ::Extent (extent+ foreground))
	     (e1 ::Extent (extent+ background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: (- 1 progress)))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: (- 1 progress))))
	(unless only-with-relatives
	  (with-translation (left top)
	    (painter:draw-box! width height '())))
	(traverse
	 foreground
	 doing:
	 (lambda (item::Element t::Traversal)
	   (draw-tween! item
			  counterparts
			  source-position
			  target-position
			  progress
			  only-with-relatives: only-with-relatives)))))
     ((and (Tile? foreground)
	   (Tile? background))
      (let* ((e0 ::Extent (extent+ foreground))
	     (e1 ::Extent (extent+ background))
	     (width ::real (linear-interpolation
			    from: e0:width to: e1:width
			    at: (- 1 progress)))
	     (height ::real (linear-interpolation
			     from: e0:height to: e1:height
			     at: (- 1 progress))))
	  (with-translation (left top)
	    (painter:with-intensity (- 1.0 progress)
	      (lambda ()
		(painter:with-stretch (/ width e1:width) (/ height e1:height)
		  (lambda ()
		    (draw! background)))))
	    (painter:with-intensity progress
	      (lambda ()
		(painter:with-stretch (/ width e0:width) (/ height e0:height)
		  (lambda ()
		    (draw! foreground)))))))
      (when (gnu.lists.LList? foreground)
	(traverse foreground
		  doing:
		  (lambda (element::Element traverse::Traversal)
		    (draw-tween! element counterparts
				   source-position
				   target-position
				   progress
				   only-with-relatives: #t)))))
     )))

(define (draw-emerging! expression::Element p::Position
			intensity::float)
  ::void
  (painter:with-intensity intensity
    (lambda ()
      (with-translation (p:left p:top)
	(if (gnu.lists.LList? expression)
	    (let ((outer ::Extent (extent+ expression)))
	      (painter:draw-box! outer:width outer:height '()))
	    (draw! expression))))))

(define/kw (measure-positions!
	    expression
	    left::real := 0
	    top::real := 0
	    at: level::int := 0
	    into:
	    measurements::(!maps (Element) to: Position)
	    := (attribute+ (element ::Element)::Position
			  (Position left: 0 top: 0)))
  ::(maps (Element) to: Position)
  (let* ((p ::Position (measurements expression))
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
	     (when #t ;;(gnu.lists.LList? item)
	       (measure-positions! item
				   p:left
				   p:top
				   at: (+ level 1)
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
    (extent+ initial))

  (define final-position ::(maps (Element) to: Position)
    (measure-positions! final))

  (define final-extent ::Extent
    (extent+ final))

  (define maximum-extent ::Extent
    (Extent width: (max initial-extent:width
			final-extent:width)
	    height: (max initial-extent:height
			 final-extent:height)))

  (define (extent) ::Extent maximum-extent)

  (define shift ::(maps (Element) to: Position)
    (attribute+ (element::Element)::Position
	       (Position left: 0 top: 0)))

  (define (draw! context::Cursor)::void
    (cond ((is progress <= 0.5)
	   (draw-tween! final origin
			final-position
			initial-position
			progress)
	   (draw-tween! initial
			progeny
			initial-position
			final-position
			(- 1.0 progress)))
	  (else
	   (draw-tween! initial progeny
			initial-position
			final-position
			(- 1.0 progress))
	   (draw-tween! final
			origin
			final-position
			initial-position
			progress))))
  (Magic))

(define (reduce expression
		#!optional
		(origin::(!maps (Element) to: (list-of Element))
			 (attribute (e)::(list-of Element)
			   (recons e '())))
		(progeny::(!maps (Element) to: (list-of Element))
			  (attribute (e)::(list-of Element)
			    (recons e '())))
		#!key
		(context::EvaluationContext (default-context)))
  
  (define (mark-origin! newborn parent)
    (set! (origin newborn) (recons parent '()))
    (set! (progeny parent) (recons newborn '())))

  (define (add-origin! newborn parent)
    (and-let* ((`(,default) (origin newborn))
	       ((eq? newborn default)))
      (set! (origin newborn) '()))
    (and-let* ((`(,default) (progeny parent))
	       ((eq? parent default)))
      (set! (progeny parent) '()))
    (unless (any (is _ eq? parent) (origin newborn))
      (set! (origin newborn) (cons parent (origin newborn))))
    (unless (any (is _ eq? newborn) (progeny parent))
      (set! (progeny parent) (cons newborn (progeny parent)))))
  
  (define (dissolve! item #!key (when? ::predicate
				       (lambda (item)
					 (and-let* ((`(,i) (progeny item))
						    ((eq? i item)))))))
    (when (when? item)
      (for child in (progeny item)
	(set! (origin child) (only (isnt _ eq? item) (origin child))))
      (set! (progeny item) '()))
    
    (when (gnu.lists.LList? item)
      (traverse
       item
       doing:
       (lambda (e::Element t::Traversal)
	 (dissolve! e when?: when?)))))

  (define (eradicate! item #!key (when? ::predicate
					(lambda (item)
					  (and-let* ((`(,i) (origin item))
						     ((eq? i item)))))))
    (when (when? item)
      (for child in (origin item)
	(set! (progeny child) (only (isnt _ eq? item) (progeny child))))     
      (set! (origin item) '()))
    
    (when (gnu.lists.LList? item)
      (traverse
       item
       doing:
       (lambda (e::Element t::Traversal)
	 (eradicate! e when?: when?)))))
  
  (define (substitute variables #;with values #;in expression)
    (match expression
      (`(quote ,_)
       expression)
      (`(lambda ,args ,body)
       (let*-values (((variables* values*) (only.
					    (lambda (var value)
					      (isnt var in. args))
					    variables values))
		     ((result) (cons* (car expression) args
				      (substitute variables*
						  #;with values*
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
	   (counterpart #;of expression #;from variables
					    #;in values)
	   expression))))

  (define (counterpart #;of variable #;from variables
			    #;in values)
    (match variables
      (`(,,variable . ,_)
       (let* ((result (deep-copy (car values)))
	      (result (if (self-evaluating? result)
			  result
			  (cons (Atom "quote") result))))
	 (eradicate! result when?: always)
	 (add-origin! result (car variables))
	 result))
      (,variable
       (let ((result (cons (Atom "quote") (copy values))))
	 (add-origin! result variable)
	 result))
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
	     (let ((result (cons first (reduce-operands rest))))
	       (mark-origin! result operands)
	       (copy-properties cell-display-properties
				operands result))
	     (let ((result (cons first* rest)))
	       (mark-origin! result operands)
	       (copy-properties cell-display-properties
				operands result)))))
      (`()
       operands)
      (_
       (reduce operands))))

  (define (deep-copy expression)
    (match expression
      (`(,h . ,t)
       (let ((result (cons (deep-copy h) (deep-copy t))))
	 (mark-origin! result expression)
	 (copy-properties cell-display-properties expression result)
	 result))
      (_
       (let ((result (copy expression)))
	 (mark-origin! result expression)
	 result))))

  (define (transfer-heritage! args vals)
    (match args
      (`(,arg . ,args*)
       (let ((val (car vals))
	     (vals* (cdr vals))
	     (children (progeny arg)))
	 (set! (progeny val) children)
	 (for p in children
	   (set! (car (origin p)) val))
	 (transfer-heritage! args* vals*)))
      ('()
       (values))
      (arg
       (let ((children (progeny arg)))
	 (set! (progeny vals) children)
	 (for p in children
	   (set! (car (origin p)) vals))))))
	 
  (define (reduce expression)
    (match expression
      (`(if #f ,then ,else)
       (dissolve! expression)
       (let ((result (deep-copy else)))
	 (mark-origin! result else)
	 result))
      (`(if ,test ,then ,else)
       (let ((test* (reduce test))
	     (if* (car expression)))
	 (cond ((match/equal? test test*)
		(dissolve! expression)
		(let ((result (deep-copy then)))
		  (mark-origin! result then)
		  result))
	       (else
		(let ((result (cons* if* test* then else '())))
		  (mark-origin! result expression)
		  (mark-origin! test* test)
		  (copy-properties* cell-display-properties
				    expression result)
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
		   (mark-origin! operator* operator)
		   (mark-origin! operands* operands)
		   (mark-origin! result expression)
		   (copy-properties cell-display-properties
				    expression
				    result))
		 (match operator
		   (atom::Atom
		    (cond
		     ((context:primitive? operator)
		      (let* ((result
			      (apply-primitive
			       (context:value operator)
			       operands)))
			(mark-origin! result expression)
			(and-let* ((`(car (quote (,a . ,b)))
				    expression))
			  (mark-origin! result a))

			(and-let* ((`(cdr (quote (,a . ,b)))
				    expression)
				   (`(quote ,b*) result))
			  (mark-origin! b* b)
			  (mark-origin! (car result)
					(caadr expression)))

			(and-let* ((`(cons ,a ',b) expression)
				   (`(quote (,a* . ,b*)) result))
			  (mark-origin! a* a)
			  (mark-origin! b* b)
			  (mark-origin! (car result)
					(caaddr expression)))
			
			result))
		     ((context:defines? operator)
		      (let ((operator* (context:value operator)))
			(match operator*
			  (`(lambda ,args ,body)
			   (let ((result (substitute
					  args
					  #;with operands
						 #;in body)))
			     (transfer-heritage! args operands)
			     (dissolve! expression)
			     (mark-origin! result operator)
			     result))
			  (_
			   `(,operator* . ,operands)))))
		     (else
		      expression)))
		   (`(lambda ,args ,body)
		    (dissolve! expression)
		    (let ((result (substitute
				   args
				   #;with operands
					  #;in body)))
		      result))
		   (`(,_ . ,_)
		    (let* ((operator* (reduce operator))
			   (result (cons operator* operands)))
		      (mark-origin! result expression)
		      (mark-origin! operator* operator)
		      (copy-properties cell-display-properties
				       expression
				       result)))
		   (_
		    expression))))))
      (_
       (if (and (Atom? expression)
		(context:defines? expression))
	   (let ((result (copy (context:value expression))))
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
	 (attribute (e::Element)::(list-of Element)
	   (recons e '()))
	 (attribute (e::Element)::(list-of Element)
	   (recons e '()))))

(define/memoized (morph-from expression::Tile)::Morph
  (let*-values (((reduced origins progenies) (reduce expression))
		((result) (Morph expression reduced
				 origins progenies)))
    
    (unless (match/equal? expression reduced)
      (set! (morph-to reduced) result))
    result))

(define-object (ExpressionReducer initial-expression::Tile)::Player

  (define (typename)::String "Stepper")

  (define duration/ms ::float 700.0)
  
  (define current-morph ::Morph
    (morph-from initial-expression))

  (define playing-backwards? ::boolean #f)
  
  (define (advance! timestep/ms::int)::boolean
    (safely
     (let ((change (/ timestep/ms duration/ms)))
       (if playing-backwards?
	   (let ((new-progress (- current-morph:progress change)))
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
	       #t)))))))
  
  (define (rewind!)::void
    (set! current-morph (morph-from initial-expression))
    (set! current-morph:progress 0.0))
  
  (define (back!)::void
    (set! playing-backwards? #t)
    (set! now-playing? #f)
    (when (and (is current-morph:progress <= 0.0)
	       (isnt current-morph:initial eq? initial-expression))
      (set! current-morph (morph-to current-morph:initial))
      (set! current-morph:progress 1.0))
    (painter:play! (this)))

  (define (play!)::void
    (set! now-playing? #t)
    (set! playing-backwards? #f)
    (painter:play! (this)))
  
  (define (pause!)::void
    (set! now-playing? #f))
  
  (define (next!)::void    
    (begin
      (set! now-playing? #f)
      (set! playing-backwards? #f)
      (is current-morph:progress >= 1.0)
      (when (is current-morph:progress >= 1.0)
	(set! current-morph (morph-from current-morph:final))
	(set! current-morph:progress 0.0))
      (painter:play! (this))))
      
  (define (fast-forward!)::void
    (WARN "fast-forward not implemented for Stepper")
    (values))

  (define now-playing? ::boolean #f)
  
  (define (playing?)::boolean now-playing?)

  (define (draw! context::Cursor)::void
    (current-morph:draw! context))

  (define (value)::Object
    (cons (Atom "Stepper") (cons initial-expression '())))

  (define max-extent ::Extent
    (extent+ current-morph))
  
  (define (extent)::Extent
    (let ((current ::Extent (extent+ current-morph)))
      (set! max-extent:width (max max-extent:width current:width))
      (set! max-extent:height (max max-extent:height current:height))
      max-extent))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    #!null)
  
  (Magic))

(define-simple-extension (Stepper expression::Tile)
  (PlayerWithControls (ExpressionReducer expression)))

(set! (extension 'Stepper)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (otherwise (begin
		      (WARN "Unable to create Stepper from "source)
		      #!null)
	   (parameterize ((cell-access-mode CellAccessMode:Editing))
	     (and-let* ((`(Stepper ,expression) source))
	       (Stepper expression)))))))
