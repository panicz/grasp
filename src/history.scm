(import (define-syntax-rule))
(import (assert))
(import (srfi :17))
(import (hash-table))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-parameter))
(import (keyword-arguments))
(import (default-value))
(import (mapping))
(import (fundamental))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (painter))
(import (extent))
(import (indexable))
(import (primitive))
(import (space))
(import (cursor))
(import (document-operations))
(import (editor-operations))
(import (print))
(import (parse))
(import (string-building))
(import (text))
(import (comments))
(import (examples))
(import (extension))

(define-interface Edit ()
  (apply! document::pair)::Cursor
  (inverse)::Edit
  )

(define-type (NoEdit)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (the-cursor))
  ((inverse)::Edit
   (NoEdit)))

(define-type (EditSequence operations:  (list-of Edit))
  implementing Edit
  with
  ((apply! document)::Cursor
   (let ((result ::Cursor #!null))
     (for operation::Edit in operations
       (set! result (operation:apply! document)))
     result))
  ((inverse)::Edit
   (define (transform sequence inverted)
     (match sequence
       (`(,head::Edit . ,tail)
        (transform tail (cons (head:inverse) inverted)))
       ('()
        inverted)))
   (transform operations '()))
  )

(define-type (Move from: Cursor
		   to: Cursor
		   with-shift: int := 0)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (let ((item (extract! at: from from: document)))
     (insert! item into: document at: to)
     (cursor-climb-back to document)))

  ((inverse)::Edit
   (match (this)
     ((Move from: `(,s0 . ,source)
            to: `(,d0 ,d1 . ,destination)
	    with-shift: s)
      (Move from: (recons (+ d1 1) destination)
            to: (recons* s (- s0 1) source)
	    with-shift: d0))))
  )

(define-type (Remove element: (either pair
				      EmptyListProxy)
		     at: Cursor := (the-cursor)
		     with-shift: int := 0)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (let ((item (extract! at: at from: document)))
     (assert (eq? item element))
     (recons* with-shift (- (car at) 1) (cdr at))))
  ((inverse)::Edit
   (match at
     (`(,tip . ,root)
      (Insert element: element
	      at: (recons* with-shift (- tip 1) root)))))
  )

(define-type (Insert element: pair
		     at: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (last-element (last element)))
     (insert! element into: document at: at)
     (let* ((base-cursor (cursor-climb-back (recons (+ top 1) root)
					    document)))
       (if (or (gnu.lists.LList? last-element)
	       (Text? last-element))
	   (cursor-retreat base-cursor document)
	   base-cursor))))
  ((inverse)::Edit
   (match at
     (`(,tip ,top . ,root)
      (Remove element: element
	      at: (recons (+ top 1) root)
	      with-shift: tip)))))

(define-type (ResizeBox at: Cursor := (the-cursor)
			from: Extent
			to: Extent
			with-anchor: real)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (let* ((box (the-expression at: at in: document))
	  (ending (line-ending-embracing with-anchor box)))
     (resize! box to:width to:height ending)
     (the-cursor)))
      
  ((inverse)::Edit
   (ResizeBox at: at
	      from: to
	      to: from
	      with-anchor: with-anchor)))

(define (resize! box::pair
		 width::real
		 height::real
		 ending::LineEnding)::void
  (let* ((painter ::Painter (the-painter))
	 (min-line-height ::real (painter:min-line-height))
	 (space-width ::real (painter:space-width))
	 (paren-width ::real (painter:paren-width))
	 (last-space ::Space (last-space box))
	 (prior ::Extent (extent box)))
    (define (set-width!)
      (traverse
       box doing:
       (lambda (item::Element t::Traversal)
	 (and-let* ((space ::Space item))
	   (for-each-pair (lambda (cell::pair)
			    (and-let* ((`(,,@integer?
					  ,,@integer?
					  . ,_) cell))
			      (set-car! cell 0)))
			  space:fragments))))
      (let* ((break (last-pair-before ending:index
				      ending:space:fragments))
	     (coda ::pair (last-pair last-space:fragments))
	     (new-width (as int (quotient (- width ending:reach
					     paren-width
					     paren-width)
					  space-width))))
	(when (is (car coda) integer?)
	  (set! (car coda) 0))
	(set! (car break) (max 0 new-width))))
    
    (define (set-height!)::void
      (let ((increment (- height prior:height)))
	(if (is increment > 0)
	    (let* ((lines ::int (quotient increment
					  min-line-height)))
	      (set-cdr! ending:space:fragments
			(let ((tip (cdr ending:space:fragments)))
			  (times lines (lambda ()
					 (set! tip (cons 0 tip))))
			  tip)))
	    (let ((lines ::int (quotient (- increment)
					 min-line-height)))
	      (call/cc
	       (lambda (return)
		 (traverse
		  box doing:
		  (lambda (item::Element t::Traversal)
		    (and-let* ((space ::Space item))
		      (let remove-line ((fragments space:fragments))
			(if (is lines <= 0)
			    (return)
			    (match fragments
			      (`(,,@integer? ,,@integer? ,,@integer?
					     . ,_)
			       (set-cdr! fragments (cddr fragments))
			       (set! lines (- lines 1))
			       (remove-line fragments))
			      (`(,,@integer? ,,@integer?)
			       (if (eq? space last-space)
				   (set-cdr! fragments '())
				   (values)))
			      (`(,head . ,tail)
			       (remove-line tail))
			      (_
			       (values))
			      ))))))))))))
    (set-width!)
    (set-height!)))

(define-type (InsertCharacter list: (list-of char)
			      after: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (let ((target ::Textual (cursor-ref document after))
	 (n ::int (car after)))
     (for c in list
       (target:insert-char! c n)
       (set! n (+ n 1))))
   (recons (+ (car after) (length list))
	   (cdr after)))
  ((inverse)::Edit
   (RemoveCharacter list: list
		    before: (recons (+ (car after)
				       (length list))
				    (cdr after)))))

(define-type (RemoveCharacter list: (list-of char)
			      before: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (let* ((n ::int (length list))
	  (target ::Textual (cursor-ref document before))
	  (i (- (car before) n)))
     (assert (is i >= 0))
     (for c in list
       (unless (eq? c (target:char-ref i))
	 (WARN "the removed char "(target:char-ref i)
	       " differs from expected "c))
       (target:delete-char! i)))
   (recons (- (car before)
	      (length list))
	   (cdr before)))
  
  ((inverse)::Edit
   (InsertCharacter list: list
		    after: (recons (- (car before)
				      (length list))
				   (cdr before)))))

(define-type (InsertComment content: TextualComment
			    at: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (grandpa ::Indexable (cursor-ref document root))
	      (item ::Space (grandpa:part-at top)))
     (let-values (((fragments
		    remnant) (space-fragment-index item:fragments
						    tip)))
       (and-let* ((`(,fragment::integer . ,fragments*) fragments))
	 (set! (car fragments) remnant)
	 (set! (cdr fragments) (cons content fragments*))
	 (recons* (content:first-index) (+ tip 1) top root)))))

  ((inverse)::Edit
   (RemoveComment content: content at: at)))

(define-type (RemoveComment content: TextualComment
			    at: Cursor)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (grandpa ::Indexable (cursor-ref document root))
	      (item ::Space (grandpa:part-at top)))
     (set! item:fragments (content:remove-from! item:fragments)))
   (recons (as int (- (car at) 1)) (cdr at)))
   
  ((inverse)::Edit
   (InsertComment content: content
		  at: (recons (- (car at) 1) (cdr at)))))

(define-type (CommentExpression at: Cursor := (the-cursor)
				with-shift: int)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (`(,expression) (extract! at: (recons top root)
					from: document)))
     (insert! (ExpressionComment expression: expression)
              into: document at: (recons* with-shift (- top 1) root))
     (recons* tip #\; (+ with-shift 1) (- top 1) root)))
  ((inverse)::Edit
   (and-let* ((`(,tip ,top . ,root) at))
     (UncommentExpression at: (recons* tip #\;
				       (+ with-shift 1)
				       (- top 1) root)))))

(define-type (UncommentExpression at: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,cherry #\; ,tip ,top . ,root) at)
	      ((ExpressionComment expression: expression)
               (extract! at: (recons* tip top root) from: document)))
     (insert! (cons expression '())
	      at: (recons* (- tip 1) top root)
	      into: document)
     (recons* cherry (+ top 1) root)))
  ((inverse)::Edit
   (and-let* ((`(,cherry #\; ,tip ,top . ,root) at))
     (CommentExpression at: (recons* cherry (+ top 1) root)
                        with-shift: (- tip 1)))))

(e.g.
 (parameterize ((the-document
		 (string->document "(define (f x y) z)")))
   (let* ((operation ::Edit (CommentExpression at: '(0 5 3 1 1)
					       with-shift: 1))
	  (inverse ::Edit (operation:inverse))
	  (_ (operation:apply! (the-document)))
	  (commented (document->string (the-document)))
	  (_ (inverse:apply! (the-document)))
	  (uncommented (document->string (the-document))))
     (values commented uncommented)))
 ===> "(define (f x #;y) z)" "(define (f x y) z)")
 
(define-type (SplitElement at: Cursor := (the-cursor)
			   with: Space)
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (parent ::cons (cursor-ref document root))
	      (target ::TextualTile (parent:part-at top))
	      (final ::TextualTile (target:part-at tip))
	      ((eq? target final))
	      (suffix ::TextualTile (target:split! tip))
	      (owner (drop (quotient top 2) parent))
	      (cell (cons suffix (cdr owner))))
     (set! (cdr owner) cell)
     (set! (post-head-space cell)
	   (post-head-space owner))
     (set! (post-head-space owner) with)
     (and-let* ((`(,_ . ,cursor) (cursor-advance at document)))
       (recons (with:last-index) cursor))))
     
  ((inverse)::Edit
   (MergeElements removing: with
		  after: at)))

(define-type (MergeElements removing: Space
			    after: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) after)
	      (parent ::cons (drop (quotient top 2)
				   (cursor-ref document root)))
	      (`(,left::Textual ,right::Textual . ,_) parent))
     (assert (eqv? tip (text-length left)))
     (assert (eq? removing (post-head-space parent)))
     (left:merge! right)
     (set! (post-head-space parent)
	   (post-head-space (cdr parent)))
     (set! (cdr parent) (cdr (cdr parent)))
     after))
  
  ((inverse)::Edit
   (SplitElement at: after
		 with: removing)))

(define-type (EnchantExpression at: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (otherwise #!null
     (and-let* ((enchanted ::Element
			  (enchant-expression! at: at
					       in: document)))
       (recons (enchanted:first-index)
	       (cursor-core at document)))))

  ((inverse)::Edit
   (DisenchantExpression at: at)))

(define-type (DisenchantExpression at: Cursor := (the-cursor))
  implementing Edit
  with
  ((apply! document::pair)::Cursor
   (otherwise #!null
     (and-let* ((expression ::Element
			    (disenchant-expression! at: at
						    in: document)))
       (recons (expression:first-index)
	       (cursor-core at document)))))

  ((inverse)::Edit
   (EnchantExpression at: at)))

  
(define-object (History document::pair)::StringBuilding
  (define fronts ::(list-of (list-of Edit)) '())

  (define undo-step ::int 0)
  
  (define (buildString out::StringBuilder)::StringBuilder
    (parameterize ((cell-access-mode CellAccessMode:Evaluating))
      (and-let* ((`(,front . ,_) fronts))
	(let ((n ::int 0))
	  (for operation in front
	    (when (is n = undo-step)
	      (out:append "* "))
	    (out:append (operation:toString))
	    (out:append "\n")
	    (set! n (+ n 1)))))
      out))

  (define (toString)::String
    (let ((builder ::StringBuilder (StringBuilder)))
      (buildString builder)
      (builder:toString)))

  (define (clear!)
    (set! fronts '())
    (set! undo-step 0))
  
  (define (undo!)::void
    (and-let* ((`(,timeline . ,_) fronts)
	       (`(,last-action::Edit . ,_) (drop undo-step timeline))
	       (inverse ::Edit (last-action:inverse))
	       (cursor (inverse:apply! document)))
      (set! (the-cursor) cursor)
      (set! (the-selection-anchor) cursor)
      (set! undo-step (+ undo-step 1))))

  (define (redo!)::void
    (and-let* (((is undo-step > 0))
	       (`(,timeline . ,_) fronts)
	       (`(,undone-action::Edit . ,_) (drop (- undo-step 1)
						   timeline))
	       (cursor (undone-action:apply! document)))
      (set! (the-cursor) cursor)
      (set! (the-selection-anchor) cursor)
      (set! undo-step (- undo-step 1))))

  (define (record! operation ::Edit)::void
    (cond ((null? fronts)
	   (set! fronts (cons (cons operation '()) fronts)))
	  ((is undo-step > 0)
	   (set! fronts (cons (cons operation
				    (drop undo-step (car fronts)))
			      fronts))
	   (set! undo-step 0))
	  ((and-let* ((`((,(Remove element: e 
				   at: source
				   with-shift: n) . ,_) . ,_) fronts)
		      ((Insert element: e*
			       at: target) operation)
		      ((eq? e e*)))
	     (Move from: source
		   to: target
		   with-shift: n)) =>
		   (lambda (operation::Move)
		     (if (equal? operation:from operation:to)
			 (set! (car fronts) (cdr (car fronts)))
			 (set! (car (car fronts))
			       operation))))
	  ((and-let* ((`((,(Insert element: `(,atom::Atom)
				   at: `(,t ,n . ,root)) . ,_)
			 . ,_) fronts)
		      (l (atom:text-length))
		      ((InsertCharacter list: `(,c)
					after: `(,,l ,,(+ n 1)
						     . ,,root))
		       operation))
	     ;; we're not recording anything, because
	     ;; atom is shared between history and the document,
	     ;; so that the change made to the document
	     ;; will affect history
	     ))

	  ((and-let* ((`((,(InsertCharacter list: chars
					    after: `(,n . ,root)) . ,_)
			 . ,_) fronts)
		      (l (length chars))
		      ((InsertCharacter list: new
					after: `(,,(+ n l)
						 . ,,root)) operation)
		      ((or (and (every char-whitespace? chars)
				(every char-whitespace? new))
			   (and (every (isnt _ char-whitespace?) chars)
				(every (isnt _ char-whitespace?)
				       new)))))
	     (append! chars new)))
	  
	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((RemoveCharacter list: chars
					before: `(,n . ,root))
		       last-operation)
		      ((RemoveCharacter list: new
					before: `(,,(- n (length chars))
						  . ,,root)) operation)
		      ((or (and (every char-whitespace? chars)
				(every char-whitespace? new))
			   (and (every (isnt _ char-whitespace?) chars)
				(every (isnt _ char-whitespace?)
				       new)))))
	     (set! last-operation:list (append! new chars))))

	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((RemoveCharacter list: chars
					before: `(,n . ,root))
		       last-operation)
		      (l (length chars))
		      ((RemoveCharacter list: new
					before: `(,,(- n (length chars)
						       -1)
						  . ,,root)) operation)
		      ((or (and (every char-whitespace? chars)
				(every char-whitespace? new))
			   (and (every (isnt _ char-whitespace?) chars)
				(every (isnt _ char-whitespace?)
				       new)))))
	     (set! last-operation:before (recons (+ n (length new))
						 root))
	     (append! chars new)))
	  
	  (else
	   (set-car! fronts (cons operation
				  (car fronts)))))
    ))

(define-property+ (history document::pair)::History
  (History document))

(define/kw (last-operation document::pair := (the-document))::Edit
  (or (and-let* ((history ::History (history document))
		 (`(,front . ,_) history:fronts)
		 (`(,op . ,_) (drop history:undo-step front)))
	op)
      (NoEdit)))

(define/kw (remove-element! at: cursor::Cursor := (the-cursor)
			    from: document := (the-document))
  ::Remove
  (let* ((shift (last-index (space-preceding cursor in: document)))
	 (element (extract! at: cursor from: document))
	 (history ::History (history document))
	 (action ::Remove (Remove element: element
				  at: cursor
				  with-shift: shift)))
    (history:record! action)
    action))
