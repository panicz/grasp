(import (define-syntax-rule))
(import (assert))
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
(import (traversal))
(import (primitive))
(import (space))
(import (cursor))
(import (document-operations))
(import (editor-operations))
(import (print))
(import (parse))
(import (string-building))
(import (text))

(define-interface Edit ()
  (apply!)::Cursor
  (inverse)::Edit
  )

(define-type (NoEdit)
  implementing Edit
  with
  ((apply!)::Cursor
   (the-cursor))
  ((inverse)::Edit
   (NoEdit)))

(define-type (Move from: Cursor
		   to: Cursor
		   in: pair := (the-document)
		   with-shift: int := 0)
  implementing Edit
  with
  ((apply!)::Cursor
   (let ((item (extract! at: from from: in)))
     (insert! item into: in at: to)
     (cursor-climb-back to in)))

  ((inverse)::Edit
   (match (this)
     ((Move from: `(,s0 . ,source)
            to: `(,d0 ,d1 . ,destination)
	    in: document
	    with-shift: s)
      (Move from: (recons (+ d1 1) destination)
            to: (recons* s (- s0 1) source)
	    in: document
	    with-shift: d0))))
  )

(define-type (Remove element: (either pair
				      HeadTailSeparator
				      EmptyListProxy)
		     at: Cursor := (the-cursor)
		     from: pair := (the-document)
		     with-shift: int := 0)
  implementing Edit
  with
  ((apply!)::Cursor
   (let ((item (extract! at: at from: from)))
     (assert (eq? item element))
     (recons* with-shift (- (car at) 1) (cdr at))))
  ((inverse)::Edit
   (match at
     (`(,tip . ,root)
      (Insert element: element
	      at: (recons* with-shift (- tip 1) root)
	      into: from))))
  )

(define-type (Insert element: (either pair HeadTailSeparator)
		     at: Cursor := (the-cursor)
		     into: pair := (the-document))
  implementing Edit
  with
  ((apply!)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (last-element (last element)))
     (insert! element into: into at: at)
     (let* ((base-cursor (cursor-climb-back (recons (+ top 1) root)
					    into)))
       (if (or (gnu.lists.LList? last-element)
	       (Text? last-element))
	   (cursor-retreat base-cursor into)
	   base-cursor))))
  ((inverse)::Edit
   (match at
     (`(,tip ,top . ,root)
      (Remove element: element
	      at: (recons (+ top 1) root)
	      from: into
	      with-shift: tip)))))

(define-type (ResizeBox at: Cursor := (the-cursor)
			from: Extent
			to: Extent
			in: pair := (the-document)
			with-anchor: real)
  implementing Edit
  with
  ((apply!)::Cursor
   (let* ((box (the-expression at: at in: in))
	  (ending (line-ending-embracing with-anchor box)))
     (resize! box to:width to:height ending)
     (the-cursor)))
      
  ((inverse)::Edit
   (ResizeBox at: at
	      from: to
	      to: from
	      in: in
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
			      after: Cursor := (the-cursor)
			      into: pair := (the-document))
  implementing Edit
  with
  ((apply!)::Cursor
   (let ((target ::Textual (cursor-ref into after))
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
				    (cdr after))
		    from: into)))

(define-type (RemoveCharacter list: (list-of char)
			      before: Cursor := (the-cursor)
			      from: pair := (the-document))
  implementing Edit
  with
  ((apply!)::Cursor
   (let* ((n ::int (length list))
	  (target ::Textual (cursor-ref from before))
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
				   (cdr before))
		    into: from)))

(define-type (SplitElement with: Space
			   at: Cursor := (the-cursor)
			   in: pair := (the-document))
  implementing Edit
  with
  ((apply!)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (parent ::cons (cursor-ref in root))
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
     (and-let* ((`(,_ . ,cursor) (cursor-advance at in)))
       (recons (with:last-index) cursor))))
     
  ((inverse)::Edit
   (MergeElements removing: with
		  at: (and-let* ((`(,tip ,top . ,root) at))
			(recons* (with:first-index)
				 (+ top 1)
				 root)))))
			       

(define-type (MergeElements removing: Space
			    at: Cursor := (the-cursor)
			    in: pair := (the-document))
  implementing Edit
  with
  ((preceding-cursor)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (parent ::Indexable (cursor-ref in root))
	      (space ::Space (parent:part-at top))
	      (target ::Space (space:part-at tip)))
     (cursor-retreat (recons* (space:first-index) top root) in)))
   
  ((apply!)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (parent ::cons (cursor-ref in root))
	      (space ::Space (parent:part-at top))
	      (target ::Space (space:part-at tip))
	      (`(,left ,right . ,_) parent)
	      (left ::Textual left)
	      (right ::Textual right)
	      (cursor (preceding-cursor))
	      ((left:merge! right)))
     ;;(assert (eq? removing space target))
     (set! (post-head-space parent)
	   (post-head-space (cdr parent)))
     (set! (cdr parent) (cdr (cdr parent)))
     cursor))
  
  ((inverse)::Edit
   (SplitElement at: (preceding-cursor)
		 in: in
		 with: removing)))

(define-object (History document::pair)::StringBuilding
  (define fronts ::(list-of (list-of Edit)) '())

  (define undo-step ::int 0)
  
  (define (buildString out::StringBuilder)::StringBuilder
    (with-eval-access
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
	       (`(,last-action . ,_) (drop undo-step timeline))
	       (operation ::Edit last-action)
	       (inverse ::Edit (operation:inverse))
	       (cursor (inverse:apply!)))
      (set! (the-cursor) cursor)
      (set! (the-selection-anchor) cursor)
      (set! undo-step (+ undo-step 1))))

  (define (redo!)::void
    (and-let* (((is undo-step > 0))
	       (`(,timeline . ,_) fronts)
	       (`(,undone-action . ,_) (drop (- undo-step 1)
					     timeline))
	       (operation ::Edit undone-action)
	       (cursor (operation:apply!)))
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
				   from: document
				   with-shift: n) . ,_) . ,_) fronts)
		      ((Insert element: e*
			       at: target
			       into: ,document) operation)
		      ((eq? e e*)))
	     (Move from: source
		   to: target
		   with-shift: n)) =>
		   (lambda (operation::Move)
		     (if (equal? operation:from operation:to)
			 (set! (car fronts) (cdr (car fronts)))
			 (set! (car (car fronts))
			       operation))))
	  ((and-let* ((`((,(Insert element: `(,atom)
				   at: `(,t ,n . ,root)
				   into: document) . ,_)
			 . ,_) fronts)
		      (atom ::Atom atom)
		      (l (atom:text-length))
		      ((InsertCharacter list: `(,c)
					after: `(,,l ,,(+ n 1)
						     . ,,root)
					into: ,document)
		       operation))
	     ;; we're not recording anything, because
	     ;; atom is shared between history and the document,
	     ;; so that the change made to the document
	     ;; will affect history
	     ))

	  ((and-let* ((`((,(InsertCharacter list: chars
					    after: `(,n . ,root)
					    into: document) . ,_)
			 . ,_) fronts)
		      (l (length chars))
		      ((InsertCharacter list: new
					after: `(,,(+ n l)
						 . ,,root)
					into: ,document) operation)
		      ((or (and (every char-whitespace? chars)
				(every char-whitespace? new))
			   (and (every (isnt _ char-whitespace?) chars)
				(every (isnt _ char-whitespace?)
				       new)))))
	     (append! chars new)))

	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((RemoveCharacter list: chars
					before: `(,n . ,root)
					from: document)
		       last-operation)
		      (last-operation ::RemoveCharacter last-operation)
		      (l (length chars))
		      ((RemoveCharacter list: new
					before: `(,,(- n 1)
						  . ,,root)
					from: ,document) operation)
		      ((or (and (every char-whitespace? chars)
				(every char-whitespace? new))
			   (and (every (isnt _ char-whitespace?) chars)
				(every (isnt _ char-whitespace?)
				       new)))))
	     (set! last-operation:list (append! new chars))))
	  
	  #;((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((Insert element: atom
			       at: `(,a0 . ,a*)) last-operation)
		      ((Insert element: e2
			       at: `(,b0 . ,,a*)) operation)
		      
		      ...)
	     (append! e1 e2)
	     (append! e2 e1)))
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

(define/kw (remove-element! at: cursor::Cursor
			    from: document := (the-document))
  ::Remove
  (let* ((shift (last-index (space-preceding cursor in: document)))
	 (element (extract! at: cursor from: document))
	 (history ::History (history document))
	 (action ::Remove (Remove element: element
				  from: cursor
				  with-shift: shift)))
    (history:record! action)
    action))
