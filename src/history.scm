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

(define-interface Edit ()
  (apply! document::pair)::void
  (inverse)::Edit
  (cursor document::pair)::Cursor
  )

(define-type (Move from: Cursor
		   to: Cursor
		   with-shift: int := 0)
  implementing Edit
  with
  ((apply! document::pair)::void
   (let ((item (extract! at: from from: document)))
     (insert! item into: document at: to)
     #!null))

  ((inverse)::Edit
   (match (this)
     ((Move from: `(,s0 . ,source)
            to: `(,d0 ,d1 . ,destination)
	    with-shift: s)
      (Move from: (recons (+ d1 1) destination)
            to: (recons* s (- s0 1) source)
	    with-shift: d0))))
  ((cursor document::pair)::Cursor
   (cursor-climb-back to document)))

(define-type (Remove element: (either pair
				      HeadTailSeparator
				      EmptyListProxy)
		     from: Cursor
		     with-shift: int := 0)
  implementing Edit
  with
  ((apply! document::pair)::void
   (let ((item (extract! at: from from: document)))
     (assert (eq? item element))
     (values)))
  ((inverse)::Edit
   (match from
     (`(,tip . ,root)
      (Insert element: element
	      at: (recons* with-shift (- tip 1) root)))))
  ((cursor document::pair)::Cursor
   (recons* with-shift (- (car from) 1) (cdr from))))

(define-type (Insert element: (either pair HeadTailSeparator)
		     at: Cursor)
  implementing Edit
  with
  ((apply! document::pair)::void
   (insert! element into: document at: at))
  ((inverse)::Edit
   (match at
     (`(,tip ,top . ,root)
      (Remove element: element
	      from: (recons (+ top 1) root)
	      with-shift: tip))))
  ((cursor document::pair)::Cursor
   (and-let* ((`(,tip ,top . ,root) at)
	      (cursor (recons (+ top 1) root)))
     (match element
       (`(,,@gnu.lists.LList?)
	(cursor-retreat
	 (cursor-climb-back cursor document)))
       (_
	(cursor-climb-back cursor document))))))

(define-type (ResizeBox at: Cursor
			from: Extent
			to: Extent
			with-anchor: real)
  implementing Edit
  with
  ((apply! document::pair)::void
   (let* ((box (the-expression at: at in: document))
	  (ending (line-ending-embracing with-anchor box)))
     (resize! box to:width to:height ending)))
      
  ((inverse)::Edit
   (ResizeBox at: at
	      from: to
	      to: from
	      with-anchor: with-anchor))
  ((cursor document::pair)::Cursor
   (the-cursor)))

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

(define-object (History document::pair)
  (define fronts ::(list-of (list-of Edit)) '())

  (define undo-step ::int 0)
  
  (define (undo!)::void
    (and-let* ((`(,timeline . ,_) fronts)
	       (`(,last-action . ,_) (drop undo-step timeline))
	       (operation ::Edit last-action)
	       (inverse ::Edit (operation:inverse)))
      (inverse:apply! document)
      (set! (the-cursor) (inverse:cursor document))
      (set! (the-selection-anchor) (the-cursor))
      (set! undo-step (+ undo-step 1))))

  (define (redo!)::void
    (and-let* (((is undo-step > 0))
	       (`(,timeline . ,_) fronts)
	       (`(,undone-action . ,_) (drop (- undo-step 1)
					     timeline))
	       (operation ::Edit undone-action))
      (operation:apply! document)
      (set! (the-cursor) (operation:cursor document))
      (set! (the-selection-anchor) (the-cursor))
      (set! undo-step (- undo-step 1))))

  (define (record! operation ::Edit)::void
    (cond ((null? fronts)
	   (set! fronts (cons (cons operation '()) fronts)))
	  ((is undo-step > 0)
	   (set! fronts (cons (cons operation
				    (car fronts))
			      fronts))
	   (set! undo-step 0))
	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((Remove element: e 
			       from: source
			       with-shift: n) last-operation)
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
	  ((and-let* ((`((,last-operation . ,_) . ,_) fronts)
		      ((Insert element: `(,atom)
			       at: `(,t ,n . ,root)) last-operation)
		      (atom ::Atom atom)
		      (l (atom-length atom))
		      ((Insert element: `(,c)
			       at: `(,,l ,,(+ n 1)
					 . ,,root)) operation)
		      (c ::gnu.text.Char c)
		      ((isnt c separator?)))
	     ;; we're not recording anything, because
	     ;; atom is shared between history and the document,
	     ;; so that the change made to the document
	     ;; will affect history
	     ))
	  
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
