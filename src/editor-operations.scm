(import (define-parameter))
(import (keyword-arguments))
(import (functions))
(import (fundamental))
(import (indexable))
(import (space))
(import (cursor))
(import (primitive))
(import (document-operations))
(import (infix))
(import (match))
(import (print))
(import (extent))
(import (painter))
(import (history))
(import (text))

(define-parameter (cursor-column)::real 0)

(define (move-cursor-right!)
  (set! (the-cursor) (cursor-advance))
  (let* ((painter (the-painter))
	 (cursor-position ::Position (painter:cursor-position)))
    (set! (cursor-column) cursor-position:left))
  (set! (the-selection-anchor) (the-cursor)))

(define (move-cursor-left!)
  (set! (the-cursor) (cursor-retreat))
  (let* ((painter (the-painter))
	 (cursor-position ::Position (painter:cursor-position)))
    (set! (cursor-column) cursor-position:left))
  (set! (the-selection-anchor) (the-cursor)))

(define (expand-selection-right!)
  (set! (the-cursor) (cursor-advance))
  (let* ((painter (the-painter))
	 (cursor-position ::Position (painter:cursor-position)))
    (set! (cursor-column) cursor-position:left)))

(define (expand-selection-left!)
  (set! (the-cursor) (cursor-retreat))
  (let* ((painter (the-painter))
	 (cursor-position ::Position (painter:cursor-position)))
    (set! (cursor-column) cursor-position:left)))

(define (move-cursor-up!)
  (let* ((painter (the-painter))
	 (target (the-expression))
	 (initial-position ::Position (painter:cursor-position))
	 (cursor-height (painter:cursor-height))
	 (initial-cursor (the-cursor)))
    (let probe ((attempt 1))
      (let* ((shift (* attempt cursor-height))
	     (cursor (cursor-under (cursor-column)
				  (- initial-position:top
				     shift))))
	(cond ((isnt cursor equal? initial-cursor)
	       (set! (the-cursor) cursor)
	       (set! (the-selection-anchor) cursor))
	      ((is 0 < shift < initial-position:top)
	       (probe (+ attempt 1))))))))

(define (move-cursor-down!)
  (let* ((painter (the-painter))
	 (target (the-expression))
	 (initial-position ::Position (painter:cursor-position))
	 (cursor-height (painter:cursor-height))
	 (document-extent ::Extent (sequence-extent))
	 (initial-cursor (the-cursor)))
    (let probe ((attempt 1))
      (let* ((shift (* attempt cursor-height))
	     (cursor (cursor-under (cursor-column)
				  (+ initial-position:top
				     shift))))
	(cond ((isnt cursor equal? initial-cursor)
	       (set! (the-cursor) cursor)
	       (set! (the-selection-anchor) cursor))
	      ((is 0 < shift < (+ initial-position:top shift)
		   < document-extent:height)
	       (probe (+ attempt 1)))
	      )))))

(define (undo!)
  (let ((document-history ::History (history (the-document))))
    (document-history:undo!)))

(define (redo!)
  (let ((document-history ::History (history (the-document))))
    (document-history:redo!)))

(define (delete! position::Index)::void
  (let* ((target (the-expression)))
    (cond
     ((is target Textual?)
      (let ((text ::Textual (as Textual target)))
	(cond ((is 0 <= position < (text:text-length))
	       (text:delete-char! position)
	       (when (= (text:text-length) 0)
		 (extract! at: (cdr (the-cursor)))
		 (set! (the-cursor)
		       (cursor-climb-back
			(recons (- (car (cdr (the-cursor)))
				   1)
				(cdr (cdr (the-cursor))))))
		 (set! (the-selection-anchor) (the-cursor)))))))
     ((is target instance? Space)
      (if (or (is position > (first-index target))
	      (and (is position = (first-index target))
		   (or (and-let* ((`(#\] . ,_) (cursor-advance))))
		       (and-let* ((`(#\[ . ,_) (cursor-retreat)))))))
	  (delete-space! target position))))))

(define (delete-forward!)::void
  (let ((target (the-expression)))
    (cond ((and (pair? target)
		(pair? (the-cursor))
		(eqv? (car (the-cursor)) (first-index target)))
	   (let ((new-cursor (cursor-retreat)))
	     (extract!)
	     (set! (the-cursor) new-cursor)
	     (set! (the-selection-anchor) (the-cursor))))
	  (else
	   (delete! (car (the-cursor)))))))

(define (delete-backward!)::void
  (let ((target (the-expression)))
    (cond ((and (pair? target)
		(eqv? (car (the-cursor)) (last-index target)))
	   (let ((new-cursor (cursor-climb-back
			      (cursor-back (cdr (the-cursor))))))
	     (extract!)
	     (set! (the-cursor) new-cursor)
	     (set! (the-selection-anchor) (the-cursor))))
	  (else
	   (set! (the-cursor)
		 (cursor-climb-back (cursor-back)))
	   (set! (the-selection-anchor) (the-cursor))
	   (delete! (car (the-cursor)))))))


(define/kw (insert-character! c::char)
  ::boolean
  ;; musimy pamietac ze dzialana dokonywane poprzez
  ;;te funkcje powinno sie dac cofac za pomoca "undo!"
  (and-let* (((isnt c eqv? #\null))
	     (`(,tip ,top . ,subcursor) (the-cursor))
	     (parent ::Indexable (the-expression at: subcursor))
	     (item ::Indexable (parent:part-at top))
	     (final ::Indexable (item:part-at tip)))
    (cond
     ((isnt final eq? item)
      (WARN "attempted to insert character "c" to non-final position")
      #f)
     ((Space? item)
      (cond
       ((eqv? c #\") 
	;; wstawiamy nowy obiekt typu Text
	(let ((operation ::Insert (Insert element: (cons (Text) '())
					  at: (the-cursor)))
	      (history ::History (history (the-document))))
	  (history:record! operation)
	  (operation:apply! (the-document))
	  (move-cursor-right!)
	  #t))
       
       ((is c in '(#\[ #\( #\{))
	(let ((operation ::Insert (Insert element: (cons
						    (EmptyListProxy
						     (Space
						      fragments:
						      (cons 0 '())))
						    '())
					at: (the-cursor)))
	      (history ::History (history (the-document))))
	  (history:record! operation)
	  (set! (the-cursor) (operation:apply! (the-document)))
	  (set! (the-selection-anchor) (the-cursor))
	  #t))
       
       ((is c in '(#\] #\) #\}))
	(set! (the-cursor) (recons (parent:last-index)
				   subcursor))
	(set! (the-selection-anchor) (the-cursor))
	#t)

       ((is c char-whitespace?)
	(let ((operation ::Insert (Insert element: (cons c '())
					  at: (the-cursor)))
	      (history ::History (history (the-document))))
	  (history:record! operation)
	  (set! (the-cursor) (operation:apply! (the-document)))
	  (set! (the-selection-anchor) (the-cursor))
	  #t))
	
       (else
	(let ((operation ::Insert (Insert element: (cons
						    (Atom
						     (list->string
						      (list c)))
						    '())
					  at: (the-cursor)))
	      (history ::History (history (the-document))))
	  (history:record! operation)
	  
	  (set! (the-cursor) (operation:apply! (the-document)))
	  (set! (the-selection-anchor) (the-cursor))
	  #t))))
     (else
      (let ((operation ::InsertCharacter
		       (InsertCharacter
			list: (list c)
			after: (cond
				((and (Atom? final)
				      (eqv? (final:first-index)
					    tip))
				 (cursor-retreat (the-cursor)))
				((and (Atom? final)
				      (eqv? (final:last-index)
					    tip))
				 (cursor-advance (the-cursor)))
				(else
				 (the-cursor)))))
	    (history ::History (history (the-document))))
	(history:record! operation)
	(set! (the-cursor) (operation:apply! (the-document)))
	(set! (the-selection-anchor) (the-cursor))
	#t)))))
