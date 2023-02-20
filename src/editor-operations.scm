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

(define (unnest-cursor-right!)
  (and-let* ((`(,tip ,top . ,root) (the-cursor))
	     (parent ::Indexable (the-expression at: root))
	     (target ::Indexable (parent:part-at top))
	     (item ::Indexable (target:part-at tip)))
    ;;(assert (eq? target item))
    (set! (the-cursor)
	  (cond
	   ((Textual? item)
	    (recons (parent:last-index) root))
	   ((eqv? tip (parent:last-index))
	    (recons (parent:last-index) root))
	   (else
	    (recons* (parent:last-index) top root))))
    (set! (the-selection-anchor) (the-cursor))))

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

(define (delete-backward!)::boolean

  (define (perform! operation ::Edit)::boolean
    (and-let* ((document (the-document))
	       (history ::History (history document))
	       (new-cursor (operation:apply! document)))
      ;; A note: in case of removal operations,
      ;; we record the operation after applying it,
      ;; but in case of insertion operation, we record
      ;; them before applying them.
      ;; This allows for structural sharing to work
      ;; in the presence of history merging.
      (history:record! operation)
      (set! (the-cursor) new-cursor)
      (set! (the-selection-anchor) new-cursor)
      #t))

  (and-let* ((`(,tip ,top . ,root) (the-cursor))
	     (parent ::Indexable (the-expression at: root))
	     (target ::Indexable (parent:part-at top))
	     ((eq? target (target:part-at tip)))
	     (first-index ::Index (target:first-index))
	     (last-index ::Index (target:last-index))
	     (preceding-cursor (cursor-retreat (recons*
						first-index
						top root)))
	     (preceding-element (the-expression at: preceding-cursor)))
    (cond
     ((Atom? target)
      (let ((target ::Atom target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (set! (the-selection-anchor) (the-cursor))
	  (delete-backward!))
	 ((is (text-length target) <= 1)
	  (let ((cell (drop (quotient top 2) parent)))
	    ;; the cell will be cut off from the rest
	    ;; of the document after performing Remove
	    (perform! (Remove element: cell
			      at: (recons top root)
			      with-shift: (car preceding-cursor)))))
	 (else
	  (perform!
	   (RemoveCharacter
	    list: (cons (target:char-ref (target:previous-index tip))
			'())))))))
     ((Space? target)
      (let ((target ::Space target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (set! (the-selection-anchor) (the-cursor))
	  (delete-backward!))
	 ((is (text-length target) <= 1)
	  (cond
	   ((Textual? preceding-element)
	    (and-let* ((following-cursor (cursor-advance
					  (recons* last-index
						   top root)))
		       (following-element ::Textual
					  (the-expression
					   at: following-cursor))
		       (preceding-element ::Textual preceding-element)
		       ((eq? (preceding-element:getClass)
			     (following-element:getClass))))
	      (perform! (MergeElements removing: target
				       after: preceding-cursor))))
	   ((and (eq? preceding-element parent)
		 (is (text-length (as Space target)) > 0))
	    (perform! (RemoveCharacter
		       list: (cons (target:char-ref tip) '()))))
	   ;; teoretycznie moglibysmy tutaj dodac scalanie
	   ;; list
	   (else #f)))
	 (else
	  (perform! (RemoveCharacter
		     list: (cons (target:char-ref tip)
				 '())))))))
     ((Text? target)
      (let ((target ::Text target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (set! (the-selection-anchor) (the-cursor))
	  (delete-backward!))
	 ((or (eqv? tip last-index)
	      (is (text-length (as Text target)) <= 0))
	  (let ((cell (drop (quotient top 2) parent)))
	    ;; the cell will be cut off from the rest
	    ;; of the document after performing Remove
	    (perform! (Remove element: cell
			      at: (recons top root)
			      with-shift: (text-length
					   preceding-element)))))
	 (else
	  (perform! (RemoveCharacter list: (cons (target:char-ref
						  (- tip 1))
						 '())))))))
     ((gnu.lists.LList? target)
      (let ((cell (drop (quotient top 2) parent)))
	(if (or (eqv? tip last-index)
		(null? target)
		(and-let* ((empty ::EmptyListProxy target)
			   ((is empty:space EmptySpace?)))))
	    (perform! (Remove element: cell
			      at: (recons top root)
			      with-shift: (text-length
					   preceding-element)))
	    #f
	    )))
     (else
      #f))))

(define (delete-forward!)::boolean
  (let ((target ::Indexable (the-expression)))
    (cond
     ((gnu.lists.LList? target)
      (match (the-cursor)
	(`(,,(target:first-index) . ,_)
	 (insert-character! #\])
	 (delete-backward!))
	(`(,,(target:last-index) . ,_)
	 (move-cursor-right!)
	 (delete-forward!))))
     (else
      (move-cursor-right!)
      (delete-backward!)))))

(define (insert-character! c::char)::boolean
  (define (perform! operation ::Edit)::boolean
    (let* ((document (the-document))
	   (history ::History (history document)))
      (history:record! operation)
      (and-let* ((new-cursor (operation:apply! document)))
	(set! (the-cursor) new-cursor)
	(set! (the-selection-anchor) new-cursor)
	#t)))
  (and-let* (((isnt c eqv? #\null))
	     (`(,tip ,top . ,subcursor) (the-cursor))
	     (parent ::Indexable (the-expression at: subcursor))
	     (item ::Indexable (parent:part-at top))
	     (final ::Indexable (item:part-at tip)))
    (cond
     ((isnt final eq? item)
      (WARN "attempted to insert character "c" to non-final position")
      #f)
     ((Text? item)
      (perform! (InsertCharacter list: (list c))))

     ((is c in '(#\] #\) #\}))
      (unnest-cursor-right!))
      
     ((gnu.lists.LList? item)
      (set! (the-cursor) (cursor-advance))
      (set! (the-selection-anchor) (the-cursor))
      (insert-character! c))
     
     ((Space? item)
      (cond
       ((eqv? c #\")
	(perform! (Insert element: (cons (Text) '()))))
       
       ((is c in '(#\[ #\( #\{))
	(perform! (Insert element: (cons (empty) '()))))
       ((is c char-whitespace?)
	(perform! (InsertCharacter list: (list c))))
       
       ((and-let* (((is tip eqv? (item:last-index)))
		   (next-cursor (cursor-advance))
		   (next-target (the-expression at: next-cursor))
		   ((Atom? next-target)))
	  (perform! (InsertCharacter list: (list c)
				     after: next-cursor))))

       ((and-let* (((is tip eqv? (item:first-index)))
		   (previous-cursor (cursor-retreat))
		   (previous-target (the-expression
				     at: previous-cursor))
		   ((Atom? previous-target)))
	  (perform! (InsertCharacter list: (list c)
				     after: previous-cursor))))
       (else
	(perform! (Insert element: (cons (Atom (string c)) '()))))))
     ((Atom? item)
      (cond
       ((is c char-whitespace?)
	(cond
	 ((eqv? (final:first-index) tip)
	  (perform! (InsertCharacter list: (list c)
				     after: (cursor-retreat))))
	 ((eqv? (final:last-index) tip)
	  (perform! (InsertCharacter list: (list c)
				     after: (cursor-advance))))
	 (else
	  (perform! (SplitElement with: (SpaceFrom c))))))
       
       ((is c in '(#\[ #\( #\{))
	(cond
	 ((eqv? (final:first-index) tip)
	  (perform! (Insert element: (cons (empty) '())
			    at: (cursor-retreat))))
	 ((eqv? (final:last-index) tip)
	  (perform! (Insert element: (cons (empty) '())
			    at: (cursor-advance))))
	 (else
	  (perform! (SplitElement with: (EmptySpace)))
	  (perform! (Insert element: (cons (empty) '()))))))
       (else
	(perform! (InsertCharacter list: (list c))))))
     ((Text? item)
      (InsertCharacter list: (list c)))
	 
     (else
      (WARN "Don't know how to insert character "c" into "item)
      #f
      ))))
