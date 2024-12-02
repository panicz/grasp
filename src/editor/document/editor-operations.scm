(module-name (editor document editor-operations))

(import (language define-parameter))
(import (language keyword-arguments))
(import (language infix))
(import (language match))
(import (language while))
(import (language for))
(import (language fundamental))

(import (utils functions))
(import (utils print))

(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor types spaces))
(import (editor types primitive))
(import (editor document cursor))
(import (editor document document-operations))
(import (editor document history-tracking))
(import (editor types texts))
(import (editor types comments))
(import (editor types extensions extensions))
(import (editor types extensions quotations))

(define (update-cursor-column!)::void
  (let* ((cursor-position ::Position (painter:marked-cursor-position))
	 (editor ::Editor (the-editor)))
    (editor:add-post-draw-action!
     (lambda ()
       (editor:set-cursor-column!
	cursor-position:left)))))

(define (move-cursor-left!)::void
  (and-let* ((editor ::Editor (the-editor)))
    (editor:move-cursor-left!)))

(define (move-cursor-right!)::void
  (and-let* ((editor ::Editor (the-editor)))
    (editor:move-cursor-right!)))

(define (move-cursor-up!)::void
  (and-let* ((editor ::Editor (the-editor)))
    (editor:move-cursor-up!)))

(define (move-cursor-down!)::void
  (and-let* ((editor ::Editor (the-editor)))
    (editor:move-cursor-down!)))

(define (unnest-cursor-right!)::void
  (and-let* ((editor ::Editor (the-editor)))
    (editor:unnest-cursor-right!)))

(define (expand-selection-right!)::void
  (let* ((editor ::Editor (the-editor)))
    (editor:expand-selection-right!)))

(define (expand-selection-left!)::void
  (let* ((editor ::Editor (the-editor)))
    (editor:expand-selection-left!)))

(define (undo!)
  (let ((document-history ::History (history (the-document))))
    (document-history:undo!)))

(define (redo!)
  (let ((document-history ::History (history (the-document))))
    (document-history:redo!)))

(define (perform&record! operation ::Edit)::boolean
  (and-let* ((document (the-document))
	     (history ::History (history document))
	     (selection ::Highlight (the-selection))
	     (new-cursor (operation:apply! document)))
    ;; A note: in case of removal operations,
    ;; we record the operation after applying it,
    ;; but in case of insertion operation, we record
    ;; them before applying them.
    ;; This allows for structural sharing to work
    ;; in the presence of history merging.
    (history:record! operation)
    (set! (the-cursor) new-cursor)
    (set! selection:start new-cursor)
    (set! selection:end new-cursor)
    (update-cursor-column!)
    #t))

(define (delete-backward!)::boolean
  (and-let* ((cursor (the-cursor))
	     (`(,tip . ,stem) cursor)
	     (`(,top . ,root) stem)
	     (parent ::Indexable (cursor-ref (the-document) root))
	     (target ::Indexable (parent:part-at top))
	     ((Highlight start: selection-start
			 end: selection-end) (the-selection))
	     (`(,selection-start-tip . ,selection-start-stem)
	      selection-start)
	     (`(,selection-end-tip . ,selection-end-stem)
	      selection-end)
	     ((eq? target (target:part-at tip)))
	     (first-index ::Index (target:first-index))
	     (last-index ::Index (target:last-index))
	     (preceding-cursor (cursor-retreat (recons*
						first-index
						top root)))
	     (preceding-element (cursor-ref (the-document)
					    preceding-cursor)))
    (update-cursor-column!)
    (cond
     ((and (Textual? target)
	   (equal? selection-start-stem selection-end-stem)
	   (isnt selection-start-tip equal? selection-end-tip)
	   (integer? selection-start-tip)
	   (integer? selection-end-tip))
      (and-let* ((selection ::Highlight (the-selection)))
	(set! selection:start cursor)
	(set! selection:end cursor))

      (if (and (equal? (target:first-index) selection-start-tip)
	       (equal? (target:last-index) selection-end-tip))
	  (record&perform!
	   (Remove element: (drop (quotient top 2) parent)
		   at: selection-start-stem
		   with-shift: (car preceding-cursor)))
	  (let ((selection ::string (make-string
				     (- selection-end-tip
					selection-start-tip)))
		(source ::Textual (as Textual target)))
	    (for i from selection-start-tip below selection-end-tip
		 (string-set! selection (- i selection-start-tip)
			      (source:char-ref i)))
	    (record&perform!
	     (RemoveCharacter list: selection
			      before: selection-end)))))
     
     ((Atom? target)
      (let ((target ::Atom target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (delete-backward!))
	 
	 ((is (text-length target) <= 1)
	  ;; the cell will be cut off from the rest
	  ;; of the document after performing Remove
	  (let* ((parent (match parent
			   (quoted::Quotation quoted:expression)
			   (_ parent)))
		 (element (drop (quotient top 2) parent)))
	    (perform&record!
	     (Remove element: element
		     at: (recons top root)
		     with-shift: (car preceding-cursor)))))
	 (else
	  (perform&record!
	   (RemoveCharacter
	    list: (cons (target:char-ref
			 (target:previous-index tip))
			'())))))))
     ((Space? target)
      (let ((target ::Space target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (delete-backward!))
	 ((is (text-length target) <= 1)
	  (cond
	   ((Textual? preceding-element)
	    (let* ((following-cursor (cursor-advance
					  (recons* last-index
						   top root)))
		   (following-element (cursor-ref (the-document)
						  following-cursor))
		   (preceding-element ::Textual preceding-element))
	      (if (eq? (preceding-element:getClass)
		       (following-element:getClass))
		  (perform&record!
		   (MergeElements removing: target
				  after: preceding-cursor))
		  (perform&record!
		   (RemoveCharacter
		    list: (cons (target:char-ref tip) '()))))))
	   ((and (eq? preceding-element parent)
		 (is (text-length (as Space target)) > 0))
	    (perform&record!
	     (RemoveCharacter
	      list: (cons (target:char-ref tip) '()))))
	   ;; teoretycznie moglibysmy tutaj dodac scalanie
	   ;; list
	   (else
	    #f)))
	 (else
	  (perform&record!
	   (RemoveCharacter
	    list: (cons (target:char-ref tip)
			'())))))))
     ((Text? target)
      (let ((target ::Text target))
	(cond
	 ((eqv? tip first-index)
	  (set! (the-cursor) (cursor-retreat))
	  (delete-backward!))
	 ((or (eqv? tip last-index)
	      (is (text-length (as Text target)) <= 0))
	  ;; the cell will be cut off from the rest
	  ;; of the document after performing Remove
	  (perform&record!
	   (Remove element: (drop (quotient top 2) parent)
		   at: (recons top root)
		   with-shift: (text-length
				preceding-element))))
	 (else
	  (perform&record!
	   (RemoveCharacter list: (cons (target:char-ref
					 (- tip 1))
					'())))))))
     ((TextualComment? target)
      (let ((target ::TextualComment target))
	(cond
	 ((target:removable?)
	  (perform&record!
	   (RemoveComment content: target
			  at: (recons top root))))
	 (else
	  (perform&record!
	   (RemoveCharacter list: (cons (target:char-ref
					 (- tip 1))
					'())))))))
     ((gnu.lists.LList? target)
      (if (or (eqv? tip last-index)
	      (null? target)
	      (and-let* ((empty ::EmptyListProxy target)
			 ((is empty:space EmptySpace?)))))
	  (perform&record!
	   (Remove element: (drop (quotient top 2) parent)
		   at: (recons top root)
		   with-shift: (text-length
				preceding-element)))
	  #f))
     ((Enchanted? target)
      (and-let* ((ground-stem (suffix-without (isnt _ integer?)
					      stem))
		 (`(,top . ,root) ground-stem)
		 (parent (cursor-ref (the-document) root))
		 (preceding-cursor (cursor-retreat ground-stem))
		 (preceding-element (cursor-ref (the-document)
						preceding-cursor)))
	(perform&record!
	 (Remove element: (drop (quotient top 2) parent)
		 at: (recons top root)
		 with-shift: (text-length
			      preceding-element)))))
     (else
      (WARN "unable to delete "target)
      #f))))

(define (delete-forward!)::boolean
  (let ((target ::Indexable (cursor-ref)))
    (update-cursor-column!)
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
      (and-let* ((selection ::Highlight (the-selection))
		 ((equal? selection:start selection:end)))
	(move-cursor-right!))
      (delete-backward!)))))

(define (record&perform! operation ::Edit)::boolean
  (let* ((document (the-document))
	 (selection ::Highlight (the-selection))
	 (history ::History (history document)))
    (history:record! operation)
    (and-let* ((new-cursor (operation:apply! document)))
      (set! (the-cursor) new-cursor)
      (set! selection:start new-cursor)
      (set! selection:end new-cursor)
      (update-cursor-column!)
      #t)))

(define (insert-character! c::char)::boolean
  (and-let* (((isnt c eqv? #\null))
	     (`(,tip ,top . ,subcursor) (the-cursor))
	     ((Highlight start: selection-start
			 end: selection-end) (the-selection))
	     (`(,selection-start-tip . ,selection-start-stem)
	      selection-start)
	     (`(,selection-end-tip . ,selection-end-stem)
	      selection-end)
	     (parent ::Indexable (cursor-ref (the-document)
					     subcursor))
	     (item ::Indexable (parent:part-at top))
	     (final ::Indexable (item:part-at tip)))
    (update-cursor-column!)
    (cond
     ((isnt final eq? item)
      (WARN "attempted to insert character "c" to non-final position")
      #f)
     
     ((or (Text? item) (TextualComment? item))
      (cond
       ((equal? selection-start selection-end)
	(record&perform! (InsertCharacter list: (list c))))
       
       ((and (equal? selection-start-stem selection-end-stem)
	     (integer? selection-start-tip)
	     (integer? selection-end-tip))
	(let ((selection ::string (make-string (- selection-end-tip
						  selection-start-tip)))
	      (source ::Textual (as Textual item)))
	  (for i from selection-start-tip below selection-end-tip
	       (string-set! selection (- i selection-start-tip)
			    (source:char-ref i)))
	  (record&perform!
	   (RemoveCharacter list: selection
			    before: selection-end))
	  (record&perform!
	   (InsertCharacter list: (list c)
			    after: selection-start))))
	  
       (else
	(WARN "selections spanning multiple objects\
 are currently not supported"))))
     
     ((is c in '(#\] #\) #\}))
      (and-let* ((`(,_ ,_ . ,_) subcursor))
	(unnest-cursor-right!)))

     ((is c eqv? #\|)
      (cond
       ((and (Atom? item) (pair? parent))
	(let* ((atom ::Atom item)
	       (n ::int (atom:text-length))
	       (preceding-cursor (cursor-retreat
				  (recons* 0 top subcursor)))
	       (following-cursor (cursor-advance
				  (recons* n top subcursor))))
	  (cond
	   ((and (is tip >= 1) (eq? (atom:char-ref (- tip 1)) #\#))
	    (cond
	     ((= n 1)
	      ;; remove the single # atom and insert an empty comment
	      ;; into the space preceding that atom
		(record&perform!
		 (EditSequence
		  operations:
		  (list
		   (Remove element: (drop (quotient top 2) parent)
			   at: (recons top subcursor)
			   with-shift: (car preceding-cursor))
		   (InsertComment content: (BlockComment (Text))
				  at: preceding-cursor)))))
	     ((= tip 1)
	      ;; remove the # character from the beginning 
	      ;; of the atom and insert an empty comment
	      ;; into the space preceding that atom
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '()))
		 (InsertComment content: (BlockComment (Text))
				at: preceding-cursor)))))

	     ((= tip n)
	      ;; remove the # character from the end
	      ;; of the atom and insert an empty comment
	      ;; into the space following that atom
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '()))
		 (InsertComment content: (BlockComment (Text))
				at: following-cursor)))))
	     
	     (else
	      ;; remove the # character from the middle of the atom.
	      ;; then split the atom and insert a new block comment
	      ;; between the splitted parts
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '()))
		 (SplitElement at: (recons* (- tip 1) top subcursor)
			       with: (Space (cons* 0 (BlockComment (Text))
						   0 '())))))))))
	   ((and (is tip < n) (eq? (atom:char-ref tip) #\#))
	     (cond
	     ((= n 1)
	      ;; remove the single # atom and insert an empty comment
	      ;; into the space preceding that atom
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (Remove element: (drop (quotient top 2) parent)
			 at: (recons top subcursor))
		 (InsertComment content: (BlockComment (Text))
				at: preceding-cursor)))))
	     ((= tip 0)
	      ;; remove the # character from the beginning 
	      ;; of the atom and insert an empty comment
	      ;; into the space preceding that atom
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '())
				  before: (recons* 1 top subcursor))
		 (InsertComment content: (BlockComment (Text))
				at: preceding-cursor)))))
	     ((= tip (- n 1))
	      ;; remove the # character from the end
	      ;; of the atom and insert an empty comment
	      ;; into the space following that atom
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '())
				  before: (recons* n top cursor))
		 (InsertComment content: (BlockComment (Text)))))))
	     (else
	      ;; remove the # character from the middle of the atom.
	      ;; then split the atom and insert a new block comment
	      ;; between the splitted parts
	      (record&perform!
	       (EditSequence
		operations:
		(list
		 (RemoveCharacter list: (cons #\# '())
				  before: (recons* (+ tip 1) top
						   subcursor))
		 (SplitElement with: (Space (cons* 0 (BlockComment (Text))
						   0 '())))))))
	     )))))))
     
     ((Space? item)
      (cond
       ((eqv? c #\")
	(record&perform! (Insert element: (cons (Text) '()))))
       
       ((is c in '(#\[ #\( #\{))
	(record&perform! (Insert element: (cons (empty) '()))))

       ((is c char-whitespace?)
	(record&perform! (InsertCharacter list: (list c))))

       ((eqv? c #\;)
	(record&perform! (InsertComment content: (LineComment (Text)))))
       
       ((and-let* (((is tip eqv? (item:last-index)))
		   (next-cursor (cursor-advance))
		   (next-target (cursor-ref (the-document)
					    next-cursor))
		   ((Atom? next-target)))
	  (record&perform! (InsertCharacter list: (list c)
				     after: next-cursor))))

       ((and-let* (((is tip eqv? (item:first-index)))
		   (previous-cursor (cursor-retreat))
		   (previous-target (cursor-ref (the-document)
						previous-cursor))
		   ((Atom? previous-target)))
	  (record&perform! (InsertCharacter list: (list c)
				     after: previous-cursor))))
       (else
	(record&perform!
	 (Insert element: (cons (Atom (string c)) '()))))))

     ((is c eqv? #\;)
      ;; mowiac najkrocej: jezeli rodzic targeta to komentarz,
      ;; to chcemy go wykomentowac, natomiast w przeciwnym razie
      ;; chcemy go zakomentowac
      (if (is parent ExpressionComment?)
	  (record&perform! (UncommentExpression))
	  (and-let* ((`(,tip ,top::integer . ,root) (the-cursor))
		     (parent ::Indexable (cursor-ref (the-document)
						     root))
		     (preceding ::Space (parent:part-at (- top 1)))
		     (shift ::integer (preceding:last-index)))
	    (record&perform! (CommentExpression with-shift: shift)))))
     
     ((gnu.lists.LList? item)
      (set! (the-cursor) (cursor-advance))
      (insert-character! c))
     
     ((Atom? item)
      (cond
       ((is c char-whitespace?)
	(cond
	 ((eqv? (final:first-index) tip)
	  (record&perform! (InsertCharacter list: (list c)
				     after: (cursor-retreat))))
	 ((eqv? (final:last-index) tip)
	  (record&perform! (InsertCharacter list: (list c)
				     after: (cursor-advance))))
	 (else
	  (record&perform! (SplitElement with: (SpaceFrom c))))))
       
       ((is c in '(#\[ #\( #\{))
	(and
	 (cond
	  ((eqv? (final:first-index) tip)
	   (record&perform! (Insert element: (cons (empty) '())
				    at: (cursor-retreat))))
	  ((eqv? (final:last-index) tip)
	   (record&perform! (Insert element: (cons (empty) '())
				    at: (cursor-advance))))
	  (else
	   (record&perform! (SplitElement with: (EmptySpace)))
	   (record&perform! (Insert element: (cons (empty) '())))))
	 (truly (move-cursor-left!))))
       
       (else
	(record&perform! (InsertCharacter list: (list c))))))
     ((Text? item)
      (InsertCharacter list: (list c)))
	 
     (else
      (WARN "Don't know how to insert character "c" into "item)
      #f
      ))))
