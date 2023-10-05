(module-name (editor document document-operations))

(import (srfi :11))
(import (language define-interface))
(import (language define-type))
(import (utils hash-table))
(import (language define-property))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor interfaces painting))

(import (editor types primitive))
(import (editor types spaces))
(import (editor document cursor))
(import (language assert))
(import (language match))
(import (language examples))
(import (language infix))
(import (utils functions))
(import (language keyword-arguments))
(import (utils print))
(import (language for))
(import (editor types texts))
(import (editor types comments))
(import (editor types extensions interactions))
(import (editor types extensions extensions))

;; extract! returns either a cons-cell whose
;; car is the desired object, or a head/tail-separator
;; (consider: what to do when cursor points to a space?
;; we can safely return #!null, because it's different
;; than returning (#!null))

(define (cell-width taken)::int
  (let* ((extent ::Extent (if (pair? taken)
			      (sequence-extent taken)
			      (extent taken)))
	 (painter ::Painter (the-painter)))
    (as int (round (/ extent:width (painter:space-width))))))

(define/kw (extract! at: cursor::Cursor := (the-cursor)
		     from: document::pair := (the-document))
  (define (increase-space! space::Space width::real)
    (and-let* ((last-fragment (last-pair space:fragments))
	       (`(,next . ,_) last-fragment))
      (if (integer? next)
	  (set! (car last-fragment)
		(as int (round (+ next width))))
	  (set! (cdr last-fragment)
		(cons (as int (round width))
		      (cdr last-fragment))))))
  (otherwise #!null
    (and-let* ((`(,tip ,top . ,root) cursor)
               (grandpa ::Indexable (cursor-ref document root))
               (parent ::Indexable (part-at top grandpa))
               (target ::Indexable (part-at tip parent)))
      (cond
       ((eq? parent target)
	(extract! at: (cdr cursor) from: document))
       ((pair? parent)
	(if (eqv? tip 1)
            (let* ((grandpa ::pair grandpa)
	           (cell (drop (quotient top 2) grandpa))
	           (removed (car cell)))
              (cond
	       ((dotted? removed)
		(let* ((new (cons (cdr removed) '())))
	          (tail-space-to-head removed new)
	          (set! (car cell) new)
	          (increase-space! (pre-head-space (car cell)) 1)
	          (unset! (dotted? removed))))
	       (else
		(when (pair? (cdr removed))
		  (set! (pre-head-space (cdr removed))
			(join-spaces! (pre-head-space (car cell))
				      (pre-head-space (cdr removed)))))
		(set! (car cell) (cdr removed))))
              (set! (cdr removed) '())
              (let ((removed-space (post-head-space removed)))
		(unset! (post-head-space removed))
		(when (pair? (car cell))
	          (increase-space! (pre-head-space (car cell))
				   (cell-width removed))
		  (join-spaces! (pre-head-space (car cell))
				removed-space)))
              (unset! (pre-head-space removed))
              removed)
	    (let* ((parent ::pair parent)
		   (index (quotient tip 2))
		   (skip (- index 1)))
	      
	      (define (remove-tail! preceding)
		(let* ((removed (cdr preceding))
		       (removed-space (post-head-space removed)))
		  (set! (cdr preceding) (cdr removed))
		  (set! (cdr removed) '())
		  (unset! (post-head-space removed))
		  (increase-space! (post-head-space preceding)
				   (cell-width removed))
		  (join-spaces! (post-head-space preceding)
				removed-space)
		  removed))
	      
	      (if (is skip > 0)
		  (let* ((skip (- skip 1))
			 (preceding (drop skip parent)))
		    (if (dotted? preceding)
			(let* ((removed (tail-space-to-head
					 preceding
					 (cons (cdr preceding) '()))))
			  (set! (cdr preceding) '())
			  (unset! (dotted? preceding))
			  (increase-space! (post-head-space preceding)
					   (+ 1 (cell-width removed)))
			  (unset! (post-head-space
				   (last-pair removed)))
			  removed)
			(remove-tail! (cdr preceding))))
		  
		  (let ((preceding (drop skip parent)))
		    (if (dotted? preceding)
			(let* ((separator (head-tail-separator
					   preceding))
			       (added (cons (cdr preceding) '())))
			  (increase-space! (post-head-space preceding)
					   1)
			  (join-spaces! (post-head-space preceding)
					(pre-tail-space preceding))
			  (set! (cdr preceding) added)
			  (unset! (dotted? preceding))
			  separator)
			(remove-tail! preceding)))))))
	((Comment? target)
         (let ((space ::Space parent))
	   (let-values (((preceding _) (space-fragment-index
					space:fragments
					(- tip 1))))
	     (and-let* ((`(,n::integer ,c::Comment
				       ,m::integer . ,r) preceding)
			(w (cell-width c)))
	       (cond ((c:breaks-line?)
		      (set-car! preceding (as int (+ n w)))
		      (set-cdr! preceding (cddr preceding)))
		     (else
		      (set-car! preceding (as int (+ n m w)))
		      (set-cdr! preceding r)))
	       c))))
	(else
	 #!null)))))

(define/kw (insert! element
		    into: document ::pair := (the-document)
		    at: cursor ::Cursor := (the-cursor))
  ::boolean
  (and-let* ((`(,tip::integer ,top::integer . ,root) cursor)
	     (grandpa ::list (cursor-ref document root))
	     (parent ::Space (part-at top grandpa))
	     (target ::Space (part-at tip parent)))
    (cond
     ((Comment? element)
      (let-values (((suffix remnant)
		    (space-fragment-index target:fragments
					  tip))
		   ((w) (cell-width element)))
	(cond
	 ((and-let* ((`(,n::integer . ,rest) suffix)
		     (coda (max 0 (- n remnant w))))
	    (assert (is n >= remnant))
	     (set-car! suffix remnant)
	     (set-cdr! suffix (cons* element coda rest))
	     #t))
	  ((and-let* ((suffix (last-cell (is (car _) integer?)
					 target:fragments))
		      (`(,head . ,tail) suffix))
	     (set-car! suffix tip)
	     (set-cdr! suffix (cons element
				    (cons (as int (max 0 (- head tip
							    w)))
					  tail)))
	     #t))
	  (else
	   (set! target:fragments
		 (cons* tip element target:fragments))
	   #t))))
		 
     ((is top <= 1)
      (and-let* ((`(,heir . ,origin) root)
		 (predecesor ::pair (cursor-ref document
						origin))
		 (parent (drop (quotient heir 2)
			       predecesor))
		 (following-space ::Space (split-space!
					   target
					   tip)))
	(set! (car following-space:fragments)
	      (as int
		  (max (if (empty? (car parent)) 0 1)
		       (- (car following-space:fragments)
			  (cell-width element)))))
	(set! (post-head-space element)
	      following-space)
	(set! (last-tail element) (car parent))
	(set! (car parent) element) #t))
     (else
      (let* ((irrelevant (- (quotient top 2) 1))
	     (before (drop irrelevant grandpa)))
  	(cond
	 ((pair? element)
	  (let ((following-space ::Space
				 (split-space! target
					       tip)))
	    (set! (car following-space:fragments)
		  (as int
		      (max (if (empty? (cdr before))
			       0
			       1)
			   (- (car following-space:fragments)
			      (cell-width element)))))
	    (set! (post-head-space element)
		  following-space))
	  (set! (last-tail element) (cdr before))
	  (set! (cdr before) element) #t)
	 
	 ((empty? (cdr (cdr before)))
	  (assert (head/tail-separator? element))
	  (let ((following-space ::Space
				 (split-space! target
					       tip)))
	    (set! (car following-space:fragments)
		  (as int
		      (max
		       (- (car following-space:fragments)
			  1)
		       1)))
	    (set! (pre-tail-space before)
		  following-space))
	  (set! (cdr before) (car (cdr before)))
	  (update! (dotted? before) #t) #t)

	 (else
	  (WARN "Attempt to splice "element
		" in non-tail position") #f)))))))

(define/kw (replace-expression! at: cursor ::Cursor := (the-cursor)
				with: replacement
				in: document := (the-document))
  (match cursor
    (`(,,@(isnt _ integer?) . ,subcursor)
     (replace-expression! at: subcursor
			  with: replacement
			  in: document))

    (`(,,@(isnt _ odd?) . ,subcursor)
     (replace-expression! at: subcursor
			  with: replacement
			  in: document))
    
    (`(,index . ,subcursor)
     (let* ((parent (cursor-ref document subcursor))
	    (previous-index (- (quotient index 2) 1)))
       (cond ((head/tail-separator? replacement)
	      (unless (or (head/tail-separator? (cell-index parent
							    index))
			  (is previous-index < 0))
		(let ((cell (drop previous-index parent)))
		  (assert (empty? (cdddr cell)))
		  (set! (cdr cell) (caddr cell))
		  (update! (dotted? cell) #t)
		  document)))
	     ((head/tail-separator? (cell-index parent index))
	      (unless (is previous-index < 0)
		(let ((cell (drop previous-index parent)))
		  (set! (cdr cell) (cons replacement
					 (cons (cdr cell) '())))
		  (update! (dotted? cell) #f)
		  document)))
	     (else 
	      (set! (cell-index parent index) replacement)
	      document))))))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(1 1)
			with: 'x
			in: document)
   document) ===> ((x 2 . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(3 1)
			with: 'x
			in: document)
   document) ===> ((1 x . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(5 1)
			with: 'x
			in: document)
   document) ===> ((1 2 x 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(7 1)
			with: 'x
			in: document)
   document) ===> ((1 2 . x)))

(e.g.
 (let ((document `((,1 ,2 ,3))))
   (replace-expression! at: '(3 1)
			with: head/tail-separator
			in: document)
   document) ===> ((1 . 3)))

(define/kw (enchant-expression! at: cursor::Cursor := (the-cursor)
				in: document := (the-document))
  (parameterize ((cell-access-mode CellAccessMode:Evaluating))
    (and-let* ((expression ::cons (the-expression at: cursor
						  in: document))
	       (`(,keyword::symbol . ,data) expression)
	       (magic ::Extension (extension keyword))
	       (enchanted ::Enchanted (magic:enchant expression)))
      (set! (origin enchanted) expression)
      (replace-expression! at: cursor with: enchanted
			   in: document)
      enchanted)))

(define/kw (disenchant-expression! at: cursor::Cursor := (the-cursor)
				   in: document := (the-document))
  (parameterize ((cell-access-mode CellAccessMode:Evaluating))
    (and-let* ((enchanted ::Enchanted (the-expression at: cursor
						      in: document))
	       (expression (enchanted:as-expression)))
      (replace-expression! at: cursor with: expression
			   in: document)
      expression)))
