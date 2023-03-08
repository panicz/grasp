(import (srfi :11))
(import (define-interface))
(import (define-type))
(import (hash-table))
(import (define-property))
(import (fundamental))
(import (indexable))
(import (painter))
(import (extent))
(import (primitive))
(import (space))
(import (cursor))
(import (assert))
(import (match))
(import (examples))
(import (infix))
(import (functions))
(import (keyword-arguments))
(import (print))
(import (parse))
(import (for))
(import (text))
(import (comments))


;; take-cell! returns either a cons-cell whose
;; car is the desired object, or a head/tail-separator
;; (consider: what to do when cursor points to a space?
;; we can safely return #!null, because it's different
;; than returning (#!null))

(define (cell-width taken::pair)::real
  (let* ((extent ::Extent (sequence-extent taken))
	 (painter ::Painter (the-painter)))
    (quotient extent:width (painter:space-width))))

(define/kw (extract! at: cursor::Cursor := (the-cursor)
		     from: document::pair := (the-document))
  (define (increase-space! space::Space width::real)
    (let ((last-fragment (last-pair space:fragments)))
      (set! (car last-fragment)
	(+ (car last-fragment) width))))
  
  (match cursor
    (`(,,@(isnt _ integer?) . ,root)
     (extract! at: root from: document))
    
    (`(,,@(is _ <= 1) ,parent-index . ,root)
     (let* ((grandparent ::pair (cursor-ref document root))
	    (cell (drop (quotient parent-index 2) grandparent))
	    (removed (car cell)))
       (if (dotted? removed)
	   (let* ((new (cons (cdr removed) '())))
	     (tail-space-to-head removed new)
	     (set! (car cell) new)
	     (increase-space! (pre-head-space (car cell)) 1)
	     (unset! (dotted? removed)))
	   (set! (car cell) (cdr removed)))
       (set! (cdr removed) '())
       (let ((removed-space (post-head-space removed)))
	 (unset! (post-head-space removed))
	 (when (pair? (car cell))
	   (increase-space! (pre-head-space (car cell))
			    (cell-width removed))
	   (join-spaces! (pre-head-space (car cell))
			 removed-space)))
       (unset! (pre-head-space removed))
       removed))
    
    (`(,index . ,root)
     (let* ((parent ::pair (cursor-ref document root))
	    (index (quotient index 2))
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
		   (unset! (post-head-space (last-pair removed)))
		   removed)
		 (remove-tail! (cdr preceding))))
	  
	   (let ((preceding (drop skip parent)))
	     (if (dotted? preceding)
		 (let* ((separator (head-tail-separator preceding))
			(added (cons (cdr preceding) '())))
		   (increase-space! (post-head-space preceding) 1)
		   (join-spaces! (post-head-space preceding)
				 (pre-tail-space preceding))
		   (set! (cdr preceding) added)
		   (unset! (dotted? preceding))
		   separator)
		 (remove-tail! preceding))))))
    (_
     document)))

(e.g.
 (let* ((document (string->document "1 3 5"))
	(taken (extract! at: '(3 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
   ===>
   "1   5"
   "3")

(e.g.
 (let* ((document (call-with-input-string "1 3 5"
		    parse-document))
	(taken (extract! at: '(5 1) from: document)))

   (values (document->string document)
	   (pair->string taken)))
   ===>
   "1 3  "
   "5")

(e.g.
 (let* ((document (call-with-input-string "(1 3 5)"
		    parse-document))
	(taken (extract! at: '(1 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
   ===>
   "(  3 5)"
   "1")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(3 1 1) from: document)))
   (assert (head/tail-separator? taken))
   (document->string document))
 ===> "(1   5)")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(1 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
 ===>
 "(    5)"
 "1")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(5 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
 ===>
 "(1    )"
 "5")

(define/kw (insert! element
		    into: document ::pair := (the-document)
		    at: cursor ::Cursor := (the-cursor))
  ::boolean
  (and-let* ((`(,tip ,top::integer . ,root) cursor)
	     (grandpa (cursor-ref document root))
	     (parent (part-at top grandpa))
	     (target (part-at tip parent)))
    (if (is top <= 1)
	(and-let* ((`(,heir . ,origin) root)
		   (predecesor ::pair (cursor-ref document
						  origin))
		   (parent (drop (quotient heir 2)
				 predecesor))
		   (following-space ::Space (split-space!
					     target
					     tip)))
	  (set! (car following-space:fragments)
		(max (if (empty? (car parent)) 0 1)
		     (- (car following-space:fragments)
			(cell-width element))))
	  (set! (post-head-space element)
		following-space)
	  (set! (last-tail element) (car parent))
	  (set! (car parent) element) #t)

	(let* ((irrelevant (- (quotient top 2) 1))
	       (before (drop irrelevant grandpa)))
	  (cond ((pair? element)
		 (let ((following-space ::Space
					(split-space! target
						      tip)))
		   (set! (car following-space:fragments)
			 (max (if (empty? (cdr before)) 0 1)
			      (- (car following-space:fragments)
				 (cell-width element))))
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
			 (max
			  (- (car following-space:fragments)
			     1)
			  1))
		   (set! (pre-tail-space before)
			 following-space))
		 (set! (cdr before) (car (cdr before)))
		 (update! (dotted? before) #t) #t)

		(else
		 (WARN "Attempt to splice "element
		       " in non-tail position") #f))
	  ))))

(e.g.
 (let ((document (string->document "1   5")))
   (insert! (parse-string "3") into: document at: '(1 2 1))
   (document->string document)) ===> "1 3 5")

(e.g.
 (let ((document (string->document "1     7")))
   (insert! (parse-string "3 5") into: document at: '(1 2 1))
   (document->string document)) ===> "1 3 5 7")

(e.g.
 (let ((document (string->document "3 5")))
   (insert! (parse-string "1") into: document at: '(0 0 1))
   (document->string document)) ===> "1 3 5")

(e.g.
 (let ((document (string->document "5 7")))
   (insert! (parse-string "1 3") into: document at: '(0 0 1))
   (document->string document)) ===> "1 3 5 7")

(e.g.
 (let ((document (string->document "1   5")))
   (insert! head/tail-separator
	    into: document at: '(1 2 1))
   (document->string document)) ===> "1 . 5")

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

