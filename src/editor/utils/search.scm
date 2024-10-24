(module-name (editor utils search))

(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor types spaces))
(import (editor types comments))
(import (editor document cursor))
(import (utils functions))
(import (utils print))

(define-mapping (search-bindings key ::String)
  ::(maybe Tile)
  #!null)

(define-alias Bindings (!maps (String) to: (maybe Tile)))

(define (match-highlight pattern ::Tile
			 document ::Tile
			 cursor ::Cursor
			 bindings ::Bindings)
  
  ::(maybe Highlight)
  (let* ((pattern-index ::int 0)
	 (n ::int (length pattern))
	 (item-index (car cursor))
	 (context (cdr cursor)))
    (match n
      (0 (highlight-space
	  (pattern:part-at 0)
	  (document:part-at item-index)
	  cursor
	  bindings))
      (1 ...)
      (2 ...)
      (n ...))))

(define (matches pattern
		 subject
		 bindings ::Bindings)
  ::(maybe (maps (Object) to: Object))
  (otherwise #!null
    (match pattern
      (enchanted::Enchanted
       (matches (enchanted:value) subject bindings))
      (`(,,@(is _ match/equal? 'unquote) ,identifier)
       (cond
	((and (isnt identifier Atom?)
	      (isnt identifier symbol?))
	 (and (match/equal? identifier subject)
	      bindings))
	((is identifier match/equal? '_)
	 bindings)
	((is bindings overridden-at? (identifier:toString))
	 (and (match/equal? (bindings (identifier:toString))
			    subject)
	      bindings))
	(else
	 (set! (bindings (identifier:toString)) subject)
	 bindings)))

      (`(,head-pattern . ,tail-pattern)
       (and-let* ((`(,head-subject . ,tail-subject) subject)
		  (bindings* (matches head-pattern
				      head-subject
				      bindings)))
	 (matches tail-pattern tail-subject bindings*)))
      (_
       (and (match/equal? pattern subject)
	    bindings)))))

(define (highlight-space pattern ::Space
			 subject ::Space
			 context ::Cursor
			 bindings ::Bindings)
  ::(maybe Highlight)
  (otherwise #!null
    (let ((n (count (isnt _ integer?) pattern:fragments)))
      (if (= n 0)
	  (Highlight start: (recons
			     (subject:first-index)
			     context)
		     end: (recons
			   (subject:last-index)
			   context))
	  (and-let* ((m (count (isnt _ integer?) subject:fragments))
		     ((is n <= m))
		     (`(,pattern-comment . ,pattern-fragments)
		      pattern-index
		      (space-fragment-comment+index
		       pattern:fragments))
		     (`(,subject-comment . ,subject-fragments)
		      subject-index 
		      (space-fragment-comment+index
		       subject:fragments)))
	    (if (= n 1)
		(highlight-comment-infix
		 pattern-comment
		 subject-comment
		 (recons subject-index context)
		 bindings)
		(and-let* ((start ::Cursor
				  (comment-suffix-start
				   pattern-comment
				   subject-comment
				   (recons subject-index
					   context)
				   bindings)))
		  (let loop ((subpattern 1)
			     (index subject-index)
			     (pattern-fragments pattern-fragments)
			     (subject-fragments subject-fragments))
		    (if (is subpattern >= (- n 1)) 
			(and-let* ((`(,pattern-comment
				      . ,pattern-fragments)
				    pattern-index
				    (space-fragment-comment+index
				     pattern-fragments))
				   (`(,subject-comment
				      . ,subject-fragments)
				    subject-index 
				    (space-fragment-comment+index
				     subject-fragments))
				   (end ::Cursor
					(comment-suffix-start
					 pattern-comment
					 subject-comment
					 (recons (+ index
						    subject-index)
						 context)
					 bindings)))
			  (Highlight start: start
				     end: end))
			(and-let* ((`(,pattern-comment
				      . ,pattern-fragments)
				    pattern-index
				    (space-fragment-comment+index
				     pattern-fragments))
				   (`(,subject-comment
				      . ,subject-fragments)
				    subject-index 
				    (space-fragment-comment+index
				     subject-fragments))
				   ((comment-matches
				     pattern-comment
				     subject-comment
				     bindings)))
			  (loop (+ subpattern 1)
				(+ index subject-index)
				pattern-fragments
				subject-fragments)))))))))))

(define (highlight-comment-infix pattern ::Comment
				 subject ::Comment
				 context ::Cursor
				 bindings ::Bindings)
  ::(maybe Highlight)
  (otherwise #!null 
    (and-let* ((pattern ::TextualComment)
	       (subject ::TextualComment)
	       (start ::int (infix-start pattern
					 subject))
	       (end ::int (+ start (pattern:text-length))))
      (Highlight start: (recons start context)
		 end: (recons end context)))
    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment)
	       (index ::Index (pattern:first-index)))
      (highlight-infix pattern:expression
		       subject:expression
		       (recons (pattern:first-index)
			       context)
		       bindings))))

(define (highlight-infix pattern ::Tile
			 subject ::Tile
			 context ::Cursor
			 bindings ::Bindings)
  ::(maybe Highlight)
  (otherwise #!null
    (match-highlight pattern subject
		     context bindings)
    (and-let* ((pattern ::Textual)
	       (subject ::Textual)
	       (start ::int (infix-start pattern
					 subject))
	       (end ::int (+ start (pattern:text-length))))
      (Highlight start: (recons start context)
		 end: (recons end context)))))

(define (comment-matches pattern ::Comment
			 subject ::Comment
			 bindings ::Bindings)
  ::(maybe Bindings)
  (otherwise #!null
    (and-let* ((pattern ::TextualComment)
	       (subject ::TextualComment)
	       ((textual=? pattern subject)))
      bindings)
    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment))
      (matches pattern:expression
	       subject:expression
	       bindings))))

(define (comment-suffix-start pattern ::Comment
			      subject ::Comment
			      context ::Cursor
			      bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and-let* ((pattern ::TextualComment)
	       (subject ::TextualComment)
	       (start ::int (suffix-start pattern subject)))
      (recons start context))

    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment))
      (match-suffix-start pattern:expression
			  subject:expression
			  (recons (subject:first-index)
				  context)
			  bindings))))

(define (comment-prefix-end pattern ::Comment
			    subject ::Comment
			    context ::Cursor
			    bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and-let* ((pattern ::TextualComment)
	       (subject ::TextualComment)
	       (end ::int (prefix-end pattern subject)))
      (recons end context))
    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment))
      (match-prefix-end pattern:expression
			subject:expression
			(recons (subject:first-index)
				context)
			bindings))))

(define (match-prefix-end pattern ::Tile
			  subject ::Tile
			  context ::Cursor
			  bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and (matches pattern subject bindings)
	 (recons (subject:last-index) context))
    (and-let* ((pattern ::Textual)
	       (subject ::Textual)
	       (end ::int (prefix-end pattern subject)))
      (recons end context))))

(define (match-suffix-start pattern ::Tile
			    subject ::Tile
			    context ::Cursor
			    bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and (matches pattern subject bindings)
	 (recons (subject:first-index) context))
    (and-let* ((pattern ::Textual)
	       (subject ::Textual)
	       (start ::int (suffix-start pattern subject)))
      (recons start context))))
