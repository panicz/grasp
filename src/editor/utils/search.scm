(module-name (editor utils search))

(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language infix))
(import (language match))
(import (language keyword-arguments))
(import (language while))
(import (language for))
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

#|
The search algorithm is conceptually not difficult,
but because of the representation for the document
that I chose for GRASP, its implementation is very
convoluted (and I'm so sorry).
|#

(define-mapping (search-bindings key ::String)
  ::(maybe Tile)
  #!null)

(define-alias Bindings (!maps (String) to: (maybe Tile)))

(define (match-spaces pattern ::Space
		      subject ::Space
		      bindings ::Bindings)
  ::(maybe Bindings)
  (let ((n (count (isnt _ number?) pattern:fragments)))
    (if (is n <= 0)
	bindings
	(let loop ((subpattern 1)
		   (bindings bindings)
		   (pattern-fragments pattern:fragments)
		   (subject-fragments subject:fragments))
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
		     (bindings (comment-matches
				pattern-comment
				subject-comment
				bindings)))
	    (if (is subpattern >= n)
		bindings
		(loop (+ subpattern 1)
		      bindings
		      pattern-fragments
		      subject-fragments)))))))

(define (matches-pair pattern ::cons
		      subject ::cons
		      bindings ::Bindings)
  ::(maybe Bindings)
  (otherwise #!null
    (and-let* ((`(,pattern-head . ,pattern-tail) pattern)
	       (`(,subject-head . ,subject-tail) subject)
	       (bindings (matches pattern-head subject-head
				  bindings))
	       (bindings (match-spaces
			    (post-head-space pattern)
			    (post-head-space subject)
			    bindings)))
      (cond
       ((and (dotted? pattern)
	     (dotted? subject))
	(and-let* ((bindings (match-spaces
			      (pre-tail-space pattern)
			      (pre-tail-space subject)
			      bindings))
		   (bindings (match-spaces
			      (post-tail-space pattern)
			      (post-tail-space subject)
			      bindings)))
	  (matches pattern-tail subject-tail bindings)))
       ((and (pair? pattern-tail)
	     (pair? subject-tail))
	(matches-pair pattern-tail subject-tail bindings))
       (else
	(matches pattern-tail subject-tail bindings))))))

(define (matches pattern ::Tile
		 subject ::Tile
		 bindings ::Bindings)
  ::(maybe Bindings)
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
		  (bindings* (match-spaces
			      (pre-head-space pattern)
			      (pre-head-space subject)
			      bindings)))
	 (matches-pair pattern subject bindings*)))
      ('()
       (or
	(and-let* ((pattern ::EmptyListProxy)
		   (subject ::EmptyListProxy))
	  (match-spaces pattern:space
			subject:space
			bindings))
	(and (empty? subject)
	     bindings)))
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
			     (bindings bindings)
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
					(comment-prefix-end
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
				   (bindings
				    (comment-matches
				     pattern-comment
				     subject-comment
				     bindings)))
			  (loop (+ subpattern 1)
				bindings
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
	       (start ::integer (infix-start pattern
					     subject))
	       (end ::integer (+ start (pattern:text-length))))
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

(define (highlight-pattern pattern ::Tile
			   subject ::Tile
			   cursor ::Cursor
			   bindings ::Bindings)
  ::(maybe Highlight)
  (and-let* ((`(,initial-index . ,context) cursor)
	     (bindings (matches pattern subject bindings))
	     (last-pattern-index (pattern:last-index)))
    (let loop ((pattern-index (pattern:next-index
			       (pattern:first-index)))
	       (current-index initial-index))
      (let ((next-pattern-index (pattern:next-index pattern-index)))
	(if (eqv? next-pattern-index last-pattern-index)
	    (Highlight start: cursor ;;powinnismy zrobic
		       ;; climb-front/back na wynik
		       end: (recons current-index context))
	    (loop next-pattern-index
		  (subject:next-index current-index)))))))

(define (highlight-infix pattern ::Tile
			 subject ::Tile
			 context ::Cursor
			 bindings ::Bindings)
  ::(maybe Highlight)
  (otherwise #!null
    (highlight-pattern pattern subject
		       context bindings)
    (and-let* ((pattern ::Textual)
	       (subject ::Textual)
	       (start ::integer (infix-start pattern
					     subject))
	       (end ::integer (+ start (pattern:text-length))))
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
	       (start ::integer (suffix-start pattern subject)))
      (recons start context))
    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment))
      (match-suffix-start pattern:expression
			  subject:expression
			  (recons (subject:first-index)
				  context)
			  bindings))))

(define (space-fragments-match pattern-fragments ::list
			       subject-fragments ::list
			       bindings ::Bindings
			       #!optional (limit ::number +inf.0))
  ::(maybe Bindings)
  (let* ((pattern-fragments (drop-while integer? pattern-fragments))
	 (subject-fragments (drop-while integer? subject-fragments)))
    (otherwise #!null
      (if (or (is limit <= 0)
	      (and (null? pattern-fragments)
		   (null? subject-fragments)))
	  bindings
	  (and-let* ((`(,comment-pattern::Comment . ,tail-pattern) pattern-fragments)
		     (`(,comment-subject::Comment . ,tail-subject) subject-fragments)
		     (bindings (comment-matches comment-pattern comment-subject bindings)))
	    (space-fragments-match tail-pattern tail-subject bindings
				   (- limit 1)))))))

(define (space-suffix-start pattern ::Space
			    subject ::Space
			    context ::Cursor
			    bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and-let* ((n (count (isnt _ integer?) pattern:fragments))
	       (m (count (isnt _ integer?) subject:fragments))
	       ((is m >= n))
	       (`(,comment-pattern::Comment . ,rest-pattern)
		(drop-while (isnt _ Comment?) pattern:fragments))
	       (`(,comment-subject::Comment . ,rest-subject) skipped
		(drop-comment-fragments
		 (- m n) subject:fragments))
	       (bindings (space-fragments-match rest-pattern
						rest-subject
						bindings)))
      (comment-suffix-start comment-pattern
			    comment-subject
			    (recons skipped context)
			    bindings))))

(define (comment-prefix-end pattern ::Comment
			    subject ::Comment
			    context ::Cursor
			    bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and-let* ((pattern ::TextualComment)
	       (subject ::TextualComment)
	       (end ::integer (prefix-end pattern subject)))
      (recons end context))
    (and-let* ((pattern ::ExpressionComment)
	       (subject ::ExpressionComment))
      (match-prefix-end pattern:expression
			subject:expression
			(recons (subject:first-index)
				context)
			bindings))))

(define (space-prefix-end pattern ::Space
			  subject ::Space
			  context ::Cursor
			  bindings ::Bindings)
  ::(maybe Cursor)
  (otherwise #!null
    (and-let* ((n (count (isnt _ integer?) pattern:fragments))
	       (m (count (isnt _ integer?) subject:fragments))
	       ((is m >= n))
	       (bindings (space-fragments-match pattern:fragments
						subject:fragments
						bindings
						(- n 1)))
	       (`(,comment-pattern::Comment . ,_) _
		(drop-comment-fragments (- n 1) pattern:fragments))
	       (`(,comment-subject::Comment . ,_) index
		(drop-comment-fragments (- n 1) subject:fragments)))
      (comment-prefix-end comment-pattern
			  comment-subject
			  (recons index context)
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
	       (end ::integer (prefix-end pattern subject)))
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
	       (start ::integer (suffix-start pattern subject)))
      (recons start context))))

(define (match-highlight pattern ::Tile
			 subject ::Tile
			 cursor ::Cursor
			 bindings ::Bindings)
  ::(maybe Highlight)
  (or 
   (and-let* ((subject ::Enchanted))
     (match-highlight pattern (subject:value) cursor bindings))
   (and-let* ((pattern ::Enchanted))
     (match-highlight (pattern:value) subject cursor bindings))   
   (and-let* ((pattern-index (pattern:next-index
			      (pattern:first-index)))
	      (`(,subject-index . ,context) cursor))
     (match pattern
       ('()
	(and-let* ((space-pattern ::Space (pattern:part-at
					   pattern-index))
		   (space-subject ::Space (subject:part-at
					   subject-index)))
	  (highlight-space space-pattern space-subject
			   cursor bindings)))
       (`(,single)
	(let* ((pattern-opening-space ::Space
				      (pre-head-space pattern))
	       (pattern-closing-space ::Space
				      (post-head-space pattern))
	       (opening-whitespace? ::boolean
				    (whitespace?
				     pattern-opening-space))
	       (closing-whitespace? ::boolean
				    (whitespace?
				     pattern-closing-space)))
	  (cond
	   ((and opening-whitespace? closing-whitespace?)
	    (and-let* ((tile-index (subject:next-index
				    subject-index))
		       (tile ::Tile
			     (subject:part-at tile-index))
		       ((isnt tile eq? subject)))
	      
	      (or (and-let* ((single ::Textual)
			     (tile ::Textual)
			     (index ::integer (infix-start single
							   tile)))
		    (Highlight start: (recons* index tile-index
					       context)
			       end: (recons* (+ index
						(single:text-length))
					     tile-index
					     context)))
		  (and (matches single tile bindings)
		       (Highlight start: (recons*
					  (pattern:first-index)
					  tile-index
					  context)
				  end: (recons*
					(pattern:last-index)
					tile-index
					context))))))
	   (opening-whitespace?
	    (and-let* ((subject-opening-space::Space
			(subject:part-at subject-index))
		       (start ::Cursor
			      (space-suffix-start
			       pattern-opening-space
			       subject-opening-space
			       cursor
			       bindings))
		       (closing-index (subject:next-index
				       subject-index))
		       (subject-closing-tile::Tile
			(subject:part-at closing-index))
		       (end ::Cursor
			    (match-prefix-end
			     single
			     subject-closing-tile
			     (recons closing-index context)
			     bindings)))
	      (Highlight start: start end: end)))
	    (closing-whitespace?
	     (and-let* ((item-index (subject:next-index
				     subject-index))
			(start ::Cursor
			       (match-suffix-start
				single
				(subject:part-at item-index)
				(recons item-index context)
				bindings))
			(closing-index (subject:next-index
					item-index))
			(subject-closing-space::Space
			 (subject:part-at closing-index))
			(end ::Cursor
			     (space-prefix-end
			      pattern-closing-space
			      subject-closing-space
			      (recons closing-index context)
			      bindings)))
	       (Highlight start: start end: end)))
	    (else
	     (and-let* ((start ::Cursor
			       (space-suffix-start
				pattern-opening-space
				(subject:part-at subject-index)
				cursor
				bindings))
			(closing-index (subject:next-index
					(subject:next-index
					 subject-index)))
			(end ::Cursor
			     (space-prefix-end
			      pattern-closing-space
			      (subject:part-at
			       closing-index)
			      (recons closing-index context)
			      bindings)))
	       (Highlight start: start end: end))))))
       (`(,first-pattern . ,rest-pattern)
	 (and-let* ((pattern-opening-space ::Space
					   (pre-head-space
					    pattern))
		    (subject-opening-space ::Space
					   (subject:part-at
					    subject-index))
		    (subject-tile-index (subject:next-index
					 subject-index))
		    (start ::Cursor
			   (if (whitespace?
				pattern-opening-space)
			       (match-suffix-start
				first-pattern
				(subject:part-at
				 subject-tile-index)
				(recons subject-tile-index
					context)
				bindings)
			       (space-suffix-start
				pattern-opening-space
				subject-opening-space
				cursor
				bindings)))
		    ((integer? subject-index))
		    ((is subject-index >= 0))
		    ((even? subject-index)))
	   (let loop ((front-pattern rest-pattern)
		      (front-subject (drop (+ (/ subject-index 2) 1)
					   subject))
		      (index (subject:next-index
			      (subject:next-index
			       subject-tile-index)))
		      (bindings bindings))
	     (and-let* ((`(,head-pattern . ,tail-pattern)
			 front-pattern)
			(`(,head-subject . ,tail-subject)
			 front-subject))
	       (if (isnt tail-pattern pair?)
		   (and-let* ((pattern-closing-space ::Space
						     (post-head-space
						      front-pattern))
			      (subject-closing-space ::Space
						     (post-head-space
						      front-subject))
			      (end (if (whitespace?
					pattern-closing-space)
				       (match-prefix-end
					head-pattern
					head-subject
					(recons index context)
					bindings)
				       (space-prefix-end
					pattern-closing-space
					subject-closing-space
					(recons (subject:next-index
						 index))
					bindings))))
		     (Highlight start: start
				end: end))
		   (and-let* ((bindings (matches
					 head-pattern
					 head-subject
					 bindings))
			      (bindings (match-spaces
					 (post-head-space
					  front-pattern)
					 (post-head-space
					  front-subject)
					 bindings)))
		     (loop tail-pattern
			   tail-subject
			   (subject:next-index
			    (subject:next-index index))
			   bindings)))))))))
   #!null))

(define/kw (next-match of: pattern ::Tile
		       in: document ::Tile := (as Tile (the-document))
		       after: cursor ::Cursor := (the-cursor)
		       context: context ::Cursor := '())
  ::(maybe Highlight)
  #;(print "looking for "pattern" in "document
	 " after "cursor" at "context)
  (escape-with return
    (let* ((current-level ::int (length context))
	   (reference-level ::int (length cursor))
	   (pressure (- reference-level current-level 1))
	   (limit (document:last-index)))
      
      (let loop ((index (if (and (is pressure >= 0)
				 (equal? context (drop (+ pressure 1) cursor)))
			    (let ((initial-index (cursor
						  pressure)))
			      (if (= pressure 0)
				  (document:next-index
				   initial-index)
				  initial-index))
			    (document:next-index
			     (document:first-index)))))
	(reset! search-bindings)
	(let ((item (document:part-at index)))
	  (unless (eq? item document)
	    (and-let* ((result ::Highlight (match-highlight
					    pattern
					    (as Tile document)
					    (recons index context)
					    search-bindings)))
	      (return result))
	    (when (gnu.lists.LList? item)
	      (let ((result (next-match of: pattern
					in: item
					after: cursor
					context: (recons
						  index
						  context))))
		(when result
		  (return result))))))
	(unless (is index eqv? limit)
	  (loop (document:next-index index))))
      (return #!null))))

(define/kw (all-matches of: pattern
			in: document := (the-document)
			after: cursor := '())
  ::(list-of Highlight)
  (match (next-match of: pattern in: document
		     after: cursor)
    (#!null '())
    (highlight::Highlight
     (let ((result `(,highlight)))
       (let loop ((cursor highlight:start)
		  (cone result))
	 (match (next-match of: pattern in: document
			    after: cursor)
	   (#!null
	    result)
	   (next::Highlight
	    (set-cdr! cone `(,next . ,(cdr cone)))
	    (loop next:start
		  (cdr cone)))))))))
