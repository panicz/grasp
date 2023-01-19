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
     ((is target instance? Atom)
      (cond ((is 0 <= position < (atom-length target))
	     (delete-char! target position)
	     (when (= (atom-length target) 0)
	       (take-cell! at: (cdr (the-cursor)))
	       (set! (the-cursor)
		     (cursor-climb-back
		      (recons (- (car (cdr (the-cursor)))
				 1)
			      (cdr (cdr (the-cursor))))))))))
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
	     (take-cell!)
	     (set! (the-cursor) new-cursor)))
	  (else
	   (delete! (car (the-cursor)))))))

(define (delete-backward!)::void
  (let ((target (the-expression)))
    (cond ((and (pair? target)
		(eqv? (car (the-cursor)) (last-index target)))
	   (let ((new-cursor (cursor-climb-back
			      (cursor-back (cdr (the-cursor))))))
	     (take-cell!)
	     (set! (the-cursor) new-cursor)))
	  (else
	   (set! (the-cursor)
		 (cursor-climb-back (cursor-back)))
	   (delete! (car (the-cursor)))))))

(define (insert-character! c::char)::void
  (and-let* ((`(,tip . ,stem) (the-cursor))
	     (`(,top . ,root) stem)
	     (parent (the-expression at: root))
	     (target (part-at top parent)))
    (cond
     ((is c memq '(#\[ #\( #\{))
      (cond
       ((is target instance? Space)
	(splice! (cons '() '()) at: root #;(cdr (the-cursor)))
	(set! (the-cursor)
	      (recons* 0 0 (+ (car (cdr (the-cursor))) 1)
		       (cdr (cdr (the-cursor))))))
       ((eqv? (car (the-cursor)) #\])
	(set! (the-cursor)
	      (recons #\[ (cdr (the-cursor)))))

       (else
	(let ((target (take-cell! at: (cdr (the-cursor)))))
	  (splice! (cons target '()) at: (cdr (the-cursor)))
	  (set! (the-cursor)
		(recons #\[ (cdr (the-cursor))))))))

     ((is c memq '(#\] #\) #\}))
      (set! (the-cursor)
	    (recons #\] root)))
     
     ((is target instance? Atom)
      (cond
       ((or (eq? c #\space) (eq? c #\newline))
	(cond ((eqv? (car (the-cursor)) (first-index target))
	       (let ((preceding-space (part-at
				       (previous-index
					top parent)
				       parent)))
		 (insert-whitespace! c preceding-space
				     (last-index
				      preceding-space))))
	      ((eqv? (car (the-cursor)) (last-index target))
	       (let ((following-space (part-at
				       (next-index
					top parent)
				       parent)))
		 (insert-whitespace! c following-space
				     (first-index
				      following-space))
		 (move-cursor-right!)))
	      (else
	       (let* ((suffix (atom-subpart target tip))
		      (owner (drop (quotient top 2) parent))
		      (cell (cons suffix (cdr owner))))
		 (truncate-atom! target tip)
		 (set! (cdr owner) cell)
		 (set! (post-head-space cell)
		   (post-head-space owner))
		 (set! (post-head-space owner)
		   (Space fragments: (if (eq? c #\newline)
					 (cons* 0 0 '())
					 (cons 1 '()))))
		 (move-cursor-right!)))))
       
	 (else
	  (insert-char! c target (car (the-cursor)))
	  (set! (the-cursor)
	    (recons (+ (car (the-cursor)) 1)
		    (cdr (the-cursor)))))))
     ((is target instance? Space)

      (cond
       ((is c memq '(#\space #\newline))
	(insert-whitespace! c target (car (the-cursor)))
	(set! (the-cursor)
	      (recons (+ (car (the-cursor)) 1)
		      (cdr (the-cursor))))
	)

       ((is c memq '(#\. #\|))
	(splice! head/tail-separator at: (cdr (the-cursor)))
	(times 2 move-cursor-right!))
       
       (else
	(let* ((space-after (split-space!
			     target
			     (car (the-cursor)))))
	  (splice! (cons (Atom (list->string (list c))) '())
		   at: (cdr (the-cursor)))
	  (set! (the-cursor)
	    (recons* 1 (+ (car (cdr (the-cursor))) 1)
		     (cdr (cdr (the-cursor)))))))))
     )))


