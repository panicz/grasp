(module-name (editor document parse))

(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-object))
(import (utils hash-table))
(import (language define-property))
(import (language define-type))
(import (language keyword-arguments))
(import (language fundamental))
(import (language define-cache))
(import (srfi :11))
(import (srfi :17))
(import (utils conversions))
(import (utils functions))
(import (editor types primitive))
(import (language infix))
(import (language examples))
(import (editor interfaces elements))
(import (editor types spaces))
(import (editor types texts))
(import (language match))
(import (editor types comments))
(import (editor document cursor))
(import (utils print))

(import (editor types extensions extensions))
(import (editor types extensions quotations))

(define-object (Document car source)::Tile
  ;; TODO: cursor-under* etc.

  (define (setCar value)
    (set! car value))

  (define (getCar)
    car)
  
  (define (draw! context::Cursor)::void
    (cond ((EmptyListProxy? car)
	   (let ((proxy ::EmptyListProxy
			(as EmptyListProxy car)))
	     (proxy:space:draw! (hash-cons 0 context))))
	  ((pair? car)
	   (let ((s ::Space (pre-head-space car)))
	     (s:draw! (hash-cons 0 context))
	     (draw-sequence! car)))))
  
  (cons car (empty)))

(define (separator? c)::boolean
  (or (eof-object? c)
      (char-whitespace? c)
      (memq c '(#\( #\) #\[ #\] #\"))))

(define (read-atom-chars-into last-tail::pair)::void
  (let ((c (peek-char)))
    (unless (separator? c)
      (set-cdr! last-tail (cons (read-char) '()))
      (read-atom-chars-into (tail last-tail)))))

(define (read-line-comment)::Text
  (define (read-until-newline result::Text)
    (let ((c (read-char)))
      (if (or (eof-object? c)
	      (eq? c #\newline))
	  result
	  (read-until-newline (result:appendCharacter
			       (invoke (as gnu.text.Char c)
				       'intValue))))))
  (read-until-newline (Text)))

(define (read-block-comment #!optional
			    (result ::Text (Text)))
  ::Text
  (define (add! c::gnu.text.Char)
    (result:appendCharacter (c:intValue)))

  (let ((c (read-char)))
    (cond
     ((eof-object? c) result)
     ((eq? c #\|)
      (let ((d (read-char)))
	(cond ((or (eof-object? d)
		   (eq? d #\#))
	       result)
	      (else
	       (add! c)
	       (add! d)
	       (read-block-comment result)))))
     ((eq? c #\#)
      (let ((d (read-char))) 
	(cond ((eof-object? d)
	       (add! c)
	       result)
	      ((eq? d #\|)
	       (add! c)
	       (add! d)
	       ;; we make two revursive calls here:
	       ;; one to read the nested block comment
	       (read-block-comment result)
	       (add! #\|)
	       (add! #\#)
	       ;; and another one to read the rest
	       ;; at the current level of nesting
	       (read-block-comment result))
	      (else
	       (add! c)
	       (add! d)
	       (read-block-comment result)))))
     (else
      (add! c)
      (read-block-comment result)))))

(define (read-text)::Text
  (let ((result (Text)))
    (define (add! c)
      (if (is c instance? gnu.text.Char)
	  (result:appendCharacter (invoke
				   (as gnu.text.Char c)
				   'intValue))
	  (result:appendCharacter c)))

    (define (read-hex-sequence #!optional (code 0))
      (let ((d ::char (peek-char)))
	(cond ((or (eof-object? d)
		   (isnt d char-hex-digit?))
	       (add! code))
	      (else
	       (read-char)
	       (read-hex-sequence (+ (* code 16)
				     (char-hex-value d)))))))
    (define (read-escaped)
      (let ((c ::char (read-char)))
	(cond
	 ((eof-object? c) result)
	 (else
	  (match c
	    (#\n (add! #\newline))
	    (#\t (add! #\tab))
	    (#\\ (add! #\\))
	    (#\" (add! #\"))
	    (#\r (add! #\return))
	    (#\b (add! #\backspace))
	    (#\f (add! #\page))
	    (#\x (read-hex-sequence))
	    (#\newline (values))
	    (_
	     ;;(WARN "Unrecognized escape character: "c)
	     (add! c)))
	  (read-normal)))))

    (define (read-normal)
      (let ((c ::char (read-char)))
	(cond
	 ((or (eof-object? c)
	      (eq? c #\"))
	  result)
	 ((eq? c #\\)
	  (read-escaped))
	 (else
	  (add! c)
	  (read-normal)))))

    (read-normal)))


(define (read-spaces #!optional (result::Space
				 (Space (cons 0 '()))))
  ::Space
  (define (read-spaces-into pair)
    (let ((c (peek-char)))
      (if (or (eof-object? c)
	      (and (isnt c char-whitespace?)
		   (isnt c eq? #\;)))
	  result
	  (match (read-char)
	    (#\;
	     (let ((line-comment (read-line-comment)))
	       (set-cdr! pair
			 (cons (LineComment line-comment)
			       (cons 0 (tail pair))))
	       (read-spaces-into (tail (tail pair)))))
	    (#\newline
	     (set-cdr! pair
		       (cons 0 (tail pair)))
	     (read-spaces-into (tail pair)))
	    (#\space
	     (set-car! pair
		       (+ (head pair) 1))
	     (read-spaces-into pair))
	    (_
	     (read-spaces-into pair))))))

  (read-spaces-into result:fragments))

(define (read-list #!optional (max-items +inf.0))
  ::(Values (list-of Tile) Space)
  (let* ((result (empty))
	 (growth-cone '())
	 (initial-space (read-spaces))
	 (total-items 0)
	 (last-space ::Space initial-space))

    (define (read-space)::Space
      (set! last-space (read-spaces))
      last-space)

    (define (add-element! element following-space)
      (cond ((empty? result)
	     (set! result (cons element result))
	     (set! growth-cone result))
	    (else
	     (set-cdr! growth-cone (cons element
					 (cdr growth-cone)))
	     (set! growth-cone (tail growth-cone))))
      (update! (post-head-space growth-cone)
	       following-space)
      (set! total-items (+ total-items 1)))

    (define (read-next)
      (if (is total-items >= max-items)
	  (values result initial-space)
	  (let ((c (read-char)))
	    (cond
             ((or (eof-object? c) (eq? c #\)))
              (when (pair? result)
		(update! (pre-head-space result)
			 initial-space))
              (values result initial-space))

             ((eq? c #\.)
	      (let ((d (peek-char)))
		(if (separator? d)
		    (let-values (((result* post-dot-spaces)
				  (read-list 1)))
		      (match result*
			(`(,dotted-tail)
			 (set-cdr! growth-cone dotted-tail)
			 (update! (dotted? growth-cone) #t)
			 (update! (pre-tail-space growth-cone)
				  post-dot-spaces)
			 (update! (post-tail-space growth-cone)
				  (read-space))
			 (let ((c (read-char)))
			   (assert (eq? c #\))))
			 (values result initial-space))))
		    (let ((output (cons c '())))
		      (read-atom-chars-into output)
		      (add-element! (Atom (list->string output))
				    (read-space))
		      (read-next)))))

             ((eq? c #\()
              (let-values (((result* spaces*) (read-list)))
		(add-element! (if (empty? result*)
				  (EmptyListProxy spaces*)
				  result*)
			      (read-space))
		(read-next)))

	     ((eq? c #\")
	      (let ((text (read-text)))
		(add-element! text (read-space)))
	      (read-next))

	     ((eq? c #\')
	      (let-values (((next-item spaces) (read-list 1)))
		(add-element! (Quote (car next-item))
			      (post-head-space next-item))
		(read-next)))

	     ((eq? c #\`)
	      (let-values (((next-item spaces) (read-list 1)))
		(add-element! (Quasiquote (car next-item))
			      (post-head-space next-item))
		(read-next)))

	     ((eq? c #\,)
	      (let ((d (peek-char)))
		(if (eq? d #\@)
		    (let*-values (((_) (read-char))
				  ((next-item spaces) (read-list 1)))
		      (add-element! (UnquoteSplicing (car next-item))
				    (post-head-space next-item)))
		    (let-values (((next-item spaces) (read-list 1)))
		      (add-element! (Unquote (car next-item))
				    (post-head-space next-item))))
		(read-next)))

	     ((eq? c #\#)
	      (let ((d (read-char)))
		(cond
		 ((eq? d #\;)
		  (let-values (((unexpr spaces)
				(read-list 1)))
		    (let ((coda (last-pair
				 last-space:fragments))
			  (next-space ::Space
				      (post-head-space
				       unexpr)))
		      ;;(assert (EmptySpace? spaces))
		      (set-cdr! coda
				(cons
				 (ExpressionComment (car unexpr))
				 next-space:fragments))
		      (read-next))))

		 ((eq? d #\|)
		  (let* ((comment (read-block-comment))
			 (coda (last-pair
				last-space:fragments))
			 (next-space (read-spaces)))
		    (set-cdr! coda
			      (cons
			       (BlockComment comment)
			       next-space:fragments))
		    (read-next)))

		 ((and (eq? d #\\) (memq (peek-char) '(#\( #\) #\[ #\] #\")))
		  (add-element! (Atom (list->string (cons* c d (read-char) '())))
				(read-space))
		  (read-next))
		 
		 (else
		  (let ((output (cons* c d '())))
		    (read-atom-chars-into (tail output))
		    (add-element! (Atom (list->string output))
				  (read-space))
		    (read-next))))))

             (else ;; an atom
              (let ((output (cons c '())))
		(read-atom-chars-into output)
		(add-element! (Atom (list->string output))
                              (read-space))
		(read-next)))))))

    (read-next)))

(define (parse #!optional (port (current-input-port)))::list
  (parameterize ((current-input-port port))
    (let-values (((result spaces) (read-list)))
      (if (empty? result)
	  (EmptyListProxy spaces)
	  result))))

(define (parse-string s::string)::list
  (call-with-input-string s parse))

(define (parse-document #!optional
			(port (current-input-port)))
  (Document (parse port) port))

(define (string->document s::string)::list
  (call-with-input-string s parse-document))

(define (document->string doc::list)
  (with-output-to-string (lambda () (show-document doc))))

(define (pair->string p::list)
  (with-output-to-string (lambda () (show-pair p))))
