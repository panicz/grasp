(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
 (define-object)
 (extent)
 (conversions)
 (indexable)
 (space)
 (cursor)
 (primitive)
 (fundamental)
 (indexable)
 (extent)
 (text-painter)
 (combinators)
 (parse)
 (examples)
 (assert)
 (infix)
 (match)
 (term)
 (functions)
 (print)
 (painter)
 (for)
 (document-operations)
 (editor-operations)
 (interactive)
 (extension)
 (button)
 )

(define (/old/insert-character! c::char)::void
  (and-let* ((`(,tip . ,stem) (the-cursor))
	     (`(,top . ,root) stem)
	     (parent (the-expression at: root))
	     (target (part-at top parent)))
    (cond
     ((is c memq '(#\[ #\( #\{))
      (cond
       ((is target instance? Space)
	(insert! (cons '() '()) at: root #;(cdr (the-cursor)))
	(set! (the-cursor)
	      (recons* 0 0 (+ (car (cdr (the-cursor))) 1)
		       (cdr (cdr (the-cursor))))))
       ((eqv? (car (the-cursor)) #\])
	(set! (the-cursor)
	      (recons #\[ (cdr (the-cursor)))))

       (else
	(let ((target (extract! at: (cdr (the-cursor)))))
	  (insert! (cons target '()) at: (cdr (the-cursor)))
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
	(insert! head/tail-separator at: (cdr (the-cursor)))
	(times 2 move-cursor-right!))
       
       (else
	(let* ((space-after (split-space!
			     target
			     (car (the-cursor)))))
	  (insert! (cons (Atom (list->string (list c))) '())
		   at: (cdr (the-cursor)))
	  (set! (the-cursor)
	    (recons* 1 (+ (car (cdr (the-cursor))) 1)
		     (cdr (cdr (the-cursor)))))))))
     )))


;;(import (button))

(define input ::string "\
(define (! n)
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
")

(set! (the-document)
  (with-input-from-string input parse-document))

(define input-extent ::Extent (string-extent input))

(define-object (editor-message-handler size::int)::MessageHandler
  
  (define (display-messages output::Object)::void
    (let ((io ::Terminal (as Terminal output)))
      (for message in messages
	   (io:putString message))))
  
  (logger size))


(define (run-editor #!optional
		    (io ::Terminal (make-terminal)))
  ::void
  (io:enterPrivateMode)
  (when (instance? io ExtendedTerminal)
    (invoke (as ExtendedTerminal io)
	    'setMouseCaptureMode
	    MouseCaptureMode:CLICK_RELEASE_DRAG))

  (parameterize ((current-message-handler (editor-message-handler 4))
		 (the-painter (TextPainter))
		 (current-display-procedure
		  (lambda (message)
		    (io:putString
		     (with-output-to-string
		       (lambda ()
			 (display
			  message)))))))
          
    (let continue ()
      (let ((output-extent ::Extent
			   (extent (head (the-document)))))
	((the-painter):clear!)
	(safely (draw-document! (the-document)))
	(io:setCursorVisible #f)
	(io:clearScreen)
	(io:setCursorPosition 0 0)
	(io:putString ((the-painter):toString))
	(io:setCursorPosition
	 0
	 (+ 2 output-extent:height))
	(safely
	 (io:putString (with-output-to-string
			 (lambda ()
			   (write (the-cursor))
			   (write (the-selection-anchor))
			   (write (the-expression))))))
	(invoke (current-message-handler)
		'display-messages io)
	(io:flush)
	(let ((cursor-position (invoke (the-painter)
				       'cursor-position)))
	  (io:setCursorPosition cursor-position:left
				(+ cursor-position:top 1)))
	(io:setCursorVisible #t)
	(let* ((key ::KeyStroke (io:readInput))
	       (type ::KeyType (key:getKeyType)))
	  (match type	
	    (,KeyType:ArrowLeft
	     (safely
	      (if (key:shift-down?)
		  (expand-selection-left!)
		  (move-cursor-left!)))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (safely
	      (if (key:shift-down?)
		  (expand-selection-right!)
		  (move-cursor-right!)))
	     (continue))
	    	    
	    (,KeyType:EOF
	     (values))

	    (,KeyType:Character
	     
	     (let* ((code::char
		     ((key:getCharacter):charValue))
		    (c::gnu.text.Char (gnu.text.Char
				       code)))
	       (if (and (key:ctrl-down?)
			(eq? c #\space))
		   (invoke (current-message-handler)
			   'clear-messages!)
		   (/old/insert-character! c)
		   )
	       (continue)))

	    (,KeyType:Tab
	     (safely (enchant-expression!))
	     (continue))
	    
	    (,KeyType:Delete
	     (safely (delete-forward!))
	     (continue))

	    (,KeyType:Enter
	     (insert-character! #\newline)
	     (move-cursor-right!)
	     (continue))
	    
	    (,KeyType:Backspace
	     (safely (delete-backward!))

	     (continue))
	    
	    (,KeyType:MouseEvent
	     (let* ((action ::MouseAction
			    (as MouseAction key))
		    (position ::TerminalPosition
			      (action:getPosition))
		    (left (position:getColumn))
		    (top (position:getRow)))
	       (cond
		((action:isMouseMove)
		 (WARN "mouse move to "
		       (position:toString)))
		((action:isMouseDown)
		 (match (action:getButton)
		   (,MouseButton:Left
		    (WARN "cursor: " (the-cursor)))
		   (,MouseButton:Right
		    (WARN "right mouse at "
			  (position:toString)))
		   (_
		    (WARN (action:toString)
			  " mouse at "
			  (position:toString)))))
		((action:isMouseDrag)
		 (WARN "mouse move to "
		       (position:toString)))
		((action:isMouseUp)
		 (let ((target (the-expression)))
		   (when (is target Interactive?)
		     (invoke (as Interactive target)
			     'tapped left top)))
		 
		 )
		))
	     (continue))
	    
	    (_
	     (WARN "key: "key)
	     (continue))
	    )
	  )))
    (io:exitPrivateMode)
    (io:close)))

(run-editor)
