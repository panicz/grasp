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
	      (move-cursor-left!
	       selection: (if (key:shift-down?)
			      SelectionAction:resize
			      SelectionAction:discard)))
	     (continue))
	    
	    (,KeyType:ArrowRight
	     (safely
	      (move-cursor-right!
	       selection: (if (key:shift-down?)
			      SelectionAction:resize
			      SelectionAction:discard)))
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
		   (insert-character! c)
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
