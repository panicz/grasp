(module-name (editor types extensions gutenbergson))

(import (kawa regex))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language define-cache))
(import (language match))
(import (language for))
(import (language while))
(import (utils functions))
(import (language fundamental))
(import (language assert))
(import (language define-cache))
(import (language define-parameter))
(import (editor interfaces elements))
(import (editor interfaces painting))
(import (editor types primitive))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor input transforms))
;(import (editor input screen))

(import (editor types extensions extensions))
;;(import (editor types extensions combinators))

(import (utils print))

(define-interface TextDivisionUnit (#;Enchanted)
  (render! max-line-width::real line-height::real)::real
  (height max-line-width::real line-height::real)::real
  )

(define-private opening-brackets ::(set-of gnu.text.Char)
  (set #\( #\[ #\{))

(define-private closing-brackets ::(set-of gnu.text.Char)
  (set #\) #\] #\}))

(define-private punctuation ::(set-of gnu.text.Char)
  (set #\; #\. #\: #\, #\? #\!))

(define-cache (char->string single-char ::gnu.text.Char)
  ::string
  (list->string (list single-char)))

(define (read-words #!optional (input-port ::InputPort (current-input-port)))
  ::(list-of Word)
  (let ((c (read-char input-port)))
    (cond 
      ((eof-object? c)
       '())
       
      ((char-whitespace? c)
       (read-words input-port))
       
      ((or (is c in punctuation)
	   (is c in opening-brackets)
	   (is c in closing-brackets))
       (cons (char->string c) (read-words input-port)))
      
      (else
       (let ((word (call-with-output-string
                     (lambda (p)
                       (write-char c p)
                       (let loop ()
                         (let ((c (peek-char input-port)))
                            (unless (or (eof-object? c)
                                        (char-whitespace? c)
					(is c in punctuation)
					(is c in opening-brackets)
					(is c in closing-brackets))
                              (write-char (read-char input-port) p)
                              (loop))))))))
	 (cons word (read-words input-port)))))))

(define-object (Paragraph content::(sequence-of
				    (either
				     Word
				     TextStyle
				     EndTextStyle)))
  ::TextDivisionUnit
  (define (layout-words word-operation::(maps (real real Word TextDecoration)
					      to: void)
			max-line-width::real
			line-height::real)
    ::real
    (let ((style ::TextDecoration (RegularText))
	  (left ::real 0)
	  (top ::real 0))
      (for token in content
	(match token
	  (word::Word
	   (let* ((word-length ::int (string-length word))
		  (word-width ::real (painter:styled-text-width word style))
		  (space-width ::real (painter:styled-text-width " " style))
		  (expanded ::real (+ left word-width)))

	     (cond
	      ((and (= word-length 1)
		    (or (is (word 0) in punctuation)
			(is (word 0) in closing-brackets))
		    (is left > space-width))
	       (set! left (- left space-width)))
	      
	      ((is expanded > max-line-width)
	       (set! left 0)
	       (set! top (+ top line-height))))
	     
	     (word-operation left top word style)

	     (set! left (+ left word-width
			   (if (and (= word-length 1)
				    (is (word 0) in opening-brackets))
			       0
			       space-width)))))
	  
	  (modifier::TextStyle
	   (assert (not (style:contains modifier)))
	   (style:add modifier))
	  
	  ((EndTextStyle style: modifier)
	   (assert (style:contains modifier))
	   (style:remove modifier))))
      
      (+ top line-height)))

  (define (render! max-line-width::real
		   line-height::real)
    ::real
    (layout-words (lambda (left::real top::real word::Word
				      style::TextDecoration)
		    (painter:draw-styled-text! left top word style))
		  max-line-width
		  line-height))
  
  (define cached-height
    (cache (max-line-width::real line-height::real)
	   (layout-words nothing max-line-width line-height)))
  
  (define (height max-line-width::real line-height::real)::real
    (cached-height max-line-width line-height))

  #|
  (define (draw! context::Cursor)::void
    ...)

  (define (extent)::Extent
    ...)
  
  (Magic)
  |#
  )

(define-syntax-rule (match/regex subject (pattern . actions) ...)
  (cond
   ((regex-match pattern subject) . actions)
   ...))

(define (extract-style-modifiers word::Word)::(sequence-of
					       (either
						Word
						TextStyle
						EndTextStyle))
  (match/regex
   word
   ("^[*](.+)$"
    => (fn (_ word*)
	   `(,TextStyle:Bold . ,(extract-style-modifiers word*))))
   ("^[/](.+)$"
    => (fn (_ word*)
	   `(,TextStyle:Italic . ,(extract-style-modifiers word*))))
   ("^[~](.+)$"
    => (fn (_ word*) 
	   `(,TextStyle:Monospace . ,(extract-style-modifiers word*))))
   
   ("^(.+)[*]$"
    => (fn (_ word*)
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Bold))))

   ("^(.+)[/]$"
    => (fn (_ word*)
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Italic))))

   ("^(.+)[~]$"
    => (fn (_ word*)
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Monospace))))

   (".*"
    `(,word))))

(define (parse-paragraph #!optional (input ::InputPort (current-input-port)))
  ::(sequence-of (either Word TextStyle EndTextStyle))
  (let ((words (read-words input)))
    (append-map extract-style-modifiers words)))

(define-type (Chapter content: (sequence-of TextDivisionUnit)))

(define-type (Book chapters: (sequence-of Chapter)))

(define-object (InteractiveBookReader book ::Book)
  ::Maximizable

  (define current-chapter ::int 0)
  
  (define chapter-scroll ::(sequence-of real)
    ((array-of real) length: (length book:chapters)))
  
  (define (draw! context::Cursor)::void
    (let ((chapter ::Chapter (book:chapters current-chapter))
	  (top ::real 0))
      (escape-with break
	(for fragment::TextDivisionUnit in chapter:content
	  (let ((height
		 (with-translation (0 top)
		   (fragment:render! size:width
				     (painter:styled-text-height)))))
	    (set! top (+ top height))
	    (when (is top > size:height)
	      (break)))))))

  (define size ::Extent
    (Extent width: (* 60 (painter:space-width))
	    height: (* 25 (painter:styled-text-height))))
  
  (define (set-size! width::real height::real anchor::ResizeAnchor)::void
    (set! size:width width)
    (set! size:height height))

  (define (extent)::Extent size)

  (define (value)::Object
    (cons (Atom "InteractiveBookReader") (empty)))
  
  (MaximizableWidget))

(set! (extension 'InteractiveBookReader)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as InteractiveBookReader (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create InteractiveBookReader from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
