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

(define-syntax-rule (match/regex subject (pattern . actions) ...)
  (cond
   ((regex-match pattern subject) . actions)
   ...))

(define-type (Section title: string))

(define-alias Paragraph (either
			 Section
			 string ;; verbatim
			 (sequence-of
			  (either
			   string
			   TextStyle
			   EndTextSTyle))))

(define-type (Chapter title: string
		      paragraphs: (sequence-of Paragraph) := (java.util.ArrayList)))

(define-type (Book title: string
		   chapters: (sequence-of Chapter) := (java.util.ArrayList)))

(define-private opening-brackets ::(set-of gnu.text.Char)
  (set #\( #\[ #\{))

(define-private closing-brackets ::(set-of gnu.text.Char)
  (set #\) #\] #\}))

(define-private punctuation ::(set-of gnu.text.Char)
  (set #\; #\. #\: #\, #\? #\!))

(define-private brackets&punctuation ::(set-of gnu.text.Char)
  (union opening-brackets closing-brackets punctuation))

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
       
      ((is c in brackets&punctuation)
       (cons (char->string c) (read-words input-port)))
      
      (else
       (let ((word (call-with-output-string
                     (lambda (p)
                       (write-char c p)
                       (let loop ()
                         (let ((c (peek-char input-port)))
                            (unless (or (eof-object? c)
                                        (char-whitespace? c)
					(is c in brackets&punctuation))
                              (write-char (read-char input-port) p)
                              (loop))))))))
	 (cons word (read-words input-port)))))))

(define (read-paragraphs #!optional (input-port ::InputPort (current-input-port)))
  ::(sequence-of string)
  (let ((paragraphs ::java.util.List (java.util.ArrayList))
	(current-paragraph ::java.lang.StringBuilder
			   (java.lang.StringBuilder)))
    
    (define (finish-paragraph!)
      (when (is (current-paragraph:length) > 0)
	(paragraphs:add (current-paragraph:toString))
	(current-paragraph:setLength 0)))
    
    (let next ()
      (let ((line (read-line input-port)))
	(cond
	 ((eof-object? line)
	  (finish-paragraph!)
	  paragraphs)
	 ((regex-match "^[ \t]*$" line)
	  (finish-paragraph!)
	  (next))
	 ((regex-match "^[#][+]BEGIN_SRC" line)
	  (finish-paragraph!)
	  (current-paragraph:append line)
	  (let snip ()
	    (let ((line (read-line input-port)))
	      (assert (isnt line eof-object?))
	      (current-paragraph:append (as char #\newline))
	      (current-paragraph:append line)
	      (cond
	       ((regex-match "^[#][+]END_SRC" line)
		(finish-paragraph!)
		(next))
	       (else
		(snip))))))
	 (else
	  (and-let* ((n ::int (current-paragraph:length))
		     ((is n > 0))
		     ((isnt (current-paragraph:charAt (- n 1))
			    eq? (as char #\space))))
	    (current-paragraph:append (as char #\space)))
	  (current-paragraph:append line)
	  (next)))))))


(define (extract-style-modifiers word::Word)::(sequence-of
					       (either
						Word
						TextStyle
						EndTextStyle))
  (match/regex
   word
   ("^[*](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Bold . ,(extract-style-modifiers word*))))
   ("^[/](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Italic . ,(extract-style-modifiers word*))))
   ("^[~](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Monospace . ,(extract-style-modifiers word*))))
   
   ("^(.+)[*]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Bold))))

   ("^(.+)[/]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Italic))))

   ("^(.+)[~]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Monospace))))

   (".*"
    `(,word))))

(define (parse-paragraph #!optional (input ::InputPort (current-input-port)))
  ::(sequence-of (either Word TextStyle EndTextStyle))
  (let ((words (read-words input)))
    (append-map extract-style-modifiers words)))

(define (book-title? text ::string)::(maybe string)
  (and-let* ((`(,_ ,title) (regex-match "^[*] ([^\n]+)$" text)))
    title))

(define (chapter-title? text ::string)::(maybe string)
  (and-let* ((`(,_ ,title) (regex-match "^[*][*] ([^\n]+)$" text)))
    title))

(define (section-title? text ::string)::(maybe string)
  (and-let* ((`(,_ ,title) (regex-match "^[*][*][*]+ ([^\n]+)$" text)))
    title))

(define (layout-paragraph
	 paragraph::Paragraph
	 word-operation::(maps (real real Word TextDecoration) to: void)
	 max-line-width::real
	 line-height::real)
  ::real
  (match paragraph
    (verbatim::string
     (WARN "skipping verbatim string "verbatim)
     0)
    ((Section title: title)
     (WARN "skipping section title "title)
     0)
    (words
     ::(sequence-of Word)
     (let ((top ::real 0)(left ::real 0)
	   (style ::TextDecoration (RegularText)))
       (for token in words
	 (match token
	   (word::string
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
	 
	 (+ top line-height)))))

(define (render-paragraph! paragraph::Paragraph
			   max-line-width::real
			   line-height::real)
  ::real
  (layout-paragraph
   paragraph
   (lambda (left::real top::real word::Word
		       style::TextDecoration)
     (painter:draw-styled-text! left top word style))
   max-line-width
   line-height))

(define-cache (paragraph-height words::(sequence-of word)
				max-line-width::real
				line-height::real)
  ::real
  (layout-words nothing max-line-width line-height))

(define (parse-book #!optional (input ::InputPort (current-input-port)))::Book
  (let* ((paragraphs ::(sequence-of input) (read-paragraphs input))
	 (book ::Book (Book chapters: (java.util.ArrayList)))
	 (current-chapter ::Chapter #!null))
    (for paragraph in paragraphs
      (cond
       ((is paragraph book-title?)
	=> (lambda (title ::string)
             (assert (eq? book:title #!null))
             (set! book:title title)))
       ((is paragraph chapter-title?)
	=> (lambda (title ::string)
             (when current-chapter
               (book:chapters:add current-chapter))
             (set! current-chapter (Chapter title: title
					    paragraphs: (java.util.ArrayList)))))
       ((is paragraph section-title?)
	=> (lambda (title ::string)
	     (current-chapter:paragraphs:add (Section title: title))))

       ((regex-match "^[#][+]BEGIN_SRC" paragraph)
	(current-chapter:paragraphs:add (string-append "\n\n" paragraph "\n\n")))
       
       (else
        (current-chapter:paragraphs:add 
         (call-with-input-string paragraph parse-paragraph)))))
    book))

(define (sample-book)::Book
  (Book title: "Sample Book"
	chapters:
	(vector
	 (Chapter
	  title: "First chapter"
	  paragraphs:
	  (vector
	   (call-with-input-string "\
These words are: *bold*, /italic/, */bold-italic/* and ~MonoSpace~.
"parse-paragraph))))))

(define-object (InteractiveBookReader book ::Book)
  ::Maximizable

  (define current-chapter ::int 0)
  
  (define chapter-scroll ::(sequence-of real)
    ((array-of real) length: (length book:chapters)))
  
  (define (draw! context::Cursor)::void
    (painter:precise-fill-rectangle!
     0 0
     (* (painter:precise-resolution-right) size:width)
     (* (painter:precise-resolution-down) size:height)
     #xffffffff)
    (let ((chapter ::Chapter (book:chapters current-chapter))
	  (top ::real 0))
      (escape-with break
	(for paragraph::Paragraph in chapter:paragraphs
	  (let ((height
		 (with-translation (0 top)
		   (render-paragraph! paragraph
				      (min max-text-width size:width)
				      (painter:styled-text-height)))))
	    (set! top (+ top height))
	    (when (is top > size:height)
	      (break)))))))

  (define max-text-width ::real
    (painter:styled-text-width
     "Abc def ghi jkl mno pq rst uvw xyz abcdefghijklmnopqrstuvwxyz"
     (RegularText)))
  
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
