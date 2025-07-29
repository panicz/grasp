(import (kawa regex))
(import (kawa pprint))
(import (language extensions))

(define-syntax-rule (match/regex subject (pattern . actions) ...)
  (cond ((regex-match pattern subject)
	 . actions)
	...))

(define-enum TextStyle
  (Bold
   Italic
   Monospace))

(define-type (EndTextStyle style: TextStyle))

(define-alias TextDecoration (EnumSetOf TextStyle))

(define-alias Word string)

(define-private punctuation ::(set-of gnu.text.Char)
  (set #\; #\. #\: #\, #\? #\!))

(define-cache (char->string single-char ::gnu.text.Char)
  ::string
  (list->string (list single-char)))

(define* (read-words input-port ::InputPort := (current-input-port))
  ::(list-of Word)
  (let ((c (read-char input-port)))
    (cond 
      ((eof-object? c)
       '())
       
      ((char-whitespace? c)
       (read-words input-port))
       
      ((is c in punctuation)
       (cons (char->string c) (read-words input-port)))
      
      (else
       (let ((word (call-with-output-string
                     (lambda (p)
                       (write-char c p)
                       (let loop ()
                         (let ((c (peek-char input-port)))
                            (unless (or (eof-object? c)
                                        (char-whitespace? c)
					(is c in punctuation))
                              (write-char (read-char input-port) p)
                              (loop))))))))
	 (cons word (read-words input-port)))))))

(define* (read-paragraphs input-port ::InputPort := (current-input-port))
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
  (match/regex word
    ("^[*](.+)$"
     => (lambda* (`(,_ ,word*))
	  `(,TextStyle:Bold . ,(extract-style-modifiers word*))))
    ("^[/](.+)$"
     => (lambda* (`(,_ ,word*))
	  `(,TextStyle:Italic . ,(extract-style-modifiers word*))))
    ("^[~](.+)$"
     => (lambda* (`(,_ ,word*))
	  `(,TextStyle:Monospace . ,(extract-style-modifiers word*))))
    ("^(.+)[*]$"
     => (lambda* (`(,_ ,word*))
	  `(,@(extract-style-modifiers word*)
	    ,(EndTextStyle style: TextStyle:Bold))))
    ("^(.+)[/]$"
     => (lambda* (`(,_ ,word*))
	  `(,@(extract-style-modifiers word*)
	    ,(EndTextStyle style: TextStyle:Italic))))
    ("^(.+)[~]$"
     => (lambda* (`(,_ ,word*))
	  `(,@(extract-style-modifiers word*)
	    ,(EndTextStyle style: TextStyle:Monospace))))
    (".*"
     `(,word))))

(define-type (Chapter title: string
		      paragraphs: (sequence-of
				   (either
				    string
				    (sequence-of
				     (either
				      string
				      TextStyle
				      EndTextSTyle)))) := (java.util.ArrayList)))

(define-type (Book title: string
		   chapters: (sequence-of Chapter) := (java.util.ArrayList)))

(define-type (Section title: string))

(define* (parse-paragraph input ::InputPort := (current-input-port))
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

(e.g.
 (book-title? "* The Most Impressive Program Ever Written")
 ===> "The Most Impressive Program Ever Written")

(define* (parse-book input ::InputPort := (current-input-port))::Book
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

(let* ((book (call-with-input-file
		 "../../the-most-impressive-program-ever-written/book.org"
	       parse-book)))
  (pprint book))

;; Local Variables:
;; eval: (put 'match/regex 'scheme-indent-function 1)
;; End:
