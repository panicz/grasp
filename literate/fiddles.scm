(import (kawa regex))
(import (kawa pprint))
(import (language extensions))

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

(define* (rewrite-characters from: input ::InputPort := (current-input-port)
			     to: output ::OutputPort := (current-output-port))
  (let next ()
    (let ((c (read-char input)))
      (unless (eof-object? c)
	(write-char c output)
	(next)))))

(define-syntax-rule (match/regex subject (pattern . actions) ...)
  (cond ((regex-match pattern subject) . actions)
	...))

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
  (let* ((text ::string (call-with-output-string
			  (lambda (string ::OutputPort)
			    (rewrite-characters from: input to: string))))
	 (paragraphs ::(list-of string) (regex-split "\n[ \t]*\n+" text))
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

       ((regex-match "^[#][+]BEGIN_" paragraph)
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
