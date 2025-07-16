(import (kawa regex))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language for))
(import (language examples))
(import (language fundamental))
(import (utils hash-table))
(import (language mapping))
(import (utils functions))
(import (language infix))
(import (language match))
(import (language while))
(import (language attributes))
(import (language define-parameter))
(import (language mapping))
(import (language keyword-arguments))
(import (language define-cache))

(import (utils conversions))
(import (srfi :11))
(import (language assert))
(import (utils print))

(define (read-entire-input-to-string
	 #!optional (in ::InputPort (current-input-port)))
  ::string
  (call-with-output-string
    (lambda (out ::OutputPort)
      (let next ()
	(let ((c (read-char in)))
	  (unless (eof-object? c)
	    (write-char c out)
	    (next)))))))

(e.g.
 (call-with-input-string "
ząb zupa zębowa

dąb zupa dębowa

hura!
" read-entire-input-to-string)
 ===> "
ząb zupa zębowa

dąb zupa dębowa

hura!
")

#;(let* ((input-text (call-with-input-file "../../the-most-impressive-program-ever-written/book.org" read-entire-input-to-string))
       (paragraphs (regex-split "\n\n+" input-text))
       (i ::int 0))
  (for paragraph in paragraphs
    (print "\nParagraph "i": \n")
    (display paragraph)
    (newline)
    (set! i (+ i 1))))


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
    => (lambda (result)
	 (match result
	   (`(,_ ,word*)
	    `(Bold . ,(extract-style-modifiers word*))))))
   ("^[/](.+)$"
    => (lambda (result)
	 (match result
	   (`(,_ ,word*)
	    `(Italic . ,(extract-style-modifiers word*))))))
   ("^[~](.+)$"
    => (lambda (result)
	 (match result
	   (`(,_ ,word*) 
	    `(Monospace . ,(extract-style-modifiers word*))))))

   ("^(.+)[*]([.,?!:;]*)$"
    => (lambda (result)
	 (match result
	   (`(,_ ,prefix ,suffix)
	    `(,@(extract-style-modifiers (string-append prefix suffix))
	      (EndTextStyle style: Bold))))))

   ("^(.+)[/]([.,?!:;]*)$"
    => (lambda (result)
	 (match result
	   (`(,_ ,prefix ,suffix)
	    `(,@(extract-style-modifiers (string-append prefix suffix))
	      (EndTextStyle style: Italic))))))

   ("^(.+)[~]([.,?!:;]*)$"
    => (lambda (result)
	 (match result
	   (`(,_ ,prefix ,suffix)
	    `(,@(extract-style-modifiers (string-append prefix suffix))
	      (EndTextStyle style: Monospace))))))

   (".*"
    `(,word))))

(define (parse-paragraph paragraph ::string)::(sequence-of
					       (either
						Word
						TextStyle
						EndTextStyle))
  (let ((words (regex-split "\\s+" paragraph)))
    (append-map extract-style-modifiers words)))


(write
 (parse-paragraph
  "These words are: *bold*, /italic/, */bold-italic/* and ~monospace~."))
