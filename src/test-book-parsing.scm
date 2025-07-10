(import
 (kawa regex)
 (language define-interface)
 (language define-type)
 (language define-object)
 (utils conversions)
 (language infix)
 (language match)
 (language while)
 (language for)
 (language examples)
 (srfi :11)
 (language assert)
 (utils print)
 (utils hash-table)
 (language mapping)
 (utils functions)
 )

(define-alias InputPort gnu.kawa.io.InPort)
(define-alias OutputPort gnu.kawa.io.OutPort)

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

(let* ((input-text (call-with-input-file "../the-most-impressive-program-ever-written/book.org" read-entire-input-to-string))
       (paragraphs (regex-split "\n\n+" input-text))
       (i ::int 0))
  (for paragraph in paragraphs
    (print "\nParagraph "i": \n")
    (display paragraph)
    (newline)
    (set! i (+ i 1))))

