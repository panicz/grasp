(import
  (define-interface)
  (define-type)
  (define-object)
  (conversions)
  (indexable)
  (primitive)
  (space)
  (parse)
  (examples)
  (conversions)
  (srfi :11)
  (assert)
  (print)
  (cursor)
  )

(e.g.
 (parameterize ((the-document
		 (string->document "(define (f x y) z)")))
   (show->string (the-expression at: '(5 3 1 1)))) ===> "y")

(e.g.
 (parameterize ((the-document
		 (string->document "(define (f x #;y) z)")))
   (show->string (the-expression at: '(2 4 3 1 1)))) ===> "#;y")

(define (check-parser-correctness input::string)
  (let* ((parsed ::pair (with-input-from-string input parse-document))
	 (output ::string (with-output-to-string
			    (lambda ()
			      (show-document parsed)))))
    (unless (string=? input output)
      (WARN "Invalid parsing, expected: \"\\\n"input
	    "\ngot: \"\\\n"output"\"\n"))))

(check-parser-correctness "
(let ((quoted 'x)
      (quasiquoted `x)
      (unquoted ,x)
      (spliced ,@x))
  `(,'(quoted)
    ,`(quasiquoted)
    ,,(un quoted)
    ,@,@(un quote spliced)))
")

(check-parser-correctness "
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")

(check-parser-correctness "
(e.g. 
  (string-append \"abc\" \"def\")
   ===> \"abcdef\")
")

(check-parser-correctness "
   ; this input contains line comments
( ; in various places 
 + ; like, after an operator
 2 ; and an operand
 3 ; and at the end of a list
)

( ;as well as in an empty list
)

;and at the end of input
")
  

(check-parser-correctness "
  ; line comments with empty input
")

(check-parser-correctness "
(begin
  #;(move 10 #;bytes #;(from (beautiful)) source #;to target)
)
")

(check-parser-correctness "
hey!
  #| what follows is an empty list
  #| and this comment is nested |#|#
( #|this is the aforementioned empty list|#
  #|and it seems to work|# )
")
