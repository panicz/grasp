(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (utils conversions)
  (editor interfaces elements)
  (language infix)
  (language match)
  (editor types primitive)
  (editor types spaces)
  (editor document parse)
  (language examples)
  (utils conversions)
  (srfi :11)
  (language assert)
  (utils print)
  (editor document cursor)
  )


 (let ((doc (string->document "(define (f x y) z)")))
   ;; The pattern matcher is able to match against
   ;; parsed atoms, in addition to regular symbols
   (and-let* ((`(((define (f x y) z))) doc))))

;; Note that in the case of atoms and symbols,
;; the order of comparison does matter.
;; (This is unfortunate, but we can't fix that
;; without patching Kawa)

(e.g. (is (Atom "a") match/equal? 'a))

(e.g. (isnt 'a match/equal? (Atom "a")))


;; eq? uses objects' identities, so that symbols
;; can be told apart from atoms, and distinct same looking
;; atoms can be as well

(e.g. (isnt 'a eq? (Atom "a")))

(e.g. (isnt (Atom "a") eq? 'a))

(e.g. (isnt (Atom "a") eq? (Atom "a")))


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

(check-parser-correctness "
(first rest ...)
")

(check-parser-correctness "
(first . rest)
")

(check-parser-correctness "
(first . ...)
")

(check-parser-correctness "
(first . (rest))
")

(check-parser-correctness "
(first . #|and|# #;then rest) ;over
")

(check-parser-correctness "
(first #||# .  #|and|# #;then ;finally
 rest)
")

(check-parser-correctness "
(first . #|and|# (#;then rest))
")

(check-parser-correctness "
(first . #|and|# (#;the-end))
")

(check-parser-correctness "
(first . ,rest)
")

(check-parser-correctness "
(first . ,@rest)
")

(check-parser-correctness "
(first . 'rest)
")

(check-parser-correctness "
(first . `rest)
")
