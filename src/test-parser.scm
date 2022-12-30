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
  )

(define factorial-definition (with-input-from-string "\
(define (factorial n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
" parse-document))

(show-document factorial-definition)
(newline)

(define string-append-example (with-input-from-string "\
(e.g. 
  (string-append \"abc\" \"def\")
   ===> \"abcdef\")
" parse-document))

(show-document string-append-example)
(newline)

(define something-with-line-comments
  (with-input-from-string "\
; this input contains line comments
( ; in various places 
 + ; like, after an operator
 2 ; and an operand
 3 ; and at the end of a list
)

( ;as well as in an empty list
)

;and at the end of input
" parse-document))

(show-document something-with-line-comments)
(newline)


(define line-comments-with-empty-input
  (with-input-from-string "\
; line comments with empty input
" parse-document))

(show-document line-comments-with-empty-input)
(newline)

(define nested-expression-comments
  (with-input-from-string "\
(begin
#; (move 10 #;bytes #;(from (beautiful)) source #;to target)
)
" parse-document))

(show-document nested-expression-comments)
(newline)

(define nested-block-comments (with-input-from-string "\
hey!
#| what follows is an empty list
#| and this comment is nested |#|#
( #|this is the aforementioned empty list|#
#|and it seems to work|# )
" parse-document))

(show-document nested-block-comments)
(newline)

