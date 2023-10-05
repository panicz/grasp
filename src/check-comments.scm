(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (utils conversions)
  (editor interfaces elements)
  (editor interfaces painting)
  (editor types spaces)
  (editor document cursor)
  (editor types primitive)
  (editor types extensions combinators)
  (editor text-painter)
  (editor types comments)
  (editor document parse)
  (language examples)
  (language assert)
  (language infix)
  (language match)
  ((utils functions))
  (utils print)
  (editor input pane)
  (editor document document-operations)
  )

(set! (the-painter) (TextPainter))

(display "====================================")

(let* ((document (with-input-from-string
"          #|FACTORIAL|#
  ;  A factorial of an integer number n
  ;  is a product 1 * 2 * 3 * 4 * ... * n.
  ;  It represents the number of permutations
  ;  of an n-element set.
  ; 
(define (! n) ; -> int
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1
      (* n (! (- n 1))))) ; factorial
(e.g. (! 5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document))
       (editor ::Editor (as Editor (the-screen))))
  (set! editor:document document)
  (set! (the-document) document))

(invoke (the-screen) 'draw! '())

(display (the-painter))

(display (the-expression at: '(#\[ 11 0 1)))
