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
  (editor document parse)
  
  (editor types extensions extensions)
  (editor types extensions visual-stepper)
  )

(match (parse-string "a")
  (`(a)
   (WARN 'yes))
  (else
   (WARN 'no else (invoke (car else) 'getClass))))

(match (cons (Atom "a") (empty))
  (`(a) (WARN 'yup))
  (_ (WARN 'nope)))

(let ((empty (parse-string "")))
  (WARN empty (empty:getClass) (match/equal? empty '())))

(WARN (reduce (parse-string "! 5")))

(let-values (((reduced or gy) (reduce (parse-string "! 5"))))
  (WARN (reduce reduced)))
