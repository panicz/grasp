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
  (utils hash-table)
  (language mapping)
  (editor utils search)
  )

(e.g.
 (match (parse-string "fix suffixum")
   (`(,pattern ,subject)
    (infix-start pattern subject)))
 ===> 3)

(e.g.
 (match (parse-string "abc \"abc\"")
   (`(,a ,b)
    (textual=? a b))))

(e.g.
 (match (parse-string "pre prefix")
   (`(,pattern ,subject)
    (prefix-end pattern subject)))
 ===> 3)

(e.g.
 (match (parse-string "fix suffix")
   (`(,pattern ,subject)
    (suffix-start pattern subject)))
 ===> 3)

(e.g.
 (match (parse-string "in suffixum")
   (`(,pattern ,subject)
    (infix-start pattern subject)))
 ===> #!null)

(e.g.
 (match (parse-string "post prefix")
   (`(,pattern ,subject)
    (prefix-end pattern subject)))
 ===> #!null)

(e.g.
 (match (parse-string "suf suffix")
   (`(,pattern ,subject)
    (suffix-start pattern subject)))
 ===> #!null)

(e.g. (length (empty)) ===> 0)


(let* ((pattern (parse-string "(a b ,c)"))
       (subject (parse-string "(a b d)"))
       (bindings (matches pattern subject))
       (bound-keys (keys bindings)))
  (print bound-keys (map bindings bound-keys)))

