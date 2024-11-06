(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (utils conversions)
  (editor interfaces elements)
  (language infix)
  (language match)
  (language while)
  (language for)
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
  (utils functions)
  (editor utils search)
  )

(e.g.
 (match (parse-string "fix suffixum")
   (`(,pattern ,subject)
    (infix-start pattern subject)))
 ===> 3)

(e.g.
 (match (parse-string "b b")
   (`(,pattern ,subject)
    (infix-start pattern subject)))
 ===> 0)

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
 (match (parse-string "b rub")
   (`(,pattern ,subject)
    (suffix-start pattern subject)))
 ===> 2)

(e.g.
 (match (parse-string "c cat")
   (`(,pattern ,subject)
    (prefix-end pattern subject)))
 ===> 1)

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

(e.g.
 (let* ((pattern (parse-string "(a b ,c)"))
	(subject (parse-string "(a b d)"))
	(bindings (matches pattern subject
			   (mapping (key ::String)
			     #!null)))
	(value (lambda (x)
		 (match x
		   (x::Shadowed (x:value))
		   (_ x))))
	(bound-keys (keys bindings)))
   (map (lambda (key)
	  (let ((value (value (bindings key))))
	    `(,key ,value)))
	bound-keys)) ===> (("c" d)))

(e.g.
 (and-let* ((pattern (parse-string "b"))
	    (document (string->document "(a b d)"))
	    ((Highlight start: '(0 3 1 1)
			end: '(1 3 1 1))
	     (next-match of: pattern in: document)))))

(e.g.
 (and-let* ((pattern (parse-string "b c"))
	    (document (string->document "(a b c d)"))
	    ((Highlight start: start
			end: end) (next-match of: pattern
					      in: document))
	    ('(0 3 1 1) start)
	    ('(1 5 1 1) end))
   (with-eval-access
    (values (the-expression at: start in: document)
	    (the-expression at: end in: document))))
 ===> b c)

(e.g.
 (and-let* ((pattern (parse-string "ub"))
	    (document (string->document "(hubba #;tuba bubba)")))
   (match/equal? (all-matches of: pattern in: document)
		 `(,(Highlight start: '(1 1 1 1) end: '(3 1 1 1))
		   ,(Highlight start: '(1 3 1 1) end: '(3 3 1 1))))))


(and-let* ((pattern (parse-string "b c"))
	   (document (string->document "a b c d (a b c (a b c))")))
  (match/equal?
   (all-matches of: pattern in: document)
   (list
    (Highlight start: '(0 3 1) end: '(1 5 1))
    (Highlight start: '(0 3 9 1) end: '(1 5 9 1))
    (Highlight start: '(0 3 7 9 1) end: '(1 5 7 9 1)))))

(e.g.
 (and-let* ((pattern (parse-string "b c"))
	    (document (string->document "(rub cat)")))
   (match/equal?
    (all-matches of: pattern in: document)
    (list
     (Highlight start: '(2 1 1 1) end: '(1 3 1 1))))))

(parameterize ((debugging? #t))
 (and-let* ((pattern (parse-string "b c"))
	    (document (string->document "(rub #;(dub club) cat)")))
   (DUMP (all-matches of: pattern in: document))))

