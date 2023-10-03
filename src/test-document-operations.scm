(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (utils conversions)
  (extent)
  (indexable)
  (painter)
  (space)
  (cursor)
  (primitive)
  (combinators)
  (text-painter)
  (comments)
  (parse)
  (language examples)
  (language assert)
  (language infix)
  (language match)
  ((utils functions))
  (print)
  (pane)
  (document-operations)
  (text)
  )

(e.g.
 (parameterize ((the-document (string->document "\
#;0 1 #|2|# 3 ;4
5 ;6"))
		#;(cell-access-mode CellAccessMode:Evaluating))
   (and (equal? (extract! at: '(1 0 1))
		(ExpressionComment expression: (Atom "0")))
	(equal? (extract! at: '(2 2 1))
		(BlockComment content: (text "2")))
	(equal? (extract! at: '(2 4 1))
		(LineComment content: (text "4")))
	(equal? (extract! at: '(2 6 1))
		(LineComment content: (text "6")))
	(document->string))) ===>
		" 1  3 
5 
")
