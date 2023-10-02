(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (utils conversions)
  (extent)
  (editor interfaces indexable)
  (editor interfaces painter)
  (editor types space)
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
  (editor types text)
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
