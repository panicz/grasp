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
  (editor text-painter)
  (editor types comments)
  (editor document parse)
  (language examples)
  (language assert)
  (language infix)
  (language match)
  (utils functions)
  (utils print)
  (editor document document-operations)
  (editor types texts)
  )

#|
(e.g.
 (let* ((document (string->document "1 3 5"))
	(taken (extract! at: '(3 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
   ===>
   "1  5"
   "3")

(e.g.
 (let* ((document (call-with-input-string "1 3 5"
		    parse-document))
	(taken (extract! at: '(5 1) from: document)))

   (values (document->string document)
	   (pair->string taken)))
   ===>
   "1 3 "
   "5")

(e.g.
 (let* ((document (call-with-input-string "(1 3 5)"
		    parse-document))
	(taken (extract! at: '(1 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
   ===>
   "( 3 5)"
   "1")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(3 1 1) from: document)))
   (assert (head/tail-separator? taken))
   (document->string document))
 ===> "(1  5)")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(1 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
 ===>
 "(  5)"
 "1")

(e.g.
 (let* ((document (call-with-input-string "(1 . 5)"
		    parse-document))
	(taken (extract! at: '(5 1 1) from: document)))
   (values (document->string document)
	   (pair->string taken)))
 ===>
 "(1 )"
 "5")

(e.g.
 (parameterize ((the-document (string->document "\
#;0 1 #|2|# 3 ;4
5 ;6")))
   (let* ((0th (extract! at: '(1 0 1)))
	  (2nd (extract! at: '(2 2 1)))
	  (4th (extract! at: '(2 4 1)))
	  (6th (extract! at: '(2 6 1))))
     (values (show->string 0th)
	     (show->string 2nd)
	     (show->string 4th)
	     (show->string 6th)
	     (document->string (the-document)))))
 ===> "#;0" "#|2|#" ";4\n" ";6\n" "  1  3 \n5 \n")


(e.g.
 (parameterize ((the-document
		 (string->document
		  "#|0|# 1 #;2 3 #|4|# 5 #;6")))
   (let* ((0th (extract! at: '(1 0 1)))
	  (2nd (extract! at: '(2 2 1)))
	  (4th (extract! at: '(2 4 1)))
	  (6th (extract! at: '(2 6 1))))
     (insert! 0th at: '(1 6 1))
     (insert! 2nd at: '(1 4 1))
     (insert! 4th at: '(1 2 1))
     (insert! 6th at: '(0 0 1))
     (document->string (the-document))))
 ===> "#;6 1 #|4|#  3 #;2 5 #|0|# ")

(e.g.
 (let ((document (string->document "1   5")))
   (insert! (parse-string "3") into: document at: '(1 2 1))
   (document->string document)) ===> "1 3 5")

(e.g.
 (let ((document (string->document "1     7")))
   (insert! (parse-string "3 5") into: document at: '(1 2 1))
   (document->string document)) ===> "1 3 5 7")

(e.g.
 (let ((document (string->document "3 5")))
   (insert! (parse-string "1") into: document at: '(0 0 1))
   (document->string document)) ===> "1 3 5")

(e.g.
 (let ((document (string->document "5 7")))
   (insert! (parse-string "1 3") into: document at: '(0 0 1))
   (document->string document)) ===> "1 3 5 7")

(e.g.
 (let ((document (string->document "1   5")))
   (insert! head/tail-separator
	    into: document at: '(1 2 1))
   (document->string document)) ===> "1 . 5")
|#


#|
(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(1 1)
			with: 'x
			in: document)
   document) ===> ((x 2 . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(3 1)
			with: 'x
			in: document)
   document) ===> ((1 x . 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(5 1)
			with: 'x
			in: document)
   document) ===> ((1 2 x 3)))

(e.g.
 (let ((document `((,1 ,2 . ,3))))
   (replace-expression! at: '(7 1)
			with: 'x
			in: document)
   document) ===> ((1 2 . x)))

(e.g.
 (let ((document `((,1 ,2 ,3))))
   (replace-expression! at: '(3 1)
			with: head/tail-separator
			in: document)
   document) ===> ((1 . 3)))
|#

#|
(e.g.
 (parameterize ((the-document (string->document "\
#;0 1 #|2|# 3 ;4
5 ;6"))
		#;(cell-access-mode CellAccessMode:Evaluating))
   (and (equal? (extract! at: '(1 0 1))
		(ExpressionComment (Atom "0")))
	(equal? (extract! at: '(2 2 1))
		(BlockComment (text "2")))
	(equal? (extract! at: '(2 4 1))
		(LineComment (text "4")))
	(equal? (extract! at: '(2 6 1))
		(LineComment (text "6")))
	(document->string (the-document)))) ===>
		" 1  3 
5 
")
|#
