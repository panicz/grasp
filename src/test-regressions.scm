;; -*- mode: scheme; *preserve-trailing-whitespace*: t -*-

(import
 (language define-syntax-rule)
 (language define-interface)
 (language define-type)
 (language define-object)
 (utils conversions)
 (editor interfaces painting)
 (editor interfaces elements)
 (editor types spaces)
 (editor document cursor)
 (editor types primitive)
 (editor text-painter)
 
 (editor types extensions extensions)
 (editor types extensions combinators)
 (editor document parse)
 (language examples)
 (language assert)
 (language infix)
 (language match)
 (language while)
 (utils functions)
 (utils print)
 (utils hash-table)
 (language for)
 (editor document document-operations)
 (editor document editor-operations)
 (editor document history-tracking)
 (editor interfaces painting)
 (editor text-painter)
 (editor document documents)
 (utils test)
 )

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1))
		(verbose-tests? #t))
  (for-each insert-character! "[1 ")
  (e.g. (snapshot) ===> "
╭    ╮
│ 1  │
╰   |╯
")
  (e.g. (the-cursor) ===> (1 2 1 1))
  (delete-backward!)
  (e.g. (snapshot) ===> "
╭   ╮
│ 1 │
╰  |╯
")
  (e.g. (the-cursor) ===> (0 2 1 1)) 
)
