(import
 (define-syntax-rule)
 (define-interface)
 (define-type)
 (define-object)
 (extent)
 (conversions)
 (indexable)
 (space)
 (cursor)
 (primitive)
 (extent)
 (text-painter)
 (combinators)
 (parse)
 (examples)
 (assert)
 (infix)
 (match)
 (functions)
 (print)
 (painter)
 (for)
 (document-operations)
 (editor-operations)
 (history)
 (painter)
 (text-painter)
 )

(define (snapshot)::String
  (parameterize ((the-painter (TextPainter)))
    (draw-document! (the-document))
    (let ((result ::String (invoke (the-painter) 'toString)))
      (display result)
      result)))

(set! (the-document)
      (call-with-input-string
       "" parse-document))

(set! (the-cursor) (cursor 0 0 1))
(set! (the-selection-anchor) (the-cursor))

(insert-character! #\[)



(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")


(insert-character! #\d)

(snapshot)

(insert-character! #\e)

(snapshot)

(insert-character! #\f)

(snapshot)

(insert-character! #\i)

(snapshot)

(insert-character! #\n)

(snapshot)

(insert-character! #\e)

(snapshot)

(insert-character! #\space)

(snapshot)

