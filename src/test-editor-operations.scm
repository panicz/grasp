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

(undo!)

(e.g.
 (snapshot) ===> "
")

(redo!)

(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")

(for-each insert-character! '(#\d #\e #\f #\n #\e))

;;(WARN (car (slot-ref (history (the-document)) 'fronts)))

(e.g.
 (snapshot) ===> "
╭       ╮
│ defne │
╰      ^╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭       ╮
│ defne │
╰      ^╯
")

(times 2 move-cursor-left!)

(e.g.
 (snapshot) ===> "
╭       ╮
│ defne │
╰    ^  ╯
")

(insert-character! #\i)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰     ^  ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭       ╮
│ defne │
╰    ^  ╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰     ^  ╯
")

(times 2 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰       ^╯
")

(insert-character! #\space)

(snapshot)

#|
(and-let* ((history (history (the-document)))
	   (`((,last-operation . ,_) . ,_) history:fronts))
  (DUMP last-operation)
  (DUMP (last-operation:inverse)))
|#


#|

(insert-character! #\space)

(e.g.
 (snapshot) ===> "
╭         ╮
│ define  │
╰        |╯
")

(undo!)

(snapshot)



(insert-character! #\[)

(e.g.
 (snapshot) ===> "
╭        ╭  ╮ ╮
│ define │  │ │
╰        ╰ |╯ ╯
")
|#
