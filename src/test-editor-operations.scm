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
      ;;(display (history (the-document)))
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

(delete-backward!)

(e.g.
 (snapshot) ===> "
")

(undo!)

(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")

(for-each insert-character! '(#\d #\e #\f #\n #\e))

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

(times 2 delete-backward!)

(e.g.
 (snapshot) ===> "
╭     ╮
│ def │
╰    ^╯
")

(undo!)

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

(e.g.
 (snapshot) ===> "
╭         ╮
│ define  │
╰        |╯
")

(undo!)

;; Note: it is OK if the cursor isn't reverted faithfully,
;; as long as this does not affect re-playing history.

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰       |╯
")

(for-each insert-character! '(#\- #\c #\a #\c #\h #\e))

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰             ^╯
")
 
(undo!)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰       ^╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰             ^╯
")

(times 6 move-cursor-left!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰       ^      ╯
")


(insert-character! #\space)

(e.g.
 (snapshot) ===> "
╭               ╮
│ define -cache │
╰        |      ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰       ^      ╯
")

(insert-character! #\newline)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
│        │
│        │
│ -cache │
╰ |      ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰       ^      ╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
│        │
│        │
│ -cache │
╰ |      ╯
")

(delete-backward!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰       ^      ╯
")

(undo!)

(snapshot)

(exit)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰       ^╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭       ╮
│ defne │
╰    ^  ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")

(times 2 redo!)
(times 2 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╮
│ define │
╰       ^╯
")


(insert-character! #\[)

(e.g.
 (snapshot) ===> "
╭       ╭  ╮ ╮
│ define│  │ │
╰       ╰ |╯ ╯
")

(times 2 move-cursor-left!)
(insert-character! #\space)
(times 2 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╭  ╮ ╮
│ define │  │ │
╰        ╰ |╯ ╯
")

(for-each insert-character! '(#\! #\space #\n))

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
╰        ╰    ^╯ ╯
")

(times 3 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
╰        ╰     ╯|╯
")


(for-each insert-character! "
\"Computes the product
1 * ... * n")

(e.g.
 (snapshot) ===> "
╭        ╭     ╮           ╮
│ define │ ! n │           │
│        ╰     ╯           │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the product ┊ │
│ ┊ 1 * ... * n          ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")


(times 2 move-cursor-right!)

(for-each insert-character! "
[if [is n <= 1]
  1
 [* n [! [- n 1")

(e.g.
 (snapshot) ===> "
╭        ╭     ╮               ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭    ╭           ╮         ╮ │
│ │ if │ is n <= 1 │         │ │
│ │    ╰           ╯         │ │
│ │                          │ │
│ │   1                      │ │
│ │                          │ │
│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │
│ │  │ * n │ ! │ - n 1 │ │ │ │ │
╰ ╰  ╰     ╰   ╰      ^╯ ╯ ╯ ╯ ╯
")



;(DUMP (last-operation))

;(undo!)

;(DUMP (the-cursor))

;;(snapshot)

#|

(insert-character! #\[)

(e.g.
 (snapshot) ===> "
╭        ╭  ╮ ╮
│ define │  │ │
╰        ╰ |╯ ╯
")
|#
