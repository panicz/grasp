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
 (editor document universe)

 (utils test)
 )


(set! (the-document)
      (call-with-input-string
       "" parse-document))

(set! (the-cursor) (cursor 0 0 1))

(let ((selection ::Highlight (the-selection)))
  (set! selection:start (the-cursor))
  (set! selection:end (the-cursor)))

(insert-character! #\[)

(e.g.
 (snapshot)  ===> "
╭  ╮
│  │
╰ |╯
")

(delete-backward!)

(e.g.
 (snapshot) ===> "
")

(undo!)
(move-cursor-left!)

(e.g.
 (snapshot) ===> "
╭  ╮
│  │
╰ |╯
")

(e.g.
 (with-undo-redo
  (for-each insert-character! '(#\d #\e #\f #\n #\e))) ===> "
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

(e.g.
 (with-undo-redo
  (insert-character! #\i))  ===> "
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

(e.g.
 (with-undo-redo
  (insert-character! #\newline)) ===> "
╭        ╮
│ define │
│        │
│        │
│ -cache │
╰ |      ╯
")

(e.g.
 (with-undo-redo
  (delete-backward!)) ===> "
╭              ╮
│ define-cache │
╰       ^      ╯
")

(times 6 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭              ╮
│ define-cache │
╰             ^╯
")

(e.g.
 (with-undo-redo
  (times 6 delete-backward!)) ===> "
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
\"Computes the product")

(e.g.
 (snapshot) ===> "
╭        ╭     ╮           ╮
│ define │ ! n │           │
│        ╰     ╯           │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the product ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")

(times 5 undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
│ ❝┈┈•           │
│ ┊  ┊           │
╰ •┈┈❞           ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
╰                ╯
                  
  |               
")

(times 3 redo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
│ ❝┈┈┈┈┈┈┈┈┈┈┈•  │
│ ┊ Computes  ┊  │
╰ •┈┈┈┈┈┈┈┈┈┈┈❞  ╯
")

(times 2 redo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮    ╮
│ define │ ! n │    │
│        ╰     ╯    │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the  ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮           ╮
│ define │ ! n │           │
│        ╰     ╯           │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the product ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")

(times 20 delete-backward!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
│ ❝┈┈•           │
│ ┊  ┊           │
╰ •┈┈❞           ╯
")

(delete-backward!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
╰                ╯
                  
  |               
")

(undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
│ ❝┈┈•           │
│ ┊  ┊           │
╰ •┈┈❞           ╯
")

(times 2 undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ╮
│ define │ ! n │ │
│        ╰     ╯ │
│ ❝┈┈┈┈┈┈┈┈┈┈┈•  │
│ ┊ Computes  ┊  │
╰ •┈┈┈┈┈┈┈┈┈┈┈❞  ╯
")

(times 2 undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮    ╮
│ define │ ! n │    │
│        ╰     ╯    │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the  ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮           ╮
│ define │ ! n │           │
│        ╰     ╯           │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈• │
│ ┊ Computes the product ┊ │
╰ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞ ╯
")

(for-each insert-character! "
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

(set! (the-cursor) (cursor 0 4 1 1))

(e.g.
 (snapshot) ===> "
╭        ╭     ╮               ╮
│ define │ ! n │               │
│        ╰     ╯|              │
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(insert-character! #\space)
(insert-character! #\;)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾             ╮
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(undo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮               ╮
│ define │ ! n │               │
│        ╰     ╯|              │
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(redo!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾             ╮
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(for-each insert-character! " -> int")

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(set! (the-cursor) (cursor #\[ 7 1 1))

(e.g.
 (with-undo-redo
  (insert-character! #\;)) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭    ╭           ╮         ╮ │
│ ┊ i̶f̶ ┊ i̶s̶ n̶ <̶=̶ 1̶ ┊         ┊ │
│ ┊    ╰           ╯         ┊ │
│ ┊                          ┊ │
│ ┊   1̶                      ┊ │
│ ┊                          ┊ │
│ ┊  ╭     ╭   ╭       ╮ ╮ ╮ ┊ │
│ ┊  ┊ *̶ n̶ ┊ !̶ ┊ -̶ n̶ 1̶ ┊ ┊ ┊ ┊ │
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(e.g.
 (with-undo-redo
  (insert-character! #\;))  ===> "
╭        ╭     ╮ ⸾ -> int      ╮
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
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(times 2 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭    ╭           ╮         ╮ │
│ │ if │ is n <= 1 │         │ │
│ │ ^  ╰           ╯         │ │
│ │                          │ │
│ │   1                      │ │
│ │                          │ │
│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │
│ │  │ * n │ ! │ - n 1 │ │ │ │ │
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(insert-character! #\;)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭   ╭           ╮          ╮ │
│ │ i̶f̶│ is n <= 1 │          │ │
│ │ ^̶ ╰           ╯          │ │
│ │                          │ │
│ │   1                      │ │
│ │                          │ │
│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │
│ │  │ * n │ ! │ - n 1 │ │ │ │ │
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(times 4 move-cursor-right!)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭   ╭           ╮          ╮ │
│ │ i̶f̶│ is n <= 1 │          │ │
│ │   ╰           ╯          │ │
│ │   ~           ~          │ │
│ │   1                      │ │
│ │                          │ │
│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │
│ │  │ * n │ ! │ - n 1 │ │ │ │ │
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(insert-character! #\;)

;;(set! (verbose-tests?) #true)

(e.g.
 (snapshot) ===> "
╭        ╭     ╮ ⸾ -> int      ╮
│ define │ ! n │               │
│        ╰     ╯               │
│ ❝┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈•     │
│ ┊ Computes the product ┊     │
│ ┊ 1 * ... * n          ┊     │
│ •┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈┈❞     │
│ ╭   ╭           ╮          ╮ │
│ │ i̶f̶┊ i̶s̶ n̶ <̶=̶ 1̶ ┊          │ │
│ │   ╰           ╯          │ │
│ │   ~           ~          │ │
│ │   1                      │ │
│ │                          │ │
│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │
│ │  │ * n │ ! │ - n 1 │ │ │ │ │
╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

