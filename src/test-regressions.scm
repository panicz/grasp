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
 (language keyword-arguments)
 (editor document document-operations)
 (editor document editor-operations)
 (editor document history-tracking)
 (editor interfaces painting)
 (editor text-painter)
 (editor document universe)

 (utils test)
 (editor types extensions widgets)
 (editor types extensions testing)
 (editor types extensions visual-stepper)
 (editor types spaces)
 (editor input screen)
 (editor input document-editor)

 )

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1)))
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

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1)))
  (for-each insert-character! "[Button label: \"button")
  (times 2 move-cursor-right!)
  (for-each insert-character! " action: nothing]") 
  (e.g. (snapshot) ===> "
╭               ❝┈┈┈┈┈┈┈┈•                 ╮
│ Button label: ┊ button ┊ action: nothing │
╰               •┈┈┈┈┈┈┈┈❞                 ╯
")
  (delete-backward!)
  (e.g. (snapshot) ===> "
")
  (undo!)
  (e.g. (snapshot) ===> "
╭               ❝┈┈┈┈┈┈┈┈•                 ╮
│ Button label: ┊ button ┊ action: nothing │
╰               •┈┈┈┈┈┈┈┈❞                 ╯
")
  (perform&record! (EnchantExpression))
  (e.g. (snapshot) ===> "
╭────────╮
│ button │
╰────────╯
")
  (perform&record! (DisenchantExpression))
  (e.g. (snapshot) ===> "
╭               ❝┈┈┈┈┈┈┈┈•                 ╮
│ Button label: ┊ button ┊ action: nothing │
╰               •┈┈┈┈┈┈┈┈❞                 ╯
")
  (undo!)
  (e.g. (snapshot) ===> "
╭────────╮
│ button │
╰────────╯
")
  (delete-backward!)
  (e.g. (snapshot) ===> "
")
  (undo!)
  (move-cursor-right!)
  (e.g. (snapshot) ===> "
╭────────╮ 
│ button │ 
╰────────╯|
")
  (delete-backward!)
  (e.g. (snapshot) ===> "
")
  )

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1)))
  (for-each insert-character! "(Stepper (+ (* 2 3) (- 4 5)))")
  (e.g. (snapshot) ===> "
╭         ╭   ╭       ╮ ╭       ╮ ╮ ╮
│ Stepper │ + │ * 2 3 │ │ - 4 5 │ │ │
╰         ╰   ╰       ╯ ╰       ╯ ╯ ╯
")
  
  (perform&record! (EnchantExpression))

  ;; the content isn't visually aligned particularly well
  ;; and should probably be fixed some day
  (e.g. (snapshot) ===> "
╔═══════════════════════════════════╗
║╭  ╭       ╮ ╭       ╮  ╮          ║
║│+ │ * 2 3 │ │ - 4 5 │  │          ║
║╰  ╰       ╯ ╰       ╯  ╯          ║
║╭─────╮╭─────╮╭─────╮╭─────╮╭─────╮║
║│ ▮◀◀ ││ ▮◀  ││  ▶  ││  ▶▮ ││ ▶▶▮ │║
║╰─────╯╰─────╯╰─────╯╰─────╯╰─────╯║
╚═══════════════════════════════════╝
")
  (move-cursor-right!)
  (delete-backward!)
  (e.g. (snapshot) ===> "
")
  (undo!)
  (e.g. (snapshot) ===> "
╔═══════════════════════════════════╗
║╭  ╭       ╮ ╭       ╮  ╮          ║
║│+ │ * 2 3 │ │ - 4 5 │  │          ║
║╰  ╰       ╯ ╰       ╯  ╯          ║
║╭─────╮╭─────╮╭─────╮╭─────╮╭─────╮║
║│ ▮◀◀ ││ ▮◀  ││  ▶  ││  ▶▮ ││ ▶▶▮ │║
║╰─────╯╰─────╯╰─────╯╰─────╯╰─────╯║
╚═══════════════════════════════════╝
")
  (perform&record! (DisenchantExpression
		    at: (suffix-without (isnt _ integer?)
					(the-cursor))))
  (e.g. (snapshot) ===> "
╭         ╭   ╭       ╮ ╭       ╮ ╮ ╮
│ Stepper │ + │ * 2 3 │ │ - 4 5 │ │ │
╰         ╰   ╰       ╯ ╰       ╯ ╯ ╯
")
  (undo!)
  (e.g. (snapshot) ===> "
╔═══════════════════════════════════╗
║╭  ╭       ╮ ╭       ╮  ╮          ║
║│+ │ * 2 3 │ │ - 4 5 │  │          ║
║╰  ╰       ╯ ╰       ╯  ╯          ║
║╭─────╮╭─────╮╭─────╮╭─────╮╭─────╮║
║│ ▮◀◀ ││ ▮◀  ││  ▶  ││  ▶▮ ││ ▶▶▮ │║
║╰─────╯╰─────╯╰─────╯╰─────╯╰─────╯║
╚═══════════════════════════════════╝
")
  )

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1)))
  (for-each insert-character! "[] ")
  (move-cursor-left!)
  (insert-character! #\newline)
  (e.g. (snapshot) ===> "
╭  ╮
│  │
╰  ╯
    
    
|   
")
  (move-cursor-right!)
  (e.g. (snapshot) ===> "
╭  ╮
│  │
╰  ╯
    
    
 |  
")
  )

(parameterize  ((the-document (call-with-input-string
				  "" parse-document))
		(the-cursor (cursor 0 0 1)))
  (for-each insert-character! "[quote []]")
  (enchant-expression!)
  (e.g. (snapshot) ===> "
┏  ┓
┃  ┃
┗  ┛
")
  (move-cursor-left!)
  (insert-character! #\space)
  (e.g. (snapshot) ===> "
┏   ┓
┃   ┃
┗   ┛
")
  (insert-character! #\x)
  (e.g. (snapshot) ===> "
┏   ┓
┃ x ┃
┗  ^┛
")
  (insert-character! #\space)
  (insert-character! #\z)
  (e.g. (snapshot) ===> "
┏     ┓
┃ x z ┃
┗    ^┛
")
  (times 3 move-cursor-left!)
  (insert-character! #\space)
  (insert-character! #\y)
  (e.g. (snapshot) ===> "
┏       ┓
┃ x y z ┃
┗    ^  ┛
")
  (delete-backward!)
  (e.g. (snapshot) ===> "
┏      ┓
┃ x  z ┃
┗   |  ┛
")
  (move-cursor-left!)
  ;;(parameterize ((debugging? #t)))
  (delete-backward!)
  (e.g. (snapshot) ===> "
┏     ┓
┃   z ┃
┗ |   ┛
")
  )

(define/kw (swipe! finger ::byte
		   from: initial ::Position
		   to: final ::Position
		   via: trajectory ::(sequence-of Position) := '())
  (screen:press! finger initial:left initial:top)
  (let ((last-position initial))
    (for p ::Position in trajectory
	 (screen:move! finger p:left p:top
		       (- p:left last-position:left)
		       (- p:top last-position:top))
	 (set! last-position p)))
  (screen:release! finger final:left final:top 0 0))

(define-syntax check
  (syntax-rules (that the movement over from to
		      via transforms into and back)    
    ((check the movement over <document> from <initial>
	    to <final> via <trajectory>)
     (with ((painter (TextPainter)))
       (let* ((document ::Document (with-input-from-string
				       <document>
				     parse-document))
	      (finger ::byte 0)
	      (initial ::Position <initial>)
	      (final ::Position <final>)
	      (trajectory ::(list-of Position) <trajectory>)
	      (overlay (bordered
			(Over back: (Dummy document)
			      front: (Movement from: initial
					       to: final
					       via: trajectory)))))
	 (screen:set-content! (DocumentEditor
			       document: document))
	 (snapshot overlay)
	 (swipe! finger from: initial to: final via: trajectory)
	 (snapshot overlay))))

    ((check the movement over <document> from <initial>
	    to <final> via <trajectory> and back)
     (with ((painter (TextPainter)))
       (let* ((document ::Document (with-input-from-string
				       <document>
				     parse-document))
	      (finger ::byte 0)
	      (initial ::Position <initial>)
	      (final ::Position <final>)
	      (trajectory ::(list-of Position) <trajectory>)
	      (overlay (bordered
			(Over back: (Dummy document)
			      front: (Movement from: initial
					       to: final
					       via: trajectory)))))
	 (screen:set-content! (DocumentEditor
			       document: document))
	 (snapshot overlay)
	 (swipe! finger from: initial to: final via: trajectory)
	 (snapshot overlay)
	 (swipe! finger from: final to: initial via:
		 (reverse trajectory))
	 (snapshot overlay)
	 )))
    
    ((check the movement over <document> from <initial>
	    to <final>)
     (check the movement over <document> from <initial>
	    to <final> via (list <initial> <final>)))

    ((check the movement over <document> from <initial>
	    to <final> and back)
     (check the movement over <document> from <initial>
	    to <final> via (list <initial> <final>) and back))

    ((check that the movement over <document> from <initial>
	    to <final> via <trajectory> transforms <original>
	    into <result> and back)
     (with ((painter (TextPainter)))
       (let* ((document ::Document (with-input-from-string
				       <document>
				     parse-document))
	      (finger ::byte 0)
	      (initial ::Position <initial>)
	      (final ::Position <final>)
	      (trajectory ::(list-of Position) <trajectory>)
	      (overlay (bordered
			(Over back: (Dummy document)
			      front: (Movement from: initial
					       to: final
					       via: trajectory)))))
	 (screen:set-content! (DocumentEditor
			       document: document))
	 (e.g. (snapshot overlay) ===> <original>)
	 (swipe! finger from: initial to: final via: trajectory)
	 (e.g. (snapshot overlay) ===> <result>)
	 (swipe! finger from: final to: initial
		 via: (reverse trajectory))
	 (e.g. (snapshot overlay) ===> <original>))))

    ((check that the movement over <document> from <initial>
	    to <final> transforms <original>
	    into <result> and back)
     (check that the movement over <document> from <initial>
	    to <final> via (list <initial> <final>)
	    transforms <original> into <result> and back))
    
    ((check that the movement over <document> from <initial>
	    to <final> via <trajectory> transforms <original>
	    into <result>)
     (with ((painter (TextPainter)))
       (let* ((document ::Document (with-input-from-string
				       <document>
				     parse-document))
	      (finger ::byte 0)
	      (initial ::Position <initial>)
	      (final ::Position <final>)
	      (trajectory ::(list-of Position) <trajectory>)
	      (overlay (bordered
			(Over back: (Dummy document)
			      front: (Movement from: initial
					       to: final
					       via: trajectory)))))
	 (screen:set-content! (DocumentEditor
			       document: document))
	 (e.g. (snapshot overlay) ===> <original>)
	 (swipe! finger from: initial to: final via: trajectory)
	 (e.g. (snapshot overlay) ===> <result>))))

    ((check that the movement over <document> from <initial>
	    to <final> transforms <original>
	    into <result>)
     
     (check that the movement over <document> from <initial>
	    to <final> via (list <initial> <final>)
	    transforms <original> into <result>))    
     ))

(check that the movement over "(define (! n)
(if (<= n 0)
  1
 (* n (! (- n 1)))))"
       from (Position left: 29 top: 3)
       to (Position left: 41 top: 7)
       transforms "
╔══════════════════════════════════════════════╗
║╭        ╭     ╮               ╮              ║
║│ define │ ! n │               │              ║
║│        ╰     ╯           ↘   ↙              ║
║│ ╭    ╭        ╮            ✶⠢⣀              ║
║│ │ if │ <= n 0 │          ↗ │ ↖⠑⠢⣀           ║
║│ │    ╰        ╯            │ │   ⠑⠢⣀        ║
║│ │                          │ │      ⠑↖⣀  ↗  ║
║│ │   1                      │ │         ✶    ║
║│ │                          │ │       ↙   ↘  ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │              ║
║│ │  │ * n │ ! │ - n 1 │ │ │ │ │              ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯              ║
╚══════════════════════════════════════════════╝
" into "
╔════════════════════════════════════════════════════╗
║╭        ╭     ╮                           ╮        ║
║│ define │ ! n │                           │        ║
║│        ╰     ╯           ↘   ↙           │        ║
║│ ╭                          ✶⠢⣀         ╮ │        ║
║│ │                        ↗   ↖⠑⠢⣀      │ │        ║
║│ │                                ⠑⠢⣀   │ │        ║
║│ │                                   ⠑↖⣀│ ↗        ║
║│ │    ╭        ╮                        ✶ │        ║
║│ │ if │ <= n 0 │                      ↙ │ ↘        ║
║│ │    ╰        ╯                        │ │        ║
║│ │                                      │ │        ║
║│ │   1                                  │ │        ║
║│ │                                      │ │        ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮             │ │        ║
║│ │  │ * n │ ! │ - n 1 │ │ │             │ │        ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯             ╯ ╯        ║
╚════════════════════════════════════════════════════╝
" and back)
  
(check that the movement over "(define (! n)
(if (<= n 0)
  1
 (* n (! (- n 1)))))"
       from (Position left: 29 top: 6)
       to (Position left: 41 top: 10)
       transforms "
╔══════════════════════════════════════════════╗
║╭        ╭     ╮               ╮              ║
║│ define │ ! n │               │              ║
║│        ╰     ╯               │              ║
║│ ╭    ╭        ╮            ╮ │              ║
║│ │ if │ <= n 0 │            │ │              ║
║│ │    ╰        ╯          ↘ │ ↙              ║
║│ │                          ✶⠢⣀              ║
║│ │   1                    ↗ │ ↖⠑⠢⣀           ║
║│ │                          │ │   ⠑⠢⣀        ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │      ⠑↖⣀  ↗  ║
║│ │  │ * n │ ! │ - n 1 │ │ │ │ │         ✶    ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯       ↙   ↘  ║
║                                              ║
╚══════════════════════════════════════════════╝
" into "
╔════════════════════════════════════════════════════╗
║╭        ╭     ╮                           ╮        ║
║│ define │ ! n │                           │        ║
║│        ╰     ╯                           │        ║
║│ ╭    ╭        ╮                        ╮ │        ║
║│ │ if │ <= n 0 │                        │ │        ║
║│ │    ╰        ╯          ↘   ↙         │ │        ║
║│ │                          ✶⠢⣀         │ │        ║
║│ │                        ↗   ↖⠑⠢⣀      │ │        ║
║│ │                                ⠑⠢⣀   │ │        ║
║│ │                                   ⠑↖⣀│ ↗        ║
║│ │                                      ✶ │        ║
║│ │   1                                ↙ │ ↘        ║
║│ │                                      │ │        ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮             │ │        ║
║│ │  │ * n │ ! │ - n 1 │ │ │             │ │        ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯             ╯ ╯        ║
╚════════════════════════════════════════════════════╝
" and back)

(check that the movement over "(define (! n)
(if (<= n 0)
  1
 (* n (! (- n 1)))))"
       from (Position left: 29 top: 8)
       to (Position left: 41 top: 12)
       transforms "
╔══════════════════════════════════════════════╗
║╭        ╭     ╮               ╮              ║
║│ define │ ! n │               │              ║
║│        ╰     ╯               │              ║
║│ ╭    ╭        ╮            ╮ │              ║
║│ │ if │ <= n 0 │            │ │              ║
║│ │    ╰        ╯            │ │              ║
║│ │                          │ │              ║
║│ │   1                    ↘ │ ↙              ║
║│ │                          ✶⠢⣀              ║
║│ │  ╭     ╭   ╭       ╮ ╮ ↗ │ ↖⠑⠢⣀           ║
║│ │  │ * n │ ! │ - n 1 │ │ │ │ │   ⠑⠢⣀        ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯      ⠑↖⣀  ↗  ║
║                                         ✶    ║
║                                       ↙   ↘  ║
║                                              ║
╚══════════════════════════════════════════════╝
" into "
╔════════════════════════════════════════════════════╗
║╭        ╭     ╮                           ╮        ║
║│ define │ ! n │                           │        ║
║│        ╰     ╯                           │        ║
║│ ╭    ╭        ╮                        ╮ │        ║
║│ │ if │ <= n 0 │                        │ │        ║
║│ │    ╰        ╯                        │ │        ║
║│ │                                      │ │        ║
║│ │   1                    ↘   ↙         │ │        ║
║│ │                          ✶⠢⣀         │ │        ║
║│ │                        ↗   ↖⠑⠢⣀      │ │        ║
║│ │                                ⠑⠢⣀   │ │        ║
║│ │                                   ⠑↖⣀│ ↗        ║
║│ │                                      ✶ │        ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮           ↙ │ ↘        ║
║│ │  │ * n │ ! │ - n 1 │ │ │             │ │        ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯             ╯ ╯        ║
╚════════════════════════════════════════════════════╝
" and back)

(check that the movement over "(define (! n)
(if (<= n 0)
  1
 (* n (! (- n 1)))))"
       from (Position left: 29 top: 11)
       to (Position left: 41 top: 15)
       transforms "
╔══════════════════════════════════════════════╗
║╭        ╭     ╮               ╮              ║
║│ define │ ! n │               │              ║
║│        ╰     ╯               │              ║
║│ ╭    ╭        ╮            ╮ │              ║
║│ │ if │ <= n 0 │            │ │              ║
║│ │    ╰        ╯            │ │              ║
║│ │                          │ │              ║
║│ │   1                      │ │              ║
║│ │                          │ │              ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │              ║
║│ │  │ * n │ ! │ - n 1 │ │ ↘ │ ↙              ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ✶⠢⣀              ║
║                           ↗   ↖⠑⠢⣀           ║
║                                   ⠑⠢⣀        ║
║                                      ⠑↖⣀  ↗  ║
║                                         ✶    ║
║                                       ↙   ↘  ║
║                                              ║
╚══════════════════════════════════════════════╝
" into "
╔════════════════════════════════════════════════════╗
║╭        ╭     ╮                           ╮        ║
║│ define │ ! n │                           │        ║
║│        ╰     ╯                           │        ║
║│ ╭    ╭        ╮                        ╮ │        ║
║│ │ if │ <= n 0 │                        │ │        ║
║│ │    ╰        ╯                        │ │        ║
║│ │                                      │ │        ║
║│ │   1                                  │ │        ║
║│ │                                      │ │        ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮             │ │        ║
║│ │  │ * n │ ! │ - n 1 │ │ ↘   ↙         │ │        ║
║│ │  ╰     ╰   ╰       ╯ ╯ ╯ ✶⠢⣀         │ │        ║
║│ │                        ↗   ↖⠑⠢⣀      │ │        ║
║│ │                                ⠑⠢⣀   │ │        ║
║│ │                                   ⠑↖⣀│ ↗        ║
║╰ ╰                                      ✶ ╯        ║
║                                       ↙   ↘        ║
║                                                    ║
╚════════════════════════════════════════════════════╝
" and back)

(check that the movement over "(define (! n)
(if (<= n 0)
  1
 (* n (! (- n 1)))))"
       from (Position left: 9 top: 1) 
       to (Position left: 36 top: 3)
       via (list (Position left: 9
			   top: 1)
		 (Position left: 21
			   top: 7)
		 (Position left: 36
			   top: 3))
       transforms "
╔═════════════════════════════════════════╗
║╭      ↘ ╭ ↙   ╮               ╮         ║
║│ define ✶⢄! n │               │         ║
║│      ↗ ╰ ↖⢄  ╯               │  ↖   ↗  ║
║│ ╭    ╭     ⠑⢄ ╮            ╮ │ ⣀⠤⠒✶    ║
║│ │ if │ <= n 0⠑⢄            ⣀⠤⠒⠉ ↙   ↘  ║
║│ │    ╰        ╯⠑⢄      ⢀⡠⠔⠊│ │         ║
║│ │                ⠑⢄⢀⡠⠔⠊⠁   │ │         ║
║│ │   1              ⠁       │ │         ║
║│ │                          │ │         ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │         ║
║│ │  │ * n │ ! │ - n 1 │ │ │ │ │         ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯         ║
╚═════════════════════════════════════════╝
" into "
╔═══════════════════════════════════════════════╗
║╭      ↘   ↙                   ╮╭     ╮        ║
║│ define ✶⢄                    ││ ! n │        ║
║│      ↗   ↖⢄                  │╰ ↖   ↗        ║
║│ ╭    ╭     ⠑⢄ ╮            ╮ │ ⣀⠤⠒✶          ║
║│ │ if │ <= n 0⠑⢄            ⣀⠤⠒⠉ ↙   ↘        ║
║│ │    ╰        ╯⠑⢄      ⢀⡠⠔⠊│ │               ║
║│ │                ⠑⢄⢀⡠⠔⠊⠁   │ │               ║
║│ │   1              ⠁       │ │               ║
║│ │                          │ │               ║
║│ │  ╭     ╭   ╭       ╮ ╮ ╮ │ │               ║
║│ │  │ * n │ ! │ - n 1 │ │ │ │ │               ║
║╰ ╰  ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯               ║
╚═══════════════════════════════════════════════╝
")

(parameterize  ((the-document (call-with-input-string "\
#|block comment|#

;; line comment

\"some text\"
" parse-document)))
  
  (e.g. (snapshot) ===> "
┌─────────────┐
│block comment│
└─────────────┘
               
┃ line comment 
               
❝┈┈┈┈┈┈┈┈┈┈┈•  
┊ some text ┊  
•┈┈┈┈┈┈┈┈┈┈┈❞  
"))

(parameterize  ((the-document (call-with-input-string "\
('x `x ,x ,@x '() `() ,() ,@())
(',x ',@y ,(a) ,@(b))
" parse-document)))
  (e.g. (snapshot) ===> "
╭ ▗   ┌ ┐           ┏  ┓ ╓  ╖ ╷  ╷  ╷  ╷  ╮
│  x   x   x  ┈┐x┌┈ ┃  ┃ ║  ║ │  │ ┈┤  ├┈ │
╰         └ ┘  └ ┘  ┗  ┛ ╙  ╜ └  ┘  └  ┘  ╯
╭ ▗     ▗       ╷   ╷  ╷   ╷  ╮            
│   x    ┈┐y┌┈  │ a │ ┈┤ b ├┈ │            
╰  └ ┘    └ ┘   └   ┘  └   ┘  ╯            
"))
