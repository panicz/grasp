;; -*- mode: scheme; *preserve-trailing-whitespace*: t -*-


(import
  (language define-interface)
  (language define-type)
  (language define-object)
  (language while)
  (utils conversions)
  (editor interfaces painting)
  (editor interfaces elements)
  (editor types spaces)
  (editor document cursor)
  (editor types primitive)
  
  (editor types extensions extensions)
  (editor types extensions combinators)
  (editor text-painter)
  (editor document parse)
  (language examples)
  (language assert)
  (language infix)
  (language match)
  (utils functions)
  (utils print)
  (editor document documents)
  )

#|
(define parsed (with-input-from-string "\
(#;(a (b c) d) #|efg|# ;hij
 define (! n #|int|#) ; -> int
  (if #;(<= n 1) (is n <= 1)
      1 ; base case
      (* n (! (- n 1)))))" parse-document))
|#

(define document (with-input-from-string "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))

(e.g. (! 5) ===> 120)

(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document))

(set! (the-document) document)

;;(show parsed)


;; this is what we're aiming for:


&{
╭                         ╮
│                ╭─────╮  │
│ ╭───────╮     ╭╯     ╰╮ │
│ │       │     │   B   │ │
│ │   A   │----⯈╰╮     ╭╯ │
│ │       │      ╰─────╯  │
│ ╰───────╯       ╱       │
│        ⯈       ╱        │
│         ╲     ⯈         │
│          ╭───╮          │
│          │ C │          │
│          ╰───╯          │
╰                         ╯
}

;|


;; No dobra, do tej pory ("podejscie funkcyjne") bylismy w stanie
;; wyjasniac sobie kod za pomoca przykladow: dla takiego a takiego
;; wejscia otrzymamy takie a takie wyjscie.
;;
;; Wydaje sie jednak, ze w przypadku systemow interaktywnych
;; to podejscie jest niewystarczajace - ze raczej chcielibysmy
;; moc "opowiadac historie"

(define (grasped program-text::string)::String
  (let ((document (call-with-input-string program-text
					  parse-document)))
    (with ((painter (TextPainter)))
      (parameterize ((the-document document))
      (draw-document! document)
      (painter:toString)))))

(set! painter (TextPainter))

(e.g.
(with ((painter ::TextPainter (TextPainter)))
    (painter:draw-line! 0 0 30 3)
    (painter:draw-line! 3 2 30 15)
    (painter:draw-line! 32 15 34 0)
    (painter:draw-line! 5 15 34 7)
    (painter:toString)) ===> "
▀▀▀▄▄▄▄▖                          ▌
       ▝▀▀▀▀▀▄▄▄▄▖                ▌
   ▀▄            ▝▀▀▀▀▀▄▄▄▄▖     ▐ 
     ▀▄                    ▝▀▀▘  ▐ 
       ▀▄                        ▐ 
         ▀▄                      ▐ 
           ▀▄                    ▌ 
             ▀▄                ▗▄▛▘
               ▀▄▖          ▄▞▀▘ ▌ 
                 ▝▚▖    ▗▄▀▀    ▗▘ 
                   ▝▚▄▞▀▘       ▐  
                 ▄▄▀▘▝▚▖        ▐  
             ▗▄▀▀      ▝▚▖      ▐  
          ▄▞▀▘           ▝▚▖    ▞  
      ▄▄▀▀                 ▝▚▖  ▌  
     ▀                       ▝▘ ▘  
")

(e.g.
 (parameterize ((cell-access-mode CellAccessMode:Evaluating))
   (equal?
    document
    '(((define (! n)
	  (if (<= n 0)
	      1
	      (* n (! (- n 1)))))
	
	(e.g. (! 5) ===> 120)

	(Button action: (lambda () (WARN "button pressed!"))
		label: "Press me!"))))))


(parameterize ((cell-access-mode CellAccessMode:Evaluating))
  (e.g. (the-expression at: '(1 1 1)) ===> define)
  (e.g. (the-expression at: '(1 3 1 1)) ===> !)
  (e.g. (the-expression at: '(3 3 1 1)) ===> n)
  (e.g. (the-expression at: '(1 5 1 1)) ===> if)
  (e.g. (the-expression at: '(1 3 5 1 1)) ===> <=)
  (e.g. (the-expression at: '(5 5 1 1)) ===> 1)
  (e.g. (the-expression at: '(1 7 5 1 1)) ===> *)
  (e.g. (the-expression at: '(3 7 5 1 1)) ===> n)
  (e.g. (the-expression at: '(1 5 7 5 1 1)) ===> !)
  (e.g. (the-expression at: '(1 3 5 7 5 1 1)) ===> -)
  (e.g. (the-expression at: '(3 3 5 7 5 1 1)) ===> n)
  (e.g. (the-expression at: '(5 3 5 7 5 1 1)) ===> 1)
  )

(e.g. (is '(1 1 1) cursor< '(1 3 1 1)))
(e.g. (isnt '(1 3 1 1) cursor< '(1 1 1)))
(e.g. (is '(1 3 1 1) cursor< '(3 3 1 1)))
(e.g. (isnt '(3 3 1 1) cursor< '(1 3 1 1)))
(e.g. (is '(3 3 1 1) cursor< '(1 5 1 1)))
(e.g. (isnt '(1 5 1 1) cursor< '(3 3 1 1)))	

(e.g. (is '(1 3 1 1) cursor< '(3 1 1)))

(e.g.
 (grasped "\
(#;define (! n #|int|#) ;->int
  #;(if #;(<= n 0)
      1 ;base case
      #;(* n #|nested|# ;call
 #;(! #|recursive
step|# (- n 1)))))

#;(e.g. #;(! 5) #;===> 120)
") ===> "
╭        ╭     ┌───┐ ╮ ⸾->int                  ╮
│ d̶e̶f̶i̶n̶e̶ │ ! n │int│ │                         │
│        ╰     └───┘ ╯                         │
│   ╭    ╭        ╮                          ╮ │
│   ┊ i̶f̶ ┆ ≮≠ n̸ 0̸ ┆                          ┊ │
│   ┊    ╰        ╯                          ┊ │
│   ┊         ⸾b̶a̶s̶e̶ ̶c̶a̶s̶e̶                     ┊ │
│   ┊       1̶                                ┊ │
│   ┊                                        ┊ │
│   ┊       ╭     ┌──────┐ ⸾c̸a̸l̸l̸           ╮ ┊ │
│   ┊       ┆ *̸ n̸ │n̸e̸s̸t̸e̸d̸│                 ┆ ┊ │
│   ┊       ┆     └──────┘                 ┆ ┊ │
│   ┊       ┆  ╭   ┌─────────┐ ╭       ╮ ╮ ┆ ┊ │
│   ┊       ┆  ┊ !̶ │r̶e̶c̶u̶r̶s̶i̶v̶e̶│ ┊ -̶ n̶ 1̶ ┊ ┊ ┆ ┊ │
│   ┊       ┆  ┊   │s̶t̶e̶p̶     │ ╰       ╯ ┊ ┆ ┊ │
╰   ╰       ╰  ╰   └─────────┘           ╯ ╯ ╯ ╯
                                                
╭      ╭     ╮          ╮                       
┊ e̶.̶g̶.̶ ┆ !̸ 5̸ ┆ ≠≠≠≯ 1̶2̶0̶ ┊                       
╰      ╰     ╯          ╯                       
")

(e.g. (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))

(e.g. (! 5) ===> 120)
") ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│        ╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
                                       
╭      ╭     ╮          ╮              
│ e.g. │ ! 5 │ ===> 120 │              
╰      ╰     ╯          ╯              
")


(e.g.
 (parameterize ((the-cursor (cursor 0 1 3 1 1))
		(the-selection-anchor (cursor 0 1 3 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│        ╰ ^   ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")


(e.g.
 (parameterize ((the-cursor (cursor 0 2 3 1 1))
		(the-selection-anchor (cursor 0 2 3 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│        ╰  |  ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")

(e.g.
 (parameterize ((the-cursor (cursor 0 1 1 1))
		(the-selection-anchor (cursor 0 1 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│ ^      ╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")


(e.g.
 (parameterize ((the-cursor (cursor 6 1 1 1))
		(the-selection-anchor (cursor 6 1 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│       ^╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")


(e.g.
 (parameterize ((the-selection-anchor (cursor 0 1 1 1))
		(the-cursor (cursor 6 1 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│ ~~~~~~^╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")


(e.g.
 (parameterize ((the-selection-anchor (cursor 1 1 1 1))
		(the-cursor (cursor 4 1 1 1)))
   (grasped "\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
")) ===> "
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│  ~~~^  ╰     ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
")



#|
              11111111112222222222333333333
    012345678901234567890123456789012345678
  0 ╭        ╭     ╮                      ╮
  1 │ define │ ! n │                      │
  2 │        ╰     ╯                      │
  3 │   ╭    ╭        ╮                 ╮ │
  4 │   │ if │ <= n 0 │                 │ │
  5 │   │    ╰        ╯                 │ │
  6 │   │                               │ │
  7 │   │       1                       │ │
  8 │   │                               │ │
  9 │   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
 10 │   │       │ * n │ ! │ - n 1 │ │ │ │ │
 11 ╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
 12                                        
 13 ╭      ╭     ╮          ╮              
 14 │ e.g. │ ! 5 │ ===> 120 │              
 15 ╰      ╰     ╯          ╯              

|#

(e.g. (cursor-under 0 0) ===> (#\[ 1 1)) ; (define ...)
(e.g. (cursor-under 0 11) ===> (#\[ 1 1)) ; (define ...)
(e.g. (cursor-under 38 0) ===> (#\] 1 1)) ; (define ...)
(e.g. (cursor-under 38 11) ===> (#\] 1 1)) ; (define ...)
(e.g. (cursor-under 2 1) ===> (0 1 1 1)) ; define
(e.g. (cursor-under 7 1) ===> (5 1 1 1)) ; define
(e.g. (cursor-under 9 1) ===> (#\[ 3 1 1)) ; (! n)
(e.g. (cursor-under 11 1) ===> (0 1 3 1 1)) ; !
(e.g. (cursor-under 12 1) ===> (0 2 3 1 1)) ; [Space (1)]
(e.g. (cursor-under 15 1) ===> (#\] 3 1 1)) ; (! n)
(e.g. (cursor-under 4 3) ===> (#\[ 5 1 1)) ; (if ...)
(e.g. (cursor-under 4 11) ===> (#\[ 5 1 1)) ; (if ...)
(e.g. (cursor-under 6 4) ===> (0 1 5 1 1)) ; if
(e.g. (cursor-under 9 4) ===> (#\[ 3 5 1 1)) ; (<= n 0)
(e.g. (cursor-under 18 4) ===> (#\] 3 5 1 1)) ; (<= n 0)
(e.g. (cursor-under 12 7) ===> (0 5 5 1 1)) ; 1
(e.g. (cursor-under 12 10) ===> (#\[ 7 5 1 1)) ; (* n ...)
(e.g. (cursor-under 0 14) ===> (#\[ 3 1)) ; (e.g. ...)
(e.g. (cursor-under 24 15) ===> (#\] 3 1)) ; (e.g. ...)
(e.g. (cursor-under 7 14) ===> (#\[ 3 3 1)) ; (! 5)

;; (DUMP (cursor-under 16 1))
;; (DUMP (cursor-under 17 1))
;; (DUMP (cursor-under 2 3))
;; (DUMP (cursor-under 3 4))

;; (exit)

(e.g. (grasped "\
(head
.
tail)") ===> "
╭      ╮
│ head │
│ ____ │
│      │
│ tail │
╰      ╯
")

(e.g. (grasped "\
(((a b)
(c d))  .  ((e f)
(g h)))") ===> "
╭ ╭ ╭     ╮ ╮  ╷  ╭ ╭     ╮ ╮ ╮
│ │ │ a b │ │  │  │ │ e f │ │ │
│ │ ╰     ╯ │  │  │ ╰     ╯ │ │
│ │ ╭     ╮ │  │  │ ╭     ╮ │ │
│ │ │ c d │ │  │  │ │ g h │ │ │
╰ ╰ ╰     ╯ ╯  ╵  ╰ ╰     ╯ ╯ ╯
")

(e.g. (grasped "\
((() . ())
    (   )
.
     ( ))") ===> "
╭ ╭ ╭  ╮ ╷ ╭  ╮ ╮ ╮
│ │ │  │ │ │  │ │ │
│ ╰ ╰  ╯ ╵ ╰  ╯ ╯ │
│     ╭     ╮     │
│     │     │     │
│ ____╰_____╯____ │
│      ╭   ╮      │
│      │   │      │
╰      ╰   ╯      ╯
")
