(import (srfi :11))
(import (for))
(import (fundamental))
(import (primitive))
(import (conversions))
(import (infix))
(import (extent))
(import (painter))
(import (define-object))
(import (indexable))
(import (cursor))
(import (match))
(import (space))
(import (functions))
(import (mapping))
(import (hash-table))
(import (print))

(define-object (CharPainter)::Painter

  (define shiftLeft ::real 0)
  (define shiftTop ::real 0)

  (define (translate! x::real y::real)::void
    (set! shiftLeft (+ shiftLeft x))
    (set! shiftTop (+ shiftTop y)))
  
  (define (current-translation-left)::real
    shiftLeft)
  
  (define (current-translation-top)::real
    shiftTop)

  (define clipLeft ::real 0)
  (define clipTop ::real 0)
  (define clipWidth ::real +inf.0)
  (define clipHeight ::real +inf.0)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (set! clipLeft left)
    (set! clipTop top)
    (set! clipWidth width)
    (set! clipHeight height))
		 
  (define (current-clip-width)::real
    clipWidth)
  
  (define (current-clip-height)::real
    clipHeight)
  
  (define (current-clip-left)::real
    clipLeft)
  
  (define (current-clip-top)::real
    clipTop)

  (define (with-clip w::real h::real action::(maps () to: void))::void
    (let ((painter (the-painter)))
      (let ((x0 clipLeft)
	    (y0 clipTop)
	    (w0 clipWidth)
            (h0 clipHeight)
            (x shiftLeft)
	    (y shiftTop))
	(invoke painter 'clip! x y w h)
        (action)
	(invoke painter 'clip! x0 y0 w0 h0))))
  
  (define markedCursorPosition ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)::void
    (set! markedCursorPosition:left (+ shiftLeft +left))
    (set! markedCursorPosition:top (+ shiftTop +top))
    )

  (define (cursor-position)::Position
    markedCursorPosition)

  (define (cursor-height)::real 1)
  
  (define (space-width)::real 1)
  
  (define (paren-width)::real 2)

  (define (min-box-height)::real 3)
  
  (define (min-line-height)::real 1)
  
  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)

  (define (draw-horizontal-split! top::real)::void
    (for i from (max 0 (current-clip-left))
      below (min (current-width) clipWidth)
      (put! #\─ top i)))
  
  (define (draw-vertical-split! left::real)::void
    (for i from (max 0 (current-clip-top))
      below (min (current-height) clipHeight)
      (put! #\│ i left)))

  (define (horizontal-split-height)::real 1)
  
  (define (vertical-split-width)::real 1)
  
  (define (draw-horizontal-bar! width::real)::void
    (for i from 0 below width
         (when (eq? (get -1 i) #\space)
           (put! #\_ -1 i))))
   
  (define (draw-vertical-bar! height::real)::void
    (put! #\╷ 0 0)
    (for i from 1 below (- height 1)
         (put! #\│ i 0))
    (put! #\╵ (- height 1) 0))

  (define (draw-custom-box!
	   top-left::gnu.text.Char
	   top-right::gnu.text.Char
	   bar::gnu.text.Char ;;│┃┆┇┊┋
	   bottom-left::gnu.text.Char
	   bottom-right::gnu.text.Char
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (let-values (((selection-start selection-end)
		  (the-selection)))
	(when (and (pair? (the-cursor))
		   (equal? context (cdr (the-cursor))))
	  (match (head (the-cursor))
	    (#\[ (mark-cursor! 0 1))
	    (#\] (mark-cursor! (- width 1) (- height 2)))
	    (_ (values))))
	(when (and (pair? selection-start)
		   (equal? (tail selection-start) context)
		   (is (head selection-start) in '(#\[ #\])))
	  (enter-selection-drawing-mode!))
	(put! top-left 0 0)
	(for i from 1 to (- height 2)
             (put! bar i 0))
	(put! bottom-left (- height 1) 0)
	
	(put! top-right 0 (- width 1))
	(for i from 1 to (- height 2)
             (put! bar i (- width 1)))
	(put!  bottom-right (- height 1) (- width 1))

	(when (and (pair? selection-end)
		   (equal? (tail selection-end) context)
		   (is (head selection-end) in '(#\[ #\])))
	  (exit-selection-drawing-mode!))
	))
  
  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (draw-custom-box!
     #\╭                             #\╮
     (cond 
      ((= current-comment-level 0)   #\│)
      ((even? current-comment-level) #\┆)
      (else                          #\┊))
     #\╰                             #\╯
     width height context))

  (define (draw-quote-box! width::real
			   height::real
			   context::Cursor)
    ::void
    (draw-custom-box!
     #\┏                             #\┓
     (cond 
      ((= current-comment-level 0)   #\┃)
      ((even? current-comment-level) #\┇)
      (else                          #\┋))
     #\┗                             #\┛
     width height context))

  (define (quote-paren-width)::real 2)

  (define (draw-quote-markers! width::real
			       height::real
			       context::Cursor)
    ::void
    (let-values (((selection-start selection-end)
		  (the-selection)))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 0))
	  (#\] (mark-cursor! (+ width 1) 0))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(enter-selection-drawing-mode!))

      (put! #\▗ #;◢ #;◤ 0 0)
      ;;(put! #\▖ #;◣ #;◥ 0 (+ width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(exit-selection-drawing-mode!))))

  (define (quote-marker-width)::real 1)

  (define (draw-quasiquote-box! width::real
				height::real
				context::Cursor)
    ::void
    (draw-custom-box!
     #\╓ #\╖
     #\║
     #\╙ #\╜
     width height context))

  (define (quasiquote-paren-width)::real 2)

  (define (draw-quasiquote-markers! width::real
				    height::real
				    context::Cursor)
    ::void
    (let-values (((selection-start selection-end)
		  (the-selection)))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 0))
	  (#\] (mark-cursor! (+ width 1) 0))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(enter-selection-drawing-mode!))
      
      (put! #\┌ 0 0)
      ;;(put! #\╵ 1 0)
      (put! #\┐ 0 (+ width 1))
      ;;(put! #\╵ 1 (+ width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(exit-selection-drawing-mode!))))

  (define (quasiquote-marker-width)::real 1)
  
  (define (draw-unquote-box! width::real
			     height::real
			     context::Cursor)
    ::void
    (draw-custom-box!
     #\╷                             #\╷
     (cond 
      ((= current-comment-level 0)   #\│)
      ((even? current-comment-level) #\┆)
      (else                          #\┊))
     #\└                             #\┘
     width height context))

  (define (unquote-paren-width)::real 2)

  (define (draw-unquote-markers! width::real
				 height::real
				 context::Cursor)
    ::void
    (let-values (((selection-start selection-end)
		  (the-selection)))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 (- height 1)))
	  (#\] (mark-cursor! (+ width 1) (- height 1)))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(enter-selection-drawing-mode!))

      ;;(put! #\╷ (- height 2) 0)
      (put! #\└ (- height 1) 0)
      ;;(put! #\╷ (- height 2) (+ width 1))
      (put! #\┘ (- height 1) (+ width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(exit-selection-drawing-mode!))))
  
  (define (unquote-marker-width)::real 1)
  
  (define (draw-unquote-splicing-box!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (with-translation (1 0)
      (draw-unquote-box! (- width 2) height context))

    (let-values (((selection-start selection-end)
		  (the-selection)))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start)
			 context)
		 (is (head selection-start)
		     in '(#\[ #\])))
	(enter-selection-drawing-mode!))

      (put! #\┈ (- height 2) 0)	
      (put! #\┤ (- height 2) 1)

      (put! #\├ (- height 2) (- width 2))
      (put! #\┈ (- height 2) (- width 1))
      
      (when (and (pair? selection-end)
		 (equal? (tail selection-end)
			 context)
		 (is (head selection-end)
		     in '(#\[ #\])))
	(exit-selection-drawing-mode!))))

  (define (unquote-splicing-paren-width)::real 3)

  (define (draw-unquote-splicing-markers!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (let-values (((selection-start selection-end)
		  (the-selection)))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 1 (- height 1)))
	  (#\] (mark-cursor! (+ width 2) (- height 1)))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(enter-selection-drawing-mode!))
      
      (put! #\┈ (- height 2) 0)
      (put! #\┐ (- height 2) 1)
      (put! #\└ (- height 1) 1)
      (put! #\┈ (- height 2) (+ width 3))
      (put! #\┌ (- height 2) (+ width 2))
      (put! #\┘ (- height 1) (+ width 2))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end)
			 context)
		 (is (head selection-end)
		     in '(#\[ #\])))
	(exit-selection-drawing-mode!))))

  (define (unquote-splicing-marker-width)::real 2)

  (define (draw-custom-rectangle!
	   top-left::gnu.text.Char
	   horizontal::gnu.text.Char
	   top-right::gnu.text.Char
	   vertical::gnu.text.Char
	   bottom-left::gnu.text.Char
	   bottom-right::gnu.text.Char
	   width::real
	   height::real)
    ::void
    (put! top-left 0 0)
    (for i from 1 to (- height 2)
         (put! vertical i 0)
	 (for j from 1 to (- width 2)
	      (put! #\space i j)))
    (put! bottom-left (- height 1) 0)

    (for i from 1 to (- width 2)
         (put! horizontal 0 i)
	 (put! horizontal (- height 1) i))
    
    (put! top-right 0 (- width 1))
    (for i from 1 to (- height 2)
         (put! vertical i (- width 1)))
    (put! bottom-right (- height 1) (- width 1)))
  
  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (draw-custom-rectangle!
     #\╭ #\─ #\╮
     #\│
     #\╰     #\╯
     width height))

  (define (draw-rectangle! width::real
			   height::real)
    ::void
    (draw-custom-rectangle!
     #\┌ #\─ #\┐
     #\│
     #\└     #\┘
     width height))

  (define (draw-popup! width::real height::real)::void
    (draw-rounded-rectangle! width height))

  (define (horizontal-popup-margin)::real 1)
  (define (vertical-popup-margin)::real 1)
  
  (define 4pix-code
    (let ((4pix (mapping (4p::char)::int 0)))
      (set! (4pix #\space) #b0000)
      (set! (4pix #\▘) #b0001)
      (set! (4pix #\▝) #b0010)
      (set! (4pix #\▀) #b0011)
      (set! (4pix #\▖) #b0100)
      (set! (4pix #\▌) #b0101)
      (set! (4pix #\▞) #b0110)
      (set! (4pix #\▛) #b0111)
      (set! (4pix #\▗) #b1000)
      (set! (4pix #\▚) #b1001)
      (set! (4pix #\▐) #b1010)
      (set! (4pix #\▜) #b1011)
      (set! (4pix #\▄) #b1100)
      (set! (4pix #\▙) #b1101)
      (set! (4pix #\▟) #b1110)
      (set! (4pix #\█) #b1111)
      4pix))

  (define 4pix ::char[]
    (char[] #\space
	 #\▘ #\▝ #\▀ #\▖ #\▌
	 #\▞ #\▛ #\▗ #\▚ #\▐
	 #\▜ #\▄ #\▙ #\▟ #\█))

  (define (4pix-set! x4::int y4::int)::void
    (let* ((x ::int (quotient x4 2))
           (h ::int (remainder x4 2))
           (y ::int (quotient y4 2))
	   (v ::int (remainder y4 2))
	   (c ::char (get y x))
	   (existing-code ::int (4pix-code c))
	   (mask ::int (arithmetic-shift
			1 (+ (* 2 v) h)))
	   (new-code ::int (bitwise-ior
			    existing-code mask))
	   (c* ::char (4pix new-code)))
      (put! c* y x)))

  (define (draw-line-4pix! x0::real y0::real
			   x1::real y1::real)
    ::void
    (let* ((x1-x0 ::real (- x1 x0))
           (y1-y0 ::real (- y1 y0))
	   (angle ::real (atan y1-y0 x1-x0)))
      (cond
       ((is -pi/4 <= angle <= pi/4)
	(let ((slope ::real (tan angle))
              (x0 ::int (round x0)))
          (for i from 0 to (as int (ceiling
				    x1-x0))
	       (let* ((x (+ x0 i))
	              (y (+ y0 (* slope i))))
		 (4pix-set!
		  x (as int (round y)))))))
       ((is pi/4 <= angle <= (* 3 pi/4))
	(let ((slope ::real (/ (cos angle)
			       (sin angle)))
              (y0 ::int (round y0)))
          (for j from 0 to (as int
			       (ceiling y1-y0))
	       (let ((x (+ x0 (* slope j)))
	             (y (+ y0 j)))
		 (4pix-set! (as int (round x))
			    y)))))
       (else
	(draw-line-4pix! x1 y1 x0 y0)))))

  (define (draw-line! x0::real y0::real
		      x1::real y1::real)
    ::void
    (draw-line-4pix! (* x0 2) (* y0 2)
		     (* x1 2) (* y1 2)))
  
  (define (draw-quoted-text! s::CharSequence
			     context::Cursor)
    ::void
    (let ((extent ::Extent (string-extent s)))
      (put! #\❝ 0 0)
      (put! #\• 0 (+ extent:width 3))
      (for i from 1 below (+ extent:width 3)
	   (put! #\┈ (+ extent:height 1) i)
	   (put! #\┈ 0 i))
      (for i from 1 to extent:height
	   (put! #\┊ i 0)
	   (put! #\┊ i (+ extent:width 3)))
      (put! #\• (+ extent:height 1) 0)
      (put! #\❞ (+ extent:height 1)
	    (+ extent:width 3))
      (with-translation (2 1)
	  (draw-string! s context))
      (put! #\❞ (+ extent:height 1)
	    (+ extent:width 3))))

  (define (draw-string! text::CharSequence
			context::Cursor)
    ::void
    (let-values (((selection-start
		   selection-end)
		  (the-selection)))
      (let ((focused?
	     (and (pair? (the-cursor))
		  (equal? context
			  (cdr (the-cursor)))))
	    (enters-selection-drawing-mode?
	     (and (pair? selection-start)
		  (equal? (tail selection-start)
			  context)))
	    (exits-selection-drawing-mode?
	     (and (pair? selection-end)
		  (equal?
		   (tail selection-end)
		   context)))
	    (row ::int 0)
	    (col ::int 0)
	    (n ::int 0))
	
	(define (handle-cursor-and-selection!)
	  (when (and enters-selection-drawing-mode?
		     (eqv? n (head selection-start)))
	    (enter-selection-drawing-mode!))
	  (when (and exits-selection-drawing-mode?
		     (eqv? n (head selection-end)))
	    (exit-selection-drawing-mode!))
	  (when (and focused? (eqv? n (car (the-cursor))))
	    (mark-cursor! col row)))
	
	(for c in text
	  (handle-cursor-and-selection!)
          (cond ((eq? c #\newline)
		 (set! row (+ row 1))
		 (set! col 0))
		(else
		 (put! c row col)
		 (set! col (+ col 1))))
	  
	  (set! n (+ n 1)))
	(handle-cursor-and-selection!))))

  (define (string-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    (let ((end (text:length)))
      (let next ((row ::int 0)
		 (col ::int 0)
		 (n ::int 0))
	(if (or (and (is x = col) (is y <= row))
		(is n >= end))
	    n
	    (let ((c (text n)))
	      (if (eq? c #\newline)
		  (if (is y <= row)
		      n
		      (next (+ row 1)
			    0
			    (+ n 1)))
		  (next row
			(+ col 1)
			(+ n 1))))))))

  (define (draw-caption! caption::CharSequence)
    ::void
    (let ((row ::int 0)
	  (col ::int 0)
	  (n ::int 0))
	(for c in caption
          (cond ((eq? c #\newline)
		 (set! row (+ row 1))
		 (set! col 0))
		(else
		 (put! c row col)
		 (set! col (+ col 1))))
	  
	  (set! n (+ n 1)))))
      
  (define (caption-extent caption::CharSequence)
    ::Extent
    (string-extent caption))

  (define (caption-vertical-margin)::real 1)

  (define (caption-horizontal-margin)::real 2)
  
  (define (quoted-text-extent text::CharSequence)
    ::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: (+ inner:width 4)
	      height: (+ (max inner:height 1)
			 2))))

  (define (quoted-text-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    (string-character-index-under (- x 2)
				  (- y 1)
				  text))
  
  (define (draw-atom! text::CharSequence
		      context::Cursor)
    ::void
    (with-translation (0 1)
	(draw-string! text context)))

  (define (atom-extent text::CharSequence)
    ::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: inner:width
	      height: (max (min-box-height)
			   inner:height))))

  (define (atom-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    (string-character-index-under x (- y 1) text))
  
  (define (get row::real col::real)::char
    #!abstract)

  (define (put! c::char row::real col::real)
    ::void
    #!abstract)

  (define (clear!)::void #!abstract)

  (define (current-width)::real #!abstract)

  (define (current-height)::real #!abstract)

  (define inSelectionDrawingMode ::boolean #f)
  
  (define (enter-selection-drawing-mode!)::void
    (set! inSelectionDrawingMode #t))

  (define (exit-selection-drawing-mode!)::void
    (set! inSelectionDrawingMode #f))
  
  (define (in-selection-drawing-mode?)::boolean
    inSelectionDrawingMode)

  (define current-comment-level ::int 0)
  
  (define (enter-comment-drawing-mode!)::void
    (set! current-comment-level
	  (+ current-comment-level 1)))
  
  (define (exit-comment-drawing-mode!)::void
    (set! current-comment-level
	  (- current-comment-level 1)))

  (define (in-comment-drawing-mode?)::boolean
    (is current-comment-level > 0))
  
  (define (draw-line-comment! text::CharSequence
			      context::Cursor)
    ::void
    (let*-values (((semicolons)
		   (count-while (is _ eqv? #\;)
				text))
		  ((shift skip)
		   (match semicolons 
		     (0 (put! #\⸾ 0 0)
			(values 1 0))
		     (1 (put! #\┃ 0 0)
			(values 1 1))
		     (n (put! #\┣ 0 0)
			(for i from 1 below n
			     (put! #\━ 0 i))
			(values (- n 1) n))))
		  ((end) (string-length text)))
      (with-translation (shift 0)
	  (draw-string! (substring text skip end)
			 context))))
  
  (define (line-comment-extent
	   text::CharSequence)
    ::Extent
    (let ((semicolons
	   (count-while (is _ eqv? #\;) text)))
      (Extent width:
	      (match semicolons
		(0 (+ (string-length text) 1))
		(1 (string-length text))
		(n (- (string-length text) 1)))
	      height: 1)))

  (define (line-comment-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    (let* ((semicolons (count-while
			(is _ eqv? #\;) text)))
      (string-character-index-under x y text)))

  (define (draw-block-comment! text::CharSequence
			       context::Cursor)
    ::void
    (let ((outer ::Extent (block-comment-extent
			   text)))
      (draw-rectangle! outer:width outer:height)
      (with-translation (1 1)
	  (draw-string! text context))))
  
  (define (block-comment-extent
	   text::CharSequence)
    ::Extent
    (let ((inner ::Extent (string-extent text)))
      (Extent width: (+ inner:width 2)
	      height: (+ inner:height 2))))
  
  (define (block-comment-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    (string-character-index-under (- x 1)
				  (- y 1)
				  text))

  (define 4dirs-code
    (let ((4dirs (mapping (4p::char)::int 0)))
      (set! (4dirs #\space) #b0000)
      (set! (4dirs #\╵) #b0001)
      (set! (4dirs #\╶) #b0010)
      (set! (4dirs #\└) #b0011)
      (set! (4dirs #\╷) #b0100)
      (set! (4dirs #\│) #b0101)
      (set! (4dirs #\┌) #b0110)
      (set! (4dirs #\├) #b0111)
      (set! (4dirs #\╴) #b1000)
      (set! (4dirs #\┘) #b1001)
      (set! (4dirs #\─) #b1010)
      (set! (4dirs #\┴) #b1011)
      (set! (4dirs #\┐) #b1100)
      (set! (4dirs #\┤) #b1101)
      (set! (4dirs #\┬) #b1110)
      (set! (4dirs #\┼) #b1111)
      4dirs))

  (define 4dirs ::char[]
    (char[] #\space
	 #\╵ #\╶ #\└ #\╷ #\│
	 #\┌ #\├ #\╴ #\┘ #\─
	 #\┴ #\┐ #\┤ #\┬ #\┼))

  (define (4dirs-put! c::char x::int y::int)::void
    (let* ((current (get y x))
	   (new (4dirs (bitwise-ior
		    (4dirs-code current)
		    (4dirs-code c)))))
      (put! new y x)))
  
  (define (draw-horizontal-grid! width::real)::void
    (4dirs-put! #\╶ 0 0)
    (for i from 1 below (- width 1)
	 (4dirs-put! #\─ i 0))
    (4dirs-put! #\╴ (- width 1) 0))
  
  (define (draw-vertical-grid! height::real)::void
    (4dirs-put! #\╷ 0 0)
    (for i from 1 below (- height 1)
	 (4dirs-put! #\│ 0 i))
    (4dirs-put! #\╵ 0 (- height 1)))

  (define (grid-border)::real 1)

  (define (fill-grid-cell! width::real height::real)::void
    (for row from 1 below (- height 1)
	 (for column from 1 below (- width 1)
	      (put! #\space row column))))
  
  (define (draw-point! left::real top::real
		       color-rgba::int)
    ::void
    #!abstract)

  )

(define-object (TextPainter)::Painter
  (define width ::int 0)
  (define height ::int 0)
  (define data ::char[])

  (define modifier ::procedure
    (mapping (key ::int)::char #!null))

  (define current-modifier #!null)
  
  (define (get row::real col::real)::char
    (let ((x (+ col shiftLeft))
          (y (+ row shiftTop)))
      (if (and (is 0 <= x < width)
               (is 0 <= y < height))
          (data (+ (* width y) x))
          #\space)))

  (define (draw-string! text::CharSequence
			context::Cursor)
    ::void
    (when (invoke-special
	   CharPainter (this)
	   'in-comment-drawing-mode?)
      (set! current-modifier
	    (if (even?
		 (as int
		     (slot-ref
		      (this)
		      'current-comment-level)))
		#\x338
		#\x336)))
    (invoke-special CharPainter (this)
		    'draw-string! text context)
    (set! current-modifier #!null))
  
  (define (put! c::char row::real col::real)
    ::void
    (let ((x (+ col shiftLeft))
          (y (+ row shiftTop))
	  (left (max 0 clipLeft))
	  (top (max 0 clipTop)))
      (when (and (is left <= x < (+ left
				    clipWidth))
                 (is top <= y < (+ top
				   clipHeight)))
	(when (or (is x >= width)
                  (is y >= height))
          (let* ((new-width (if (is x >= width)
				(+ x 1)
				width))
                 (new-height (if (is y >= height)
                                 (+ y 1)
                                 height))
                 (new-data (char[] length:
				(* new-width
				   new-height))))
            (for line from 0 below new-height
                 (for column from 0 below new-width
		      (set! (new-data (+ (* new-width
					    line)
                                         column))
                        (if (and (is column < width)
                                 (is line < height))
                            (data (+ (* width line)
                                     column))
                            #\space))))
            (set! width new-width)
            (set! height new-height)
            (set! data new-data)))
	(let ((n (+ (* width y) x)))
	  (set! (data n) c)
	  (when current-modifier
	    (set! (modifier n) current-modifier)))
	(when (and inSelectionDrawingMode
		   (is (+ y 1) < height))
	  (set! (data (+ (* width (+ y 1)) x)) #\~))
	)))

    (define (clear!)::void
      (reset! modifier)
      (for line from 0 below height
           (for column from 0 below width
		(set! (data (+ (* line width)
			       column))
                      #\space)))
      (set! shiftLeft 0)
      (set! shiftTop 0))

  (define (mark-cursor! +left::real +top::real)::void
    (invoke-special CharPainter (this)
		    'mark-cursor! +left +top)
    (match (the-expression)
      (,@Space?
       (put! #\| (+ +top 1) +left))
      (,@Atom?
       (put! #\^ (+ +top 1) +left))
      (_
       (values))))
  
  (define (toString)::String
    (with-output-to-string
      (lambda ()
	(write-char #\newline)
	(for line from 0 below height
             (for column from 0 below width
		  (let* ((n (+ (* line width)
			       column))
			 (c (data n)))
		    (write-char c)
		    (and-let* ((mod (modifier n)))
		      (write-char mod))
                    ))
             (write-char #\newline)))))

  (define (current-width)::real width)

  (define (current-height)::real height)

  (define (draw-point! left::real top::real
		       color-rgba::int)::void
    (put! #\⦿ top left))
  
  (CharPainter))
