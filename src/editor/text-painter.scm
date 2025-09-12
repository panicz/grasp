(module-name (editor text-painter))

(import (srfi :11))
(import (language assert))
(import (language while))
(import (language for))
(import (language fundamental))
(import (language infix))
(import (language define-object))
(import (language define-type))
(import (language match))
(import (language mapping))
(import (utils conversions))
(import (utils functions))
(import (utils hash-table))
(import (utils print))
(import (editor types primitive))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor document cursor))
(import (editor types spaces))

(define-object (CharPainter)::Painter

  (define (playing? animation::Animation)::boolean #!abstract)
  
  (define (play! animation::Animation)::void #!abstract)

  (define (stop-playing! animation::Animation)::void #!abstract)

  (define (with-intensity i::float action::(maps () to: void))::void
    #!abstract)

  (define shiftLeft ::real 0)
  (define shiftTop ::real 0)

  (define (translate! x::real y::real)::void
    (set! shiftLeft (+ shiftLeft (nearby-int (* x horizontal-stretch))))
    (set! shiftTop (+ shiftTop (nearby-int (* y vertical-stretch)))))

  (define (rotate! angle::real)::void
    (error "Rotation not supported in textual painters"))

  (define (scale! factor::real)::void
    (error "Scaling not supported in textual painters"))

  (define clipLeft ::real 0)
  (define clipTop ::real 0)
  (define clipWidth ::real +inf.0)
  (define clipHeight ::real +inf.0)

  (define (clip! left::real top::real
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
    (let ((x0 clipLeft)
	  (y0 clipTop)
	  (w0 clipWidth)
          (h0 clipHeight)
          (x shiftLeft)
	  (y shiftTop)
	  (w* (nearby-int (* w horizontal-stretch)))
	  (h* (nearby-int (* h vertical-stretch))))
      (clip! x y w* h*)
      (try-finally
       (action)
       (clip! x0 y0 w0 h0))))

  (define (mark-editor-cursor! +left::real +top::real
			       editor::WithCursor)
    ::void
    (let ((t ::Traversal (the-traversal)))
      (editor:mark-cursor! (+ t:parent-left +left)
			   (+ t:parent-top +top 1))))

  (define (editor-cursor-position editor::WithCursor)::Position
    (editor:marked-cursor-position))

  (define (mark-cursor! +left::real +top::real)::void
    (mark-editor-cursor! +left +top (the-editor)))

  (define (marked-cursor-position)::Position
    (editor-cursor-position (the-editor)))

  (define (cursor-height)::real 1)

  (define (space-width)::real 1)

  (define (paren-width)::real 2)

  (define (line-simplification-resolution)::real 3)

  (define (min-box-height)::real 3)

  (define (min-line-height)::real 1)

  (define (module-view-interline)::real 3)

  (define (module-view-interspace)::real 5)
  
  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)

  (define (draw-horizontal-split! top::real)::void
    (for i from (max 0 (current-clip-left))
      below (min (current-width) clipWidth)
      (put! #\█ top i)))

  (define (draw-vertical-split! left::real)::void
    (for i from (max 0 (current-clip-top))
      below (min (current-height) clipHeight)
      (put! #\█ i left)))

  (define (horizontal-split-height)::real 1)

  (define (vertical-split-width)::real 1)

  (define (draw-horizontal-bar! width::real
				highlighted?::boolean)
    ::void
    (let ((width (nearby-int (* width
				horizontal-stretch))))
      (when highlighted?
	(mark-cursor! 0 1)
	(begin-highlight! HighlightType:Selection))
      (for i from 0 below width
           (when (eq? (get -1 i) #\space)
             (put! #\_ -1 i)))
      (when highlighted?
	(end-highlight! HighlightType:Selection))))

  (define (draw-vertical-bar! height::real
			      highlighted?::boolean)
    ::void
    (let ((height (nearby-int (* height
				 horizontal-stretch))))
      (when highlighted?
	(mark-cursor! 0 1)
	(begin-highlight! HighlightType:Selection))
      (put! #\╷ 0 0)
      (for i from 1 below (- height 1)
	   (put! #\│ i 0))
      (put! #\╵ (- height 1) 0)
      (when highlighted?
	(end-highlight! HighlightType:Selection))))

  (define (vertical-scrollbar-width)::real 1)
  (define (horizontal-scrollbar-height)::real 1)

  (define (draw-horizontal-scrollbar!
	   total-width ::real
	   bar-width ::real
	   relative-bar-position ::real)
    ::void
    (for i from 0 below total-width
	 (put! #\space 0 i))
    (let ((bar-position (round (* relative-bar-position total-width))))
      (for i from 0 below bar-height
	   (put! #\━ 0 i))))
  
  (define (draw-vertical-scrollbar!
	   total-height ::real
	   bar-height ::real
	   relative-bar-position ::real)
    ::void
    (for i from 0 below total-height
	 (put! #\space i 0))
    (let ((bar-position (round (* relative-bar-position total-width))))
      (for i from 0 below bar-height
	   (put! #\┃ i 0))))

  (define icon-size ::Extent
    (Extent width: 2
	    height: 1))

  (define (icon-extent)::Extent
    icon-size)

  (define (draw-directory-icon!)::void
    (put! #\D 0 0))

  (define (draw-file-icon!)::void
    (put! #\f 0 0))

  (define press-mark-size ::Extent
    (Extent width: 5
	    height: 3))
  
  (define (press/release-mark-extent)::Extent
    press-mark-size)

  (define (put-around! left::real top::real
		       tl::char tr::char
		       central::char
		       bl::char br::char)
    ::void
    (put! central top left)
    (put! tl (- top 1) (- left 2))
    (put! tr (- top 1) (+ left 2))
    (put! bl (+ top 1) (- left 2))
    (put! br (+ top 1) (+ left 2)))
  
  (define (draw-release-mark! left::real top::real)::void
    (put-around! left top
		 #\↖ #\↗ 
	           #\✶
		 #\↙ #\↘))

  (define (draw-press-mark! left::real top::real)::void
    (put-around! left top
		 #\↘ #\↙
		   #\✶
		 #\↗ #\↖))

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
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end)
	   (width (nearby-int (* width horizontal-stretch)))
	   (height (nearby-int (* height vertical-stretch)))
	   (t (the-traversal)))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	  (match (head (the-cursor))
	    (#\[
	     (set! t:parent-left (+ t:parent-left t:left))
	     (set! t:parent-top (+ t:parent-top t:top))
	     (mark-cursor! 0 0)
	     (set! t:parent-top (- t:parent-top t:top))
	     (set! t:parent-left (- t:parent-left t:left))
	     )
	    (#\]
	     (set! t:parent-left (+ t:parent-left t:left))
	     (set! t:parent-top (+ t:parent-top t:top))
	     (mark-cursor! (- width 1) 0)
	     (set! t:parent-top (- t:parent-top t:top))
	     (set! t:parent-left (- t:parent-left t:left)))
	    (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))
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
	(end-highlight! HighlightType:Selection))
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

  (define (draw-border! width::real
			height::real
			highlight::(subtype-of
				    gnu.text.Char
				    (either
				     #\[
				     #\]
				     #\t
				     #\null)))
    ::void
    (draw-custom-rectangle!
     #\╔ #\═ #\╗
     #\║
     #\╚     #\╝
     width height))

  (define (border-size)::real
    1)

  (define (height/width-ratio)::real
    1/2)
  
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
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 0))
	  (#\] (mark-cursor! (+ width 1) 0))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))

      (put! #\▗ #;◢ #;◤ 0 0)
      ;;(put! #\▖ #;◣ #;◥ 0 (+ width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(end-highlight! HighlightType:Selection))))

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
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 0))
	  (#\] (mark-cursor! (+ width 1) 0))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))

      (put! #\┌ 0 0)
      ;;(put! #\╵ 1 0)
      (put! #\┐ 0 (- width 1))
      ;;(put! #\╵ 1 (+ width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(end-highlight! HighlightType:Selection))))

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
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 0 (- height 1)))
	  (#\] (mark-cursor! (+ width 1) (- height 1)))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))

      ;;(put! #\╷ (- height 2) 0)
      (put! #\└ (- height 1) 0)
      ;;(put! #\╷ (- height 2) (+ width 1))
      (put! #\┘ (- height 1) (- width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end) context)
		 (is (head selection-end) in '(#\[ #\])))
	(end-highlight! HighlightType:Selection))))

  (define (unquote-marker-width)::real 1)

  (define (draw-unquote-splicing-box!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (with-translation (1 0)
      (draw-unquote-box! (- width 2) height context))
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start)
			 context)
		 (is (head selection-start)
		     in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))

      (put! #\┈ (- height 2) 0)
      (put! #\┤ (- height 2) 1)

      (put! #\├ (- height 2) (- width 2))
      (put! #\┈ (- height 2) (- width 1))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end)
			 context)
		 (is (head selection-end)
		     in '(#\[ #\])))
	(end-highlight! HighlightType:Selection))))

  (define (unquote-splicing-paren-width)::real 3)

  (define (draw-unquote-splicing-markers!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (let* ((selection ::Highlight (the-selection))
	   (selection-start selection:start)
	   (selection-end selection:end))
      (when (and (pair? (the-cursor))
		 (equal? context (cdr (the-cursor))))
	(match (head (the-cursor))
	  (#\[ (mark-cursor! 1 (- height 1)))
	  (#\] (mark-cursor! (- width 2) (- height 1)))
	  (_ (values))))
      (when (and (pair? selection-start)
		 (equal? (tail selection-start) context)
		 (is (head selection-start) in '(#\[ #\])))
	(begin-highlight! HighlightType:Selection))

      (put! #\┈ (- height 2) 0)
      (put! #\┐ (- height 2) 1)
      (put! #\└ (- height 1) 1)
      (put! #\┈ (- height 2) (- width 1))
      (put! #\┌ (- height 2) (- width 2))
      (put! #\┘ (- height 1) (- width 2))

      (when (and (pair? selection-end)
		 (equal? (tail selection-end)
			 context)
		 (is (head selection-end)
		     in '(#\[ #\])))
	(end-highlight! HighlightType:Selection))))

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
	 #;(for j from 1 to (- width 2)
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

  (define (draw-dashed-rectangle! width::real height::real)
    ::void
    (draw-custom-rectangle!
     #\┌ #\┄ #\┐
     #\┊
     #\└     #\┘
     width height))
     
  (define (fill-background! width::real height::real)::void
    (for j from 0 below height
	 (for i from 0 below width
	      (put! #\space j i))))

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

  (define 4pix ::(array-of char)
    ((array-of char) #\space
     #\▘ #\▝ #\▀ #\▖ #\▌
     #\▞ #\▛ #\▗ #\▚ #\▐
     #\▜ #\▄ #\▙ #\▟ #\█))

  (define (4pix-set! x2::int y2::int)::void
    (let* ((x ::int (quotient x2 2))
           (h ::int (remainder x2 2))
           (y ::int (quotient y2 2))
	   (v ::int (remainder y2 2))
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

  (define 8pix ::(array-of int)
    ((array-of int)
     0 1 2 6
     3 4 5 7))

  (define (8pix-bit x::int y::int)::int
    (arithmetic-shift 1 (8pix (+ y (* 4 x)))))

  (define (8pix-code c::int)::int
    (let ((masked ::int (bitwise-and c #xff00)))
      (if (= masked #x2800)
          (bitwise-and c #x00ff)
          0)))

  (define (8pix-set! x2::int y4::int)::void
    (let* ((x ::int (quotient x2 2))
           (h ::int (remainder x2 2))
           (y ::int (quotient y4 4))
           (v ::int (remainder y4 4))
           (c ::char (get y x))
           (c* ::char (as char
                          (as int
                              (bitwise-ior
                               #x2800
                               (8pix-code (as int c))
                               (8pix-bit h v))))))
      (put! c* y x)))

  (define (draw-line-8pix! x0::real y0::real
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
		 (8pix-set! x (as int (round y)))))))
       ((is pi/4 <= angle <= (* 3 pi/4))
	(let ((slope ::real (/ (cos angle)
			       (sin angle)))
              (y0 ::int (round y0)))
          (for j from 0 to (as int
			       (ceiling y1-y0))
	       (let ((x (+ x0 (* slope j)))
	             (y (+ y0 j)))
		 (8pix-set! (as int (round x))
			    y)))))
       (else
	(draw-line-8pix! x1 y1 x0 y0)))))

  (define (draw-thick-line! x0::real y0::real
		      x1::real y1::real)
    ::void
    (draw-line-4pix! (* x0 2) (* y0 2)
		     (* x1 2) (* y1 2)))

  (define (draw-thin-line! x0::real y0::real
			   x1::real y1::real)
    ::void
    (draw-line-8pix! (* x0 2) (* y0 4)
		     (* x1 2) (* y1 4)))

  (define (precise-resolution-right)::ubyte 2)
  (define (precise-resolution-down)::ubyte 4)
  
  (define (precise-inside-out px::real py::real)
    ::(Values real real)
    (values (round (/ px 2)) (round (/ py 4))))
  
  (define (precise-outside-in x::real y::real)
    ::(Values real real)
    (values (* x 2) (* y 4)))
  
  (define (precise-draw-circle! x0::real y0::real r::real
				color::uint)
    ::void
    (for x from (ceiling (- x0 r)) to (floor (+ x0 r))
	 (let* ((p (- x x0))
		(d (sqrt (- (* r r) (* p p)))))
	   (8pix-set! x (floor (+ y0 d)))
	   (8pix-set! x (ceiling (- y0 d))))))

  (define (precise-fill-rectangle!
	   left::real top::real
	   right::real bottom::real
	   color-rgb::uint)
    ::void
    (for x from (ceiling left) to (floor right)
	 (for y from (ceiling top) to (floor bottom)
	      (8pix-set! x y))))


  (define (precise-fill-circle! x0::real y0::real r::real
				color::uint)
    ::void
    (for x from (ceiling (- x0 r)) to (floor (+ x0 r))
	 (let* ((p (- x x0))
		(d (sqrt (- (* r r) (* p p))))
		(top (round (+ y0 d)))
		(bottom (round (- y0 d))))
	   (for y from bottom to top
		(8pix-set! x y)))))

  (define (precise-draw-line! x0::real y0::real
			      x1::real y1::real
			      color::uint)
    ::void
    (draw-line-8pix! x0 y0 x1 y1))
  
  (define (draw-quoted-text! s::CharSequence
			     context::Cursor)
    ::void
    (let ((extent ::Extent (string-extent s))
	  (t ::Traversal (the-traversal)))
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
	(set! t:left (+ t:left 1))
	(draw-string! s context)
	(set! t:left (- t:left 1)))
      (put! #\❞ (+ extent:height 1)
	    (+ extent:width 3))))

  (define (measure-quoted-text-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (set! target:left (+ target:left 2))
    (set! target:top (+ target:top 1))
    (measure-string-index-position-into! target text index))
  
  (define (draw-string! text::CharSequence
			context::Cursor)
    ::void
    (let* ((focused? (and (pair? (the-cursor))
			  (equal? context
				  (cdr (the-cursor)))))
	   (parent (the-traversal))
	   (highlights (the-highlights))
	   (highlight-starts
	    ::(list-of Highlight)
	    (only (lambda (highlight::Highlight)
		    (and-let* ((`(,_ . ,,context)
				highlight:start))))
		  highlights))
	   (highlight-ends
	    ::(list-of Highlight)
	    (only (lambda (highlight::Highlight)
		    (and-let* ((`(,_ . ,,context)
				highlight:end))))
		  highlights))
	   (traversal ::Traversal
		      (Traversal
		       max-line-height: 1
		       parent-left: (+ parent:parent-left
				       parent:left)
		       parent-top: (+ parent:parent-top
				      parent:top)
		       parent: parent)))

      (define (handle-cursor-and-selection!)
	(for highlight::Highlight in highlight-starts
	  (when (eqv? (car highlight:start)
		      traversal:index)
	    (begin-highlight! highlight:type)))
	(for highlight::Highlight in highlight-ends
	  (when (eqv? (car highlight:end)
		      traversal:index)
	    (end-highlight! highlight:type)))
	(when (and focused? (eqv? traversal:index
				  (car (the-cursor))))
	  (mark-cursor! traversal:left traversal:top)))

      (parameterize ((the-traversal traversal))
	(for c in text
	  (handle-cursor-and-selection!)
	  (cond ((eq? c #\newline)
		 (traversal:on-end-line #t)
		 (traversal:new-line!)
		 (set! traversal:max-line-height 1))
		((eq? c #\return)
		 ;; this seems to solve a bug on Windows/WSL1
		 (set! traversal:index
		       (- traversal:index 1)))
		;; jeszcze chcemy combining-character
		;; po prostu dopisac do biezacego znaku
		(else
		 (put! c traversal:top traversal:left)
		 (traversal:expand-by! 1)))
	  (set! traversal:index (+ traversal:index 1)))
	(handle-cursor-and-selection!)
	(traversal:on-end-line #f))))

  (define (styled-text-width text::Word style::TextDecoration)::real
    (text:length))

  (define (styled-text-height style::TextDecoration)::real
    1)
  
  (define (draw-styled-text! left::real top::real
			     text::CharSequence style::TextDecoration)
    ::void
    (for c in text
      (put! c top left)
      (set! left (+ left 1))))
  
  (define (measure-string-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    (let ((line ::int 0)
	  (column ::int 0)
	  (i ::int 0))
      (escape-with break
	(for c in text
	  (when (= i index)
	    (break))
	  (set! i (+ i 1))
          (cond ((eq? c #\newline)
		 (set! line (+ line 1))
		 (set! column 0))
		(else
		 (set! column (+ column 1))))))
      (set! target:left (+ target:left column))
      (set! target:top (+ target:top line))
      target))
  
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

  (define (draw-text-input! text::CharSequence
			    context::Cursor)
    ::void
    (draw-string! text context))

  (define (measure-text-input-index-position-into! target::Position
						   text::CharSequence
						   index::int)
    ::Position
    (measure-string-index-position-into! target text index))

  
  (define (text-input-extent text::CharSequence)::Extent
    (string-extent text))

  (define (text-input-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    (string-character-index-under x y text))

  (define (caption-margin-top)::real 1)

  (define (caption-margin-bottom)::real 1)

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

  (define (measure-atom-index-position-into! target::Position
					     text::CharSequence
					     index::int)
    ::Position
    (set! target:top (+ target:top 1))
    (measure-string-index-position-into! target text index))
  
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

  (define horizontal-stretch ::float 1.0)
  (define vertical-stretch ::float 1.0)
  
  (define (with-stretch horizontal::float vertical::float
			action::(maps () to: void))
    ::void
    (let ((previous-horizontal ::float horizontal-stretch)
	  (previous-vertical ::float vertical-stretch))
      (set! horizontal-stretch (* horizontal-stretch horizontal))
      (set! vertical-stretch (* vertical-stretch vertical))
      (try-finally
       (action)
       (begin
	 (set! horizontal-stretch previous-horizontal)
	 (set! vertical-stretch previous-vertical)))))

  (define (put! c::char row::real col::real)
    ::void
    #!abstract)

  (define (clear!)::void #!abstract)

  (define (request-redraw!)::void #!abstract)
  
  (define (current-width)::real #!abstract)

  (define (current-height)::real #!abstract)

  (define highlight-count::(array-of byte)
    ((array-of byte) length: (length (HighlightType:values))))

  (define (in-selection-drawing-mode?)::boolean
    (is (highlight-count (HighlightType:Selection:ordinal)) > 0))
  
  (define (begin-highlight! type::HighlightType)::void
    (let ((type-index (type:ordinal)))
      (set! (highlight-count type-index)
	    (+ (highlight-count type-index) 1))))

  (define (end-highlight! type::HighlightType)::void
    (let ((type-index (type:ordinal)))
      (set! (highlight-count type-index)
	    (- (highlight-count type-index) 1))))

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

  (define (measure-line-comment-index-position-into!
	   target::Position
	   text::CharSequence
	   index::int)
    ::Position
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
      (set! target:left (+ target:left shift))
      (measure-string-index-position-into! target text index)))

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

  (define (measure-block-comment-index-position-into!
	   target::Position
	   text::CharSequence
	   index::int)
    ::Position
    (set! target:left (+ target:left 1))
    (set! target:top (+ target:top 1))
    (measure-string-index-position-into! target text index))
  
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

  (define 4dirs ::(array-of char)
    ((array-of char) #\space
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
  (define data ::(array-of char))

  (define modifier ::procedure
    (mapping (key ::int)::char #!null))

  (define current-modifier #!null)

  (define (get row::real col::real)::char
    (let ((x (+ shiftLeft
		(nearby-int (* (slot-ref (this)
					 'horizontal-stretch) col))))
          (y (+ shiftTop
		(nearby-int (* (slot-ref (this)
					 'vertical-stretch) row)))))
      (if (and (is 0 <= x < width)
               (is 0 <= y < height))
          (data (+ (* width y) x))
          #\space)))

  (define (playing? animation::Animation)::boolean #f)
  
  (define (play! animation::Animation)::void
    (let loop ()
      (unless (animation:advance! 40)
	(loop))))

  (define (stop-playing! animation::Animation)::void (values))

  (define (with-intensity i::float action::(maps () to: void))::void
    (action))

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
    (let ((x (+ shiftLeft
		(nearby-int (* (slot-ref (this)
					 'horizontal-stretch) col))))
          (y (+ shiftTop
		(nearby-int (* (slot-ref (this)
					 'vertical-stretch) row))))
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
                 (new-data ((array-of char)
			    length:
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
	(when (and (invoke-special CharPainter (this)
				   'in-selection-drawing-mode?)
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

  (define (request-redraw!)::void
    (values))
  
  (define (mark-editor-cursor! +left::real +top::real editor::WithCursor)
    ::void
    (invoke-special CharPainter (this)
		    'mark-editor-cursor! +left +top (the-editor))
    (match (cursor-ref)
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
