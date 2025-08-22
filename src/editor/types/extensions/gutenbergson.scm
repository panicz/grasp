(module-name (editor types extensions gutenbergson))

(import (kawa regex))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language define-cache))
(import (language match))
(import (language for))
(import (language while))
(import (utils functions))
(import (language fundamental))
(import (language assert))
(import (language define-cache))
(import (language define-parameter))
(import (language mapping))

(import (editor interfaces elements))
(import (editor interfaces painting))
(import (editor types primitive))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor input transforms))
(import (editor input screen))
(import (editor input input))

(import (editor document parse))

(import (editor types extensions extensions))
;;(import (editor types extensions combinators))

(import (utils print))

(define-syntax-rule (match/regex subject (pattern . actions) ...)
  (cond
   ((regex-match pattern subject) . actions)
   ...))

(define-type (Section title: (sequence-of Word)))

(define-alias StyledText
  (sequence-of (either string TextStyle EndTextStyle)))

(define-alias Paragraph
  (either Section string StyledText Tile))

(define-type (Chapter title: (sequence-of Word)
		      paragraphs: (sequence-of Paragraph) := (java.util.ArrayList)))

(define-type (Book title: string
		   chapters: (sequence-of Chapter) := (java.util.ArrayList)))

(define-private opening-brackets ::(set-of gnu.text.Char)
  (set #\( #\[ #\{))

(define-private closing-brackets ::(set-of gnu.text.Char)
  (set #\) #\] #\}))

(define-private punctuation ::(set-of gnu.text.Char)
  (set #\; #\. #\: #\, #\? #\!))

(define-private brackets&punctuation ::(set-of gnu.text.Char)
  (union opening-brackets closing-brackets punctuation))

(define-cache (char->string single-char ::gnu.text.Char)
  ::string
  (list->string (list single-char)))

(define (read-words #!optional (input-port ::InputPort (current-input-port)))
  ::(list-of Word)
  (let ((c (read-char input-port)))
    (cond 
      ((eof-object? c)
       '())
       
      ((char-whitespace? c)
       (read-words input-port))
       
      ((is c in brackets&punctuation)
       (cons (char->string c) (read-words input-port)))
      
      (else
       (let ((word (call-with-output-string
                     (lambda (p)
                       (write-char c p)
                       (let loop ()
                         (let ((c (peek-char input-port)))
                            (unless (or (eof-object? c)
                                        (char-whitespace? c)
					(is c in brackets&punctuation))
                              (write-char (read-char input-port) p)
                              (loop))))))))
	 (cons word (read-words input-port)))))))

(define (read-paragraphs #!optional (input-port ::InputPort (current-input-port)))
  ::(sequence-of string)
  (let ((paragraphs ::java.util.List (java.util.ArrayList))
	(current-paragraph ::java.lang.StringBuilder
			   (java.lang.StringBuilder)))
    
    (define (finish-paragraph!)
      (when (is (current-paragraph:length) > 0)
	(paragraphs:add (current-paragraph:toString))
	(current-paragraph:setLength 0)))
    
    (let next ()
      (let ((line (read-line input-port)))
	(cond
	 ((eof-object? line)
	  (finish-paragraph!)
	  paragraphs)
	 ((regex-match "^[ \t]*$" line)
	  (finish-paragraph!)
	  (next))
	 ((regex-match "^[#][+]BEGIN_SRC" line)
	  (finish-paragraph!)
	  (current-paragraph:append line)
	  (let snip ()
	    (let ((line (read-line input-port)))
	      (assert (isnt line eof-object?))
	      (current-paragraph:append (as char #\newline))
	      (current-paragraph:append line)
	      (cond
	       ((regex-match "^[#][+]END_SRC" line)
		(finish-paragraph!)
		(next))
	       (else
		(snip))))))
	 (else
	  (and-let* ((n ::int (current-paragraph:length))
		     ((is n > 0))
		     ((isnt (current-paragraph:charAt (- n 1))
			    eq? (as char #\space))))
	    (current-paragraph:append (as char #\space)))
	  (current-paragraph:append line)
	  (next)))))))


(define (extract-style-modifiers word::Word)::(sequence-of
					       (either
						Word
						TextStyle
						EndTextStyle))
  (match/regex
   word
   ("^[*](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Bold . ,(extract-style-modifiers word*))))
   ("^[/](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Italic . ,(extract-style-modifiers word*))))
   ("^[~](.+)$"
    => (fn (`(,_ ,word*))
	   `(,TextStyle:Monospace . ,(extract-style-modifiers word*))))
   
   ("^(.+)[*]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Bold))))

   ("^(.+)[/]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Italic))))

   ("^(.+)[~]$"
    => (fn (`(,_ ,word*))
	   `(,@(extract-style-modifiers word*)
	     ,(EndTextStyle style: TextStyle:Monospace))))

   (".*"
    `(,word))))

(define (parse-paragraph #!optional (input ::InputPort (current-input-port)))
  ::(sequence-of (either Word TextStyle EndTextStyle))
  (let ((words (read-words input)))
    (append-map extract-style-modifiers words)))

(define (book-title? text)::(maybe string)
  (and-let* (((string? text))
	     (`(,_ ,title) (regex-match "^[*] ([^\n]+)$" text)))
    title))

(define (chapter-title? text)::(maybe string)
  (and-let* (((string? text))
	     (`(,_ ,title) (regex-match "^[*][*] ([^\n]+)$" text)))
    title))

(define (section-title? text)::(maybe string)
  (and-let* (((string? text))
	     (`(,_ ,title) (regex-match "^[*][*][*]+ ([^\n]+)$" text)))
    title))

(define (layout-paragraph
	 paragraph ::Paragraph
	 word-operation ::(maps (real real Word TextDecoration) to: void)
	 #!key
	 (width ::real +inf.0)
	 (style ::TextDecoration (RegularText)))
  ::real
  (match paragraph
    (verbatim::string
     (let ((top ::real 0)
	   (style ::TextDecoration (EnumSet:of TextStyle:Monospace)))
       (call-with-input-string verbatim
	 (lambda (input)
	   (for line in (lines input)
	     (word-operation 0 top line style)
	     (set! top (+ top (painter:styled-text-height style))))))
       top))
    
    ((Section title: title)
     (layout-paragraph title word-operation
		       width: width
		       style: (EnumSet:of TextStyle:Bold TextStyle:Large)))
    
    (words
     ::(sequence-of Word)
     (let ((top ::real 0)
	   (left ::real 0)
	   (line-height (painter:styled-text-height style)))
       (for token in words
	 (match token
	   (word::string
	    (let* ((word-length ::int (string-length word))
		   (word-width ::real (painter:styled-text-width word style))
		   (space-width ::real (painter:styled-text-width " " style))
		   (expanded ::real (+ left word-width)))

	      (cond
	       ((and (= word-length 1)
		     (or (is (word 0) in punctuation)
			 (is (word 0) in closing-brackets))
		     (is left > space-width))
		(set! left (- left space-width)))
	       
	       ((is expanded > width)
		(set! left 0)
		(set! top (+ top line-height))))
	      
	      (word-operation left top word style)

	      (set! left (+ left word-width
			    (if (and (= word-length 1)
				     (is (word 0) in opening-brackets))
				0
				space-width)))))
	   
	   (modifier::TextStyle
	    ;(assert (not (style:contains modifier)))
	    (style:add modifier))
	   
	   ((EndTextStyle style: modifier)
	    ;;(assert (style:contains modifier))
	    (style:remove modifier))))
	 
	 (+ top line-height line-height)))))

(define (render-paragraph! paragraph::Paragraph
			   #!key
			   (width::real +inf.0)
			   (style ::TextDecoration (RegularText)))
  ::real
  (match paragraph
    (tile::Tile
     (tile:draw! '())
     (let ((extent ::Extent (tile:extent)))
       extent:height))
    (_
     (layout-paragraph
      paragraph
      (lambda (left::real top::real word::Word
			  style::TextDecoration)
	(painter:draw-styled-text! left top word style))
      width: width
      style: style))))

(define-cache (cached-paragraph-height words::Paragraph
				       width ::real := +inf.0
				       style ::TextDecoration := (RegularText))
  ::real
  (layout-paragraph words nothing
		    width: width
		    style: style))

(define (paragraph-height paragraph::Paragraph
			  #!optional
			  (width ::real +inf.0)
			  (style ::TextDecoration (RegularText)))
  ::real
  (match paragraph
    (tile::Tile
     (let ((extent ::Extent (tile:extent)))
       extent:height))
    (_
     (cached-paragraph-height paragraph width style))))

(define (parse-book #!optional (input ::InputPort (current-input-port)))::Book
  (let* ((paragraphs ::(sequence-of Paragraph) (read-paragraphs input))
	 (book ::Book (Book chapters: (java.util.ArrayList)))
	 (current-chapter ::Chapter #!null))
    (for paragraph in paragraphs
      (cond
       ((is paragraph book-title?)
	=> (lambda (title ::string)
             (assert (eq? book:title #!null))
             (set! book:title title)))
       ((is paragraph chapter-title?)
	=> (lambda (title ::string)
             (when current-chapter
               (book:chapters:add current-chapter))
             (set! current-chapter (Chapter title: (string-split title " ")
					    paragraphs: (java.util.ArrayList)))))
       ((is paragraph section-title?)
	=> (lambda (title ::string)
	     (current-chapter:paragraphs:add
	      (Section title: (string-split title " ")))))

       ((regex-match "^[#][+]BEGIN_SRC" paragraph)
	(match/regex
	 paragraph
	 ("(?s)^[#][+]BEGIN_SRC scheme :evaluate yes(.*)[#][+]END_SRC"
	  => (fn (`(,_ ,code))
		 (let ((document (call-with-input-string code parse-document)))
		   (let evaluate ((expressions document))
		     (match expressions
		       (`(,first . ,rest)
			(eval first)
			(evaluate rest))
		       (_
			(values)))))
		 (current-chapter:paragraphs:add document)))

	 ("(?s)^[#][+]BEGIN_SRC scheme :extension yes(.*)[#][+]END_SRC"
	  => (fn (`(,_ ,code))
		 (let ((document (call-with-input-string code parse-document)))
		   (let enchant ((expressions document))
		     (match expressions
		       (`(,first . ,rest)
			(and-let* ((`(,keyword::symbol . ,data) expression)
				   (magic ::Extension (extension keyword))
				   (enchanted ::Enchanted (magic:enchant
							   expression)))
			  (set-car! expressions enchanted))
			(enchant rest))
		       (_
			(values))))
		   (current-chapter:paragraphs:add document))))
	 
	 ("(?s)^[#][+]BEGIN_SRC scheme(.*)[#][+]END_SRC"
	  => (fn (`(,_ ,code))
		 (let ((document (call-with-input-string code parse-document)))
		   (current-chapter:paragraphs:add document))))
	 (".*"
	  (current-chapter:paragraphs:add (string-append paragraph "\n\n")))))
       
       (else
        (current-chapter:paragraphs:add 
         (call-with-input-string paragraph parse-paragraph)))))
    book))

(define (chapter-height chapter ::Chapter
			max-line-width ::real)
  ::real
  (fold-left (lambda (height::float paragraph::Paragraph)
	       ::float
	       (+ height (paragraph-height paragraph max-line-width)))
	     (paragraph-height chapter:title max-line-width
			       (EnumSet:of TextStyle:Bold TextStyle:Large))
	     chapter:paragraphs))

(define-object (ScrollBookReader reader ::InteractiveBookReader
				 x0 ::real y0 ::real)
  ::Drag
  (define (move! x ::real y ::real dx ::real dy ::real)::void
    (reader:scroll-by! dy))

  (define (drop! x ::real y ::real vx ::real vy ::real)::void
    (let ((x-x0 (- x x0))
	  (y-y0 (- y y0))
	  (line-height (painter:styled-text-height (RegularText)))
	  (threshold (painter:styled-text-width "turn" (RegularText))))
	  
      (when (is (abs y-y0) < (* 3 line-height))
	(cond
	 ((is x-x0 > threshold)
	  (reader:scroll-by! (- reader:size:height
				(abs y-y0))))

	 ((is x-x0 < (- threshold))
	  (reader:scroll-by! (- (- reader:size:height
				   (abs y-y0)))))
	 ))))
  )

(define-type (TransformCorrection dx: real dy: real
				  new-scale: real
				  new-angle: real))

(define (pinch x00 ::real y00 ::real x10 ::real y10 ::real
	       x01 ::real y01 ::real x11 ::real y11 ::real
	       scale ::real angle/rad ::real)
    ::TransformCorrection
    (let* ((px ::real (- x00 x10))
	   (py ::real (- y00 y10))
	   (d1 ::real (hypotenuse px py))
	   (sx ::real (- x01 x11))
	   (sy ::real (- y01 y11))
	   (d2 ::real (hypotenuse sx sy))
	   (s (sin angle/rad))
	   (c (cos angle/rad))
	   (scale* ::real (/ (* scale d2) d1))
	   (da ::real (- (atan sy sx) (atan py px)))
	   (angle*/rad ::real (+ angle/rad da))
	   (s* ::real (sin angle*/rad))
	   (c* ::real (cos angle*/rad))
	   (dx ::real (- (/ (+ (* c x00) (* s y00))
			    scale)
			 (/ (+ (* c* x01) (* s* y01))
			    scale*)))
	   (dy ::real (- (/ (- (* c y00) (* s x00))
			    scale)
			 (/ (- (* c* y01) (* s* x01))
			    scale*))))
      (TransformCorrection dx: dx
			   dy: dy
			   new-scale: scale*
			   new-angle: angle*/rad)))

(define-object (InteractiveBookReader book ::Book)
  ::Maximizable

  (define scale ::float 1.0)
  
  (define current-chapter ::int 0)
  
  (define chapter-scroll ::(sequence-of real)
    ((array-of float) length: (length book:chapters)))

  (define (next-chapter!)::boolean
    (let ((next-chapter (+ current-chapter 1)))
      (cond
       ((is next-chapter < (length book:chapters))
	(set! current-chapter next-chapter)
	#t)
       (else
	#f))))
	    
  (define (previous-chapter!)::boolean
    (let ((previous-chapter (- current-chapter 1)))
      (cond
       ((is previous-chapter >= 0)
	(set! current-chapter previous-chapter)
	#t)
       (else
	#f))))
  
  (define (draw! context::Cursor)::void
    (safely
     (painter:precise-fill-rectangle!
      0 0
      (* (painter:precise-resolution-right) size:width)
      (* (painter:precise-resolution-down) size:height)
      #xffffffff)
     (let ((chapter ::Chapter (book:chapters current-chapter))
	   (top ::real (chapter-scroll current-chapter))
	   (width ::real (min max-text-width (/ size:width scale)))
	   (visible-height ::real (/ size:height scale)))
       (painter:scale! scale)
       (with-translation (0 top)
	 (let ((height (render-paragraph! chapter:title width: width
					  style: (EnumSet:of TextStyle:Bold
							     TextStyle:Extra))))
	   (set! top
		 (+ top height))))
       (escape-with break
	 (for i from 0 below (length chapter:paragraphs) 
	      (let* ((paragraph ::Paragraph (chapter:paragraphs i))
		     (height
		      (with-translation (0 top)
			(render-paragraph! paragraph width: width))))
		(set! top (+ top height))
		(when (is top > visible-height)
		  (break)))))
       (painter:scale! (/ 1.0 scale)))))

  (define (press! finger::byte #;at x ::real y ::real)::boolean
    (let ((dragging-fingers  (screen:dragging-fingers)))
      (if (empty? dragging-fingers)
	  (screen:drag! finger (ScrollBookReader (this) x y))
	  (when (and (is (dragging-fingers:size) = 1)
		     (isnt finger in dragging-fingers))
	    (let* ((other-finger (the-element-of dragging-fingers))
		   (p0 ::Position (Position left: x top: y))
		   (p1 ::Position (copy
				   (last-known-pointer-position
				    other-finger))))
	      (screen:undrag! other-finger)
	      (screen:drag!
	       other-finger
	       (object (Drag)
		 ((move! x::real y::real
			 dx::real dy::real)
		  ::void
		  (let* ((p1x ::real (+ p1:left dx))
			 (p1y ::real (+ p1:top dy))
			 (correct ::TransformCorrection
				  (pinch
				   p0:left p0:top p1:left p1:top
				   p0:left p0:top p1x  p1y
				   scale 0)))
		    (set! scale correct:new-scale)
		    (scroll-by! (- correct:dy))
		    (set! p1:left p1x)		    
		    (set! p1:top p1y)))

		 ((drop! x::real y::real
			 vx::real vy::real)
		  ::void
		  ;; jezeli jest jeszcze inny palec,
		  ;; to powinnismy przywrocic do niego
		  ;; ~ScrollBookReader~
		  (values))
		 ))

	      (screen:drag!
	       finger
	       (object (Drag)
		 ((move! x::real y::real
			 dx::real dy::real)
		  ::void
		  (let* ((p0x ::real (+ p0:left dx))
			 (p0y ::real (+ p0:top dy))
			 (correct ::TransformCorrection
				  (pinch
				   p0:left p0:top p1:left p1:top
				   p0x  p0y  p1:left p1:top
				   scale 0)))
		    (set! scale correct:new-scale)
		    (scroll-by! (- correct:dy))	    
		    (set! p0:left p0x)
		    (set! p0:top p0y)))

		 ((drop! x::real y::real
			 vx::real vy::real)
		  ::void
		  ;; jezeli jest jeszcze inny palec,
		  ;; to powinnismy przywrocic do niego
		  ;; ~ScrollBookReader~
		  (values))
		 ))
	      )))))
    
  (define max-text-width ::real
    (painter:styled-text-width
     "Abc def ghi jkl mno pq rst uvw xyz abcdefghijklmnopqrstuvwxyz"
     (RegularText)))
  
  (define size ::Extent
    (Extent width: (* 60 (painter:space-width))
	    height: (* 25 (painter:styled-text-height (RegularText)))))
  
  (define (set-size! width::real height::real anchor::ResizeAnchor)::void
    (set! size:width width)
    (set! size:height height))

  (define (extent)::Extent size)

  (define (value)::Object
    (cons (Atom "InteractiveBookReader") (empty)))

  (define (current-chapter-height)::real
    (chapter-height (book:chapters current-chapter)
		    (min max-text-width size:width)))

  (define (scroll-by! delta ::real)::void
    (let* ((previous-scroll (chapter-scroll current-chapter))
           (new-scroll ::float (as float (+ previous-scroll delta)))
	   (scroll-limit ::float (- (current-chapter-height))))
      (cond 
       ((is new-scroll < scroll-limit)
	(when (is delta < 0)
	  (set! (chapter-scroll current-chapter) scroll-limit))
        (when (is current-chapter < (- (length book:chapters) 1))
          (set! current-chapter (+ current-chapter 1))
          (set! (chapter-scroll current-chapter) (as float 0))))

       ((is new-scroll > 0)
	(when (is delta > 0)
	  (set! (chapter-scroll current-chapter) (as float 0)))
        (when (is current-chapter > 0)
          (set! current-chapter (- current-chapter 1))
          (set! (chapter-scroll current-chapter)
		(as float (- (current-chapter-height))))))

       (else
        (set! (chapter-scroll current-chapter) new-scroll)))))
  
  (define (key-typed! key-code::long context::Cursor)::boolean
    (match (key-chord key-code)
      ('left
       (previous-chapter!))
      
      ('right
       (next-chapter!))
      
      ('up
       (scroll-by! (painter:styled-text-height (RegularText)))
       #t)
      
      ('down
       (scroll-by! (- (painter:styled-text-height (RegularText))))
       #t)

      ('mouse-wheel-up
       (scroll-by! (painter:styled-text-height (RegularText)))
       #t)
      
      ('mouse-wheel-down
       (scroll-by! (- (painter:styled-text-height (RegularText))))
       #t)
      
      ('page-up
       (scroll-by! (- size:height (painter:styled-text-height (RegularText))))
       #t)

      ('page-down
       (scroll-by! (- (- size:height (painter:styled-text-height (RegularText)))))
       #t)
      
      ('(ctrl mouse-wheel-up)
       (set! scale (* scale 1.25))
       #t)

      ('(ctrl mouse-wheel-down)
       (set! scale (/ scale 1.25))
       #t)
      
      (name
       (WARN "unsupported key: "name)
       #f)))
  
  (MaximizableWidget))

(set! (extension 'InteractiveBookReader)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as InteractiveBookReader (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create InteractiveBookReader from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
