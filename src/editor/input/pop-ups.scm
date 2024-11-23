(module-name (editor input pop-ups))

(import (language define-object))
(import (language attributes))
(import (language define-type))
(import (language define-parameter))
(import (language fundamental))
(import (language while))
(import (language for))
(import (language match))
(import (language infix))
(import (language keyword-arguments))

(import (utils functions))
(import (utils print))

(import (editor types primitive))

(import (editor interfaces painting))
(import (editor interfaces elements))

(import (editor types extensions widgets))

(import (editor types extensions extensions))
(import (editor input screen))


(define/kw (pop-up-action pop-up::PopUp finger::byte x::real y::real
			  inside: inner-action
			  ::(maps (Enchanted byte real real)
				  to: boolean)
			  := never
			  outside: outer-action
			  ::(maps (PopUp byte real real) to: boolean)
			  := always
			  on-the-edge: boundary-action
			  ::(maps (PopUp byte real real) to: boolean)
			  := always)
  (let* ((content ::Enchanted pop-up:content)
	 (left ::real pop-up:left)
	 (top ::real pop-up:top)
	 (horizontal ::real (painter:horizontal-popup-margin))
	 (vertical ::real (painter:vertical-popup-margin))
	 (inner-left ::real (+ left horizontal))
	 (inner-top ::real (+ top vertical))
	 (inner ::Extent (extent+ content))
	 (inner-right ::real (+ inner-left inner:width))
	 (inner-bottom ::real (+ inner-top inner:height))
	 (right ::real (+ inner-right horizontal))
	 (bottom ::real (+ inner-bottom vertical)))
    (cond ((and (is inner-left <= x < inner-right)
                (is inner-top <= y < inner-bottom))
	   (inner-action content finger
	          (- x inner-left) (- y inner-top)))
	  ((or (is x < left) (is x > right)
	       (is y < top) (is y > bottom))
	   (outer-action pop-up finger x y))
	  (else
	   (boundary-action pop-up finger (- x left) (- y top))))))

(define-type (PopUp left: real := 0
		    top: real := 0
                    content: Enchanted)
  implementing Layer
  with
  ((render!)::void
   (let ((tile ::Tile (as Tile (this))))
     (tile:draw! '())))

  ((press! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:press! finger x y))
     on-the-edge:
     (lambda (pop-up::PopUp finger::byte x::real y::real)
       ::boolean
       (screen:drag! finger pop-up))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:tap! finger x y))
     outside:
     (lambda (pop-up::PopUp finger::byte x::real y::real)
       ::boolean
       (screen:remove-overlay! pop-up))))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:second-press! finger x y))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (pop-up-action (this) finger x y
    inside:
    (lambda (content::Enchanted finger::byte x::real y::real)
      ::boolean
      (content:double-tap! finger x y))))

  ((long-press! finger::byte x::real y::real)::boolean
   (pop-up-action (this) finger x y
     inside:
     (lambda (content::Enchanted finger::byte x::real y::real)
       ::boolean
       (content:long-press! finger x y))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (content:key-typed! key-code (recons (first-index) context)))

  ((draw! context::Cursor)::void
   (let* ((inner ::Extent (extent+ content))
	  (horizontal ::real (painter:horizontal-popup-margin))
	  (vertical ::real (painter:vertical-popup-margin)))
     (with-translation (left top)
       (painter:draw-popup! (+ inner:width (* 2 horizontal))
			    (+ inner:height (* 2 vertical)))
       (with-translation (horizontal vertical)
	 (content:draw! (recons 'content context))))))

  ((part-at index::Index)::Indexable*
   (match index
    ('edge (this))
    ('content content)))

  ((measure-position #;of cursor::Cursor
			  #;into target::Position
				 #;within context::Cursor)
   ::Position
   (let* ((aspiration ::int (length cursor))
	  (level ::int (length context))
	  (suffix ::Cursor (drop (- aspiration level 1) cursor))
	  (index (car suffix)))
     (match index
       ('content
	(content:measure-position #;of cursor
				       #;into target
					      #;within context))
       (_ target))))

  ((first-index)::Index 'content)
  ((last-index)::Index 'edge)

  ((next-index index::Index)::Index 'edge)
  ((previous-index index::Index)::Index 'content)

  ((index< a::Index b::Index)::boolean ;>
   (and (eq? a 'edge) (eq? b 'content)))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (escape-with return
     (pop-up-action
      (this) 0 x y
      inside:
      (lambda (content::Tile finger::byte x::real y::real)::boolean
	      (return
	       (otherwise #!null
		 (and path
		      (content:cursor-under*
		       x y (recons 'content path))))))
      outside:
      (lambda (pop-up::PopUp finger::byte x::real y::real)::boolean
	      (return #!null))
      on-the-edge:
      (lambda (pop-up::PopUp finger::byte x::real y::real)::boolean
	      (return
	       (otherwise #!null
		 (and path (recons 'edge path))))))))

  ((extent)::Extent
   (let ((inner ::Extent (extent+ content)))
     (Extent width: (+ inner:width
		       (* 2 (painter:horizontal-popup-margin)))
	     height: (+ inner:height
			(* 2 (painter:vertical-popup-margin))))))

  implementing Drag
  with
  ((move! x::real y::real dx::real dy::real)::void
   (set! left (+ left dx))
   (set! top (+ top dy)))

  ((drop! x::real y::real vx::real vy::real)::void
   ;; sprawdzic czy v jest wieksza niz prog,
   ;; i iesli tak - usunac (this) ze screen:overlay
   (values))

  implementing Enchanted
  with
  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))

  ((center-around! x::real y::real)::void
   (let ((inner ::Extent (extent))
	 (outer ::Extent (screen:extent)))
     (set! left (max 0 (min (- outer:width inner:width)
			    (- x (quotient inner:width 2)))))
     (set! top
	   (max 0 (min (- outer:height inner:height)
		       (- y (quotient inner:height 2)))))))

  ((scroll-up! left::real top::real)::boolean
   (content:scroll-up! left top))

  ((scroll-down! left::real top::real)::boolean
   (content:scroll-down! left top))

  ((scroll-left! left::real top::real)::boolean
   (content:scroll-left! left top))

  ((scroll-right! left::real top::real)::boolean
   (content:scroll-right! left top))

  ((rotate-left! left::real top::real)::boolean
   (content:rotate-left! left top))

  ((rotate-right! left::real top::real)::boolean
   (content:rotate-right! left top))

  ((zoom-in! left::real top::real)::boolean
    (content:zoom-in! left top))

  ((zoom-out! left::real top::real)::boolean
   (content:zoom-out! left top))
  )

(define-type (Scroll width: real
                     height: real
		     left: real := 0
		     top: real := 0
		     content: Enchanted)
  implementing Drag
  with
  ((move! x::real y::real dx::real dy::real)::void
   (let ((inner ::Extent (extent+ content)))
     (set! left (max 0 (min (- inner:width width) (- left dx))))
     (set! top (max 0 (min (- inner:height height) (- top dy))))))

  ((drop! x::real y::real vx::real vy::real)::void
   (values))

  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (painter:fill-background! width height)
   (with-clip (width height)
       (with-translation ((- left) (- top))
	 (content:draw! (recons 0 context)))))

  ((tap! finger::byte  x::real y::real)::boolean
   (content:tap! finger (+ x left) (+ y top)))

  ((press! finger::byte x::real y::real)::boolean
   (screen:drag! finger (this)))

  ((second-press! finger::byte #;at x::real y::real)::boolean
    (content:press! finger (+ x left) (+ y top)))

  ((double-tap! finger::byte x::real y::real)::boolean
    (content:double-tap! finger (+ x left) (+ y top)))

  ((long-press! finger::byte x::real y::real)::boolean
    (content:long-press! finger (+ x left) (+ y top)))

  ((key-typed! key-code::long context::Cursor)::boolean
    (content:key-typed! key-code (recons (first-index) context)))

  ((extent)::Extent
   (Extent width: width
           height: height))

  ((measure-position #;of cursor::Cursor
			  #;into target::Position
				 #;within context::Cursor)
   ::Position
   (let* ((aspiration ::int (length cursor))
	  (level ::int (length context))
	  (suffix ::Cursor (drop (- aspiration level 1) cursor))
	  (index (car suffix)))
     (match index
       ('content
	(set! target:left (+ target:left left))
	(set! target:top (+ target:top top))
	(content:measure-position #;of cursor
				       #;into target
					      #;within context))
       (_ target))))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (content:cursor-under* (- x left) (- y top) (recons 0 path)))

  ((part-at index::Index)::Indexable* content)

  ((first-index)::Index 0)

  ((last-index)::Index 0)

  ((next-index index::Index)::Index 0)

  ((previous-index index::Index)::Index 0)

  ((index< a::Index b::Index)::boolean #f)

  ((scroll-up! x::real y::real)::boolean
   (when (is top > 0)
     (let* ((h ::real (painter:min-line-height)))
       (set! top (max 0 (- top h)))))
   #t)

  ((scroll-down! x::real y::real)::boolean
   (let ((inner ::Extent (extent+ content)))
     (when (is top < (- inner:height height))
       (let* ((h ::real (painter:min-line-height)))
	 (set! top (min (- inner:height height) (+ top h))))))
   #t)

  ((scroll-left! x::real y::real)::boolean
   (when (is left > 0)
     (let* ((w ::real (painter:space-width)))
       (set! left (max 0 (- left w)))))
   #t)

  ((scroll-right! x::real y::real)::boolean
   (let ((inner ::Extent (extent+ content)))
     (when (is left < (- inner:width width))
       (let* ((w ::real (painter:space-width)))
	 (set! left (min (- inner:width width) (+ left w))))))
   #t)

  ((zoom-in! x::real y::real)::boolean
   #f)

  ((zoom-out! x::real y::real)::boolean
   #f)

  ((rotate-left! x::real y::real)::boolean
   #f)

  ((rotate-right! x::real y::real)::boolean
   #f)

  ((value)::Object
   (invoke-special Base 'to-list cons to-expression)))

(define (text-field width::real content::CharSequence)::Scroll
  (let* ((input ::TextInput (text-input content))
	 (inner ::Extent (extent+ input))
	 (scroll ::Scroll (Scroll width: width
				  height: inner:height
				  content: input)))
    scroll))

(define (popup-scroll content::Enchanted)::PopUp
  (let* ((content ::Enchanted content)
	 (inner ::Extent (extent+ content))
	 (scroll ::Scroll (Scroll width: inner:width
				  height: inner:height
				  content: content))
         (popup (PopUp content: scroll))
	 (outer ::Extent (extent+ popup))
	 (available ::Extent (screen:extent)))
    (set! scroll:width (- scroll:width
                          (max 0 (- outer:width
			            available:width))))
    (set! scroll:height (- scroll:height
                           (max 0 (- outer:height
			             available:height))))
    popup))
