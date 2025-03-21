(module-name (editor input splits))

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

(import (language parameterize-up))
(import (utils functions))
(import (editor types primitive))
(import (editor document cursor))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor input transforms))
(import (editor input screen))
(import (utils print))

(define-enum SplitFocus (First Last))

(define-parameter (the-split-context)
  ::(list-of SplitFocus)
  '())

(define-type (Split at: real := 0.5
		    first: Embeddable
		    last: Embeddable
		    focus: SplitFocus := SplitFocus:First)
  implementing Splittable
  with

  ((close-document! document)::void
   (first:close-document! document)
   (last:close-document! document))
  
  (translation ::Translation (Translation))

  ((set-translation! pos::real)::void
   #!abstract)

  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   #!abstract)

  ((part-sizes)::(Values real real real)
    #!abstract)

  ((with-pane-size size::real action::procedure)::Object
   #!abstract)

  ((with-pane-translation shift::real action::procedure)::Object
   #!abstract)

  ((area/last original::Area earlier-size::real)::Area
   #!abstract)

  ((varying-dimension x::real y::real)::real
   #!abstract)

  ((varying-size w::real h::real size::real)::(Values real real)
   #!abstract)

  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   #!abstract)

  ((part focus::SplitFocus)::Embeddable
   (match focus
     (,SplitFocus:First first)
     (,SplitFocus:Last last)))

  ((active)::Embeddable
   (match focus
     (,SplitFocus:First
      (first:active))
     (,SplitFocus:Last
      (last:active))))
  
  ((draw-split!)::void
   #!abstract)

  ((render!)::void
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-translation first-size
       (lambda ()
	 (draw-split!)
	 (set-translation! (+ first-size line-size))
	 (with-post-transform translation
	   (with-pane-translation line-size
	     (lambda ()
	       (with-pane-size last-size
		 (lambda ()
		   (parameterize ((the-split-context
				   (recons SplitFocus:Last
					   (the-split-context))))
		     (with-clip ((the-pane-width) (the-pane-height))
		       (last:render!))))))))))
     (with-pane-size first-size
       (lambda ()
	 (parameterize ((the-split-context
			 (recons SplitFocus:First
				 (the-split-context))))
	   (with-clip ((the-pane-width) (the-pane-height))
	     (first:render!)))))))

  ((propagate action::procedure x::real y::real default::procedure)
   (let-values (((first-size line-size last-size) (part-sizes))
		((pos) (varying-dimension x y)))
     (cond ((is pos < first-size)
	    (set! focus SplitFocus:First)
	    (with-pane-size first-size
	      (lambda ()
		(parameterize ((the-split-context
				(recons SplitFocus:First
					(the-split-context))))
		  (action first x y)))))
	   ((is (+ first-size line-size) < pos)
	    (set-translation! (+ first-size line-size))
	    (set! focus SplitFocus:Last)
	    (with-post-transform translation
	      (with-pane-size last-size
		(lambda ()
		  (parameterize ((the-split-context
				  (recons SplitFocus:Last
					  (the-split-context))))
		    (let-values (((x* y*) (transformed-dimension
					   x y (+ first-size
						  line-size))))
		      (action last x* y*)))))))
	   (else
	    (set-translation! first-size)
	    (with-post-transform translation
	      (with-pane-size line-size
		(lambda ()
		  (let-values (((x* y*) (transformed-dimension
					 x y first-size)))
		    (default (this) x* y*)))))))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (match focus
       (,SplitFocus:First
	(with-pane-size first-size
	  (lambda ()
	    (parameterize ((the-split-context
			    (recons SplitFocus:First
				    (the-split-context))))
	      (first:key-typed! key-code
				(recons SplitFocus:First
					context))))))
       (,SplitFocus:Last
	(set-translation! (+ first-size line-size))
	(with-post-transform translation
	  (with-pane-size last-size
	    (lambda ()
	      (parameterize ((the-split-context
			      (recons
			       SplitFocus:Last
			       (the-split-context))))
		(last:key-typed! key-code
				 (recons SplitFocus:Last
					 context))))))))))

  ((drop-at! x::real y::real object::pair)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:drop-at! x y object))
	      x y
	      never))

  ((outside-in x::real y::real)::(Values real real)
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:outside-in x y))
              x y
              values))

  ((pane-under x::real y::real)::Embeddable
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:pane-under x y))
	      x y
	      (lambda _ (this))))

  ((tap! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:tap! finger x y))
	      x y
	      values))

  ((press! finger::byte #;at x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:press! finger x y))
	      x y
	      (lambda (self::Embeddable x::real y::real)
		(screen:drag! finger (ResizeSplitAt
				      (the-split-context)))
		#t)))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:second-press! finger x y))
	      x y
	      never))

  ((double-tap! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:double-tap! finger x y))
	      x y
	      never))

  ((long-press! finger::byte x::real y::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		       (target:long-press! finger x y))
	      x y
	      never))

  ((scroll-up! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-up! x y))
	      left top
	      never))

  ((scroll-down! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-down! x y))
	      left top
	      never))

  ((scroll-left! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-left! x y))
	      left top
	      never))

  ((scroll-right! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:scroll-right! x y))
	      left top
	      never))

  ((zoom-in! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:zoom-in! x y))
	      left top
	      never))

  ((zoom-out! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:zoom-out! x y))
	      left top
	      never))

  ((rotate-left! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:rotate-left! x y))
	      left top
	      never))

  ((rotate-right! left::real top::real)::boolean
   (propagate (lambda (target::Embeddable x::real y::real)
		(target:rotate-right! x y))
	      left top
	      never))

  ((can-split-beside? line::Area)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (or (with-pane-size first-size
	   (lambda ()
	     (and-let* ((first ::Splittable))
	       (first:can-split-beside? line))))
	 (with-pane-size last-size
	   (lambda ()
	     (and-let* ((last ::Splittable))
	       (last:can-split-beside?
		(area/last line (+ first-size line-size)))))))))

  ((split-beside! line::Area)::Embeddable
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-size first-size
       (lambda ()
	 (and-let* ((f ::Splittable first)
		    ((f:can-split-beside? line)))
	   (set! first (f:split-beside! line)))))
     (let ((line* (area/last line (+ first-size line-size))))
       (with-pane-size last-size
	 (lambda ()
	   (and-let* ((l ::Splittable last)
		      ((l:can-split-beside? line*)))
	       (set! last (l:split-beside! line*))))))
     (this)))

  ((can-split-below? line::Area)::boolean
   (let-values (((first-size line-size last-size) (part-sizes)))
     (or (with-pane-size first-size
	   (lambda ()
	     (and-let* ((first ::Splittable))
	       (first:can-split-below? line))))
	 (with-pane-size last-size
	   (lambda ()
	     (and-let* ((last ::Splittable))
	       (last:can-split-below?
		(area/last line (+ first-size line-size)))))))))

  ((split-below! line::Area)::Embeddable
   (let-values (((first-size line-size last-size) (part-sizes)))
     (with-pane-size first-size
       (lambda ()
	 (and-let* ((f ::Splittable first)
		    ((first:can-split-below? line)))
	   (set! first (f:split-below! line)))))
     (let ((line* (area/last line (+ first-size line-size))))
       (with-pane-size last-size
	 (lambda ()
	   (and-let* ((l ::Splittable last)
		      ((last:can-split-below? line*)))
	     (set! last (l:split-below! line*))))))
     (this)))

  )

(define (split-ref split-path)::Embeddable
  (match split-path
    ('() (screen:content))
    (`(,head . ,tail)
     (let ((parent ::Split (split-ref tail)))
       (parent:part head)))))

(define/kw (screen-area split-path::(list-of SplitFocus)
			pane::Embeddable := (screen:top))
  ::(Values Embeddable real real real real)
  (match split-path
    ('()
     (values pane
             0 0 (screen:width) (screen:height)))
    (`(,head . ,tail)
     (let-values (((parent::Embeddable
		    x::real y::real
		    w::real h::real) (screen-area tail pane)))
       (parameterize ((the-pane-width w)
		      (the-pane-height h))
	 (match parent
	   ((Split first: left last: right)
	    (let*-values (((split::Split) parent)
			  ((first-size line-size last-size)
			   (split:part-sizes)))
	      (match head
		(,SplitFocus:First
		 (let-values (((w* h*) (split:varying-size
					w h first-size)))
		   (values left x y w* h*)))
		(,SplitFocus:Last
		 (let-values (((x* y*) (split:transformed-dimension
					x y (- 0 first-size line-size)))
			      ((w* h*) (split:varying-size
					w h last-size)))
		   (values right x* y* w* h*))))))))))))

(define/kw (merge-split! at: split-path with: focus::SplitFocus)
  ::boolean
  (if (null? split-path)
    (and-let* ((split ::Split (screen:content)))
      (screen:set-content! (split:part focus)))
    (and-let* ((`(,tip . ,root) split-path)
               (parent ::Split (split-ref root))
	       (split ::Split (parent:part tip)))
      (match tip
        (,SplitFocus:First (set! parent:first (split:part focus)))
	(,SplitFocus:Last (set! parent:last (split:part focus)))))))

(define-object (ResizeSplitAt split-path::list)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (let-values (((split::Split
		   left::real top::real
		   width::real height::real) (screen-area
					      split-path
					      (screen:content))))
      (split:resize! x y left top width height)))

  (define (drop! x::real y::real vx::real vy::real)::void
    (and-let* ((target ::Split (split-ref split-path))
               (first-size line-size last-size
	                   (target:part-sizes))
	       (velocity (target:varying-dimension vx vy)))
      (cond
       ((or (is first-size <= (* 3 line-size)) ;>
            (is velocity < -1.5)) ;>
	(merge-split! at: split-path with: SplitFocus:Last))
       ((or (is last-size <= (* 3 line-size))
            (is velocity > 1.5))
	(merge-split! at: split-path with: SplitFocus:First)))))
  )

(define-type (SplitBeside)
  extending Split
  with
  ((part-sizes)::(Values real real real)
   (let* ((pane-width ::real (the-pane-width))
	  (line-width ::real (painter:vertical-split-width))
          (left-width ::real (as int (round (* at pane-width))))
          (right-width ::real (- pane-width left-width line-width)))
     (values left-width line-width right-width)))

  ((draw-split!)::void
   (painter:draw-vertical-split! 0))

  ((with-pane-size size::real action::procedure)::Object
   (parameterize ((the-pane-width size))
     (action)))

  ((with-pane-translation shift::real action::procedure)::Object
   (with-translation (shift 0)
     (action)))

  ((area/last line::Area earlier-size::real)::Area
   (Area left: (- line:left earlier-size)
	 top: line:top
	 right: (- line:right earlier-size)
	 bottom: line:bottom))

  ((varying-dimension x::real y::real)::real
   x)

  ((set-translation! pos::real)::void
   (set! translation:left pos))

  ((varying-size w::real h::real size::real)::(Values real real)
   (values size h))

  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   (values (- x shift) y))

  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   (set! at (/ (max 0.0 (* 1.0 (- pointer-left pane-left)))
	       pane-width)))
  )


(define-type (SplitBelow)
  extending Split
  with
  ((part-sizes)::(Values real real real)
   (let* ((pane-height ::real (the-pane-height))
	  (line-height ::real (painter:horizontal-split-height))
          (inner-height ::real (- pane-height line-height))
          (left-height ::real (as int (round (* at inner-height))))
          (right-height ::real (- inner-height left-height)))
     (values left-height line-height right-height)))

  ((draw-split!)::void
   (painter:draw-horizontal-split! 0))

  ((with-pane-size size::real action::procedure)::Object
   (parameterize ((the-pane-height size))
     (action)))

  ((with-pane-translation shift::real action::procedure)::Object
   (with-translation (0 shift)
     (action)))

  ((area/last line::Area earlier-size::real)::Area
   (Area left: line:left
	 top: (- line:top earlier-size)
	 right: line:right
	 bottom: (- line:bottom earlier-size)))

  ((varying-dimension x::real y::real)::real y)

  ((set-translation! pos::real)::void
   (set! translation:top pos))

  ((varying-size w::real h::real size::real)::(Values real real)
   (values w size))

  ((transformed-dimension x::real y::real shift::real)
   ::(Values real real)
   (values x (- y shift)))


  ((resize! pointer-left::real pointer-top::real
	    pane-left::real pane-top::real
	    pane-width::real pane-height::real)
   ::void
   (set! at (/ (max 0.0 (* 1.0 (- pointer-top pane-top)))
	       pane-height)))
  )

(define (halve-beside!)
  (let* ((editor (screen:active))
	 (position (screen-position editor))
	 (extent (screen-extent editor))
	 (center (+ position:left (quotient extent:width 2)))
	 (line (Area left: center
		     top: position:top
		     right: center
		     bottom: (+ position:top
				extent:height)))
	 #;(stroke (Stroke 0 editor)))
#|
    (stroke:add-point! (Position left: line:left
				 top: line:top))
    (stroke:add-point! (Position left: line:right
				 top: line:bottom))
    (screen:add-overlay! stroke)
|#
    (screen:split-beside! line)))


(define (halve-below!)
  (let* ((editor (screen:active))
	 (position (screen-position editor))
	 (extent (screen-extent editor))
	 (center (+ position:top (quotient extent:height 2)))
	 (line (Area left: position:left
		     top: center
		     right: (+ position:left
			       extent:width)
		     bottom: center))
	 (stroke (Stroke 0 editor)))
    (stroke:add-point! (Position left: line:left
				 top: line:top))
    (stroke:add-point! (Position left: line:right
				 top: line:bottom))
    (screen:add-overlay! stroke)
    (screen:split-below! line)))
