(module-name (editor interfaces painting))

(import (language define-interface))
(import (language define-object))
(import (language define-type))
(import (language define-syntax-rule))
(import (language define-parameter))

(import (language fundamental))

(import (language examples))
(import (language for))

(define-interface WithCursor ()
  (mark-cursor! left::real top::real)::void
  (marked-cursor-position)::Position

  (set-cursor-column! left::real)::void
  (cursor-column)::real

  (to-next-line)::real
  (to-previous-line)::real
  )

(define-enum TextStyle
  (Bold
   Italic
   Monospace))

(define-alias TextDecoration
  ($bracket-apply$ java.util.EnumSet TextStyle))

(define (RegularText)::TextDecoration
  (EnumSet:noneOf TextStyle))
  
(define-type (EndTextStyle style: TextStyle))

(define (string-extent s::java.lang.CharSequence)::Extent
  (let ((line-length 0)
        (max-length 0)
        (total-lines 1))
    (for c in s
         (cond ((eq? c #\newline)
                (set! max-length (max max-length
				      line-length))
                (set! total-lines (+ total-lines 1))
                (set! line-length 0))
               (else
                (set! line-length (+ line-length 1)))))
    (Extent width: (max max-length line-length)
            height: total-lines)))

(e.g.
 (string-extent "\
abc
def") ===> [Extent width: 3 height: 2])

(define-type (Position left: real := 0
		       top: real := 0))

(define-type (Area left: real top: real
                   right: real bottom: real))

(define (area points::(sequence-of Position))::Area
  (let* ((result ::Area (Area left: +inf.0 top: +inf.0
                              right: -inf.0 bottom: -inf.0)))
    (for p::Position in points
      (set! result:left (min result:left p:left))
      (set! result:top (min result:top p:top))
      (set! result:right (max result:right p:left))
      (set! result:bottom (max result:bottom p:top)))
    result))

(define-interface Map2D ()
  (outside-in x::real y::real)::(Values real real))

(define-interface UnMap2D ()
  (inside-out x::real y::real)::(Values real real))

(define-interface BiMap2D (Map2D UnMap2D))

(define-object (TransformStack)::BiMap2D
  (define transforms ::($bracket-apply$ java.util.ArrayList BiMap2D)
    (($bracket-apply$ java.util.ArrayList BiMap2D)))

  (define (toString)::String
    (transforms:toString))
  
  (define (outside-in x::real y::real)::(Values real real)
    (for transform::BiMap2D in transforms
      (let-values (((x* y*) (transform:outside-in x y)))
	(set! x x*)
	(set! y y*)))
    (values x y))

  (define (inside-out x::real y::real)::(Values real real)
    (for transform::BiMap2D in-reverse transforms
      (let-values (((x* y*) (transform:inside-out x y)))
	(set! x x*)
	(set! y y*)))
    (values x y))

  (define (addLast transform::BiMap2D)::void
    (transforms:add (transforms:size) transform))

  (define (addFirst transform::BiMap2D)::void
    (transforms:add 0 transform))

  (define (removeLast)::BiMap2D
    (let* ((index ::int (- (transforms:size) 1))
	   (result ::BiMap2D (transforms index)))
      (transforms:remove index)
      result))

  (define (removeFirst)::BiMap2D
    (let* ((index ::int 0)
	   (result ::BiMap2D (transforms index)))
      (transforms:remove index)
      result))
  )

(define the-transform-stack ::TransformStack
  (TransformStack))

(define-syntax-rule (with-post-transform t . actions)
  (the-transform-stack:addLast t)
  (try-finally
   (begin . actions)
   (the-transform-stack:removeLast)))

(define-syntax-rule (with-pre-transform t . actions)
  (the-transform-stack:addFirst t)
  (try-finally
   (begin . actions)
   (the-transform-stack:removeFirst)))

(define-syntax-rule (with-highlight highlight-type . actions)
  (painter:begin-highlight! highlight-type)
  (try-finally
   (begin . actions)
   (painter:end-highlight! highlight-type)))

(define-interface Painter ()

  (styled-text-width text::CharSequence style::TextDecoration)::real
  (styled-text-height)::real

  (draw-styled-text! left::real top::real
		     text::CharSequence style::TextDecoration)
  ::void
    
  (translate! x::real y::real)::void
  (rotate! angle::real)::void
  (with-clip w::real h::real action::(maps () to: void))::void
  (scale! factor::real)::void

  (draw-horizontal-split! top::real)::void
  (draw-vertical-split! left::real)::void
  (horizontal-split-height)::real
  (vertical-split-width)::real
  
  (play! animation::Animation)::void
  (playing? animation::Animation)::boolean
  (stop-playing! animation::Animation)::void
  
  (with-intensity i::float action::(maps () to: void))::void
  (with-stretch horizontal::float vertical::float
		action::(maps () to: void))
  ::void
  
  (mark-editor-cursor! +left::real +top::real editor::WithCursor)::void
  (editor-cursor-position editor::WithCursor)::Position

  ;; the functions should only pass (the-editor) as the
  ;; last arguments of the functions above
  (mark-cursor! +left::real +top::real)::void
  (marked-cursor-position)::Position

  (cursor-height)::real

  (line-simplification-resolution)::real
  (space-width)::real

  (module-view-interline)::real
  (module-view-interspace)::real

  (paren-width)::real
  (min-box-height)::real
  (min-line-height)::real

  (icon-extent)::Extent
  (draw-directory-icon!)::void
  (draw-file-icon!)::void

  (press/release-mark-extent)::Extent
  (draw-press-mark! left::real top::real)::void
  (draw-release-mark! left::real top::real)::void
  
  (clear!)::void

  (request-redraw!)::void
  
  (draw-quoted-text! s::CharSequence context::Cursor)
  ::void

  (measure-quoted-text-index-position-into!
   target::Position text::CharSequence index::int)
  ::Position

  (draw-caption! caption::CharSequence)::void
  (caption-extent caption::CharSequence)::Extent
  (caption-margin-top)::real
  (caption-margin-bottom)::real
  (caption-horizontal-margin)::real

  (quoted-text-extent text::CharSequence)::Extent

  (draw-atom! text::CharSequence context::Cursor)::void

  (measure-atom-index-position-into! target::Position
				     text::CharSequence
				     index::int)
  ::Position
  
  (atom-character-index-under x::real y::real
			      text::CharSequence)
  ::int

  (quoted-text-character-index-under
   x::real y::real
   text::CharSequence)
  ::int

  (atom-extent text::CharSequence)::Extent

  (draw-horizontal-bar!
   width::real highlighted?::boolean)
  ::void
   
  (draw-vertical-bar!
   height::real highlighted?::boolean)
  ::void

  (vertical-bar-width)::real
  (horizontal-bar-height)::real

  (draw-box! width::real height::real
	     context::Cursor)
  ::void

  (draw-border! width::real height::real
		highlight::(subtype-of
			    gnu.text.Char
			    (either
			     #\[
			     #\]
			     #\t
			     #\null)))
  ::void

  (border-size)::real
  (height/width-ratio)::real
  
  (draw-rounded-rectangle! width::real
			   height::real)
  ::void

  (draw-rectangle! width::real height::real)::void

  (draw-dashed-rectangle! width::real height::real)::void
  
  (fill-background! width::real height::real)::void
  
  (draw-thick-line! x0::real y0::real x1::real y1::real)
  ::void

  (draw-thin-line! x0::real y0::real x1::real y1::real)
  ::void

  #|
  The `precise` family of methods is intended
  mainly to overcome the limitations for the
  textual client, making use of its capability
  to draw points on the Braille grid, which is
  2 points wide and 4 points tall with respect
  to a single character
  |#
  
  (precise-resolution-right)::ubyte
  (precise-resolution-down)::ubyte
  
  (precise-inside-out px::real py::real)
  ::(Values real real)

  (precise-outside-in px::real py::real)
  ::(Values real real)

  (precise-draw-circle! px0::real py0::real r::real
			color::uint)
  ::void
  
  (precise-fill-circle! px0::real py0::real r::real
			color::uint)
  ::void
  
  (precise-draw-line! px0::real py0::real
		      px1::real py1::real
		      color::uint)
  ::void

  (precise-fill-rectangle! left::real top::real
			   right::real bottom::real
			   color::uint)
  ::void
  
  (begin-highlight! type::HighlightType)::void
  (end-highlight! type::HighlightType)::void
  
  (enter-comment-drawing-mode!)::void
  (exit-comment-drawing-mode!)::void
  (in-comment-drawing-mode?)::boolean

  #||#
  (draw-quote-box! width::real
		   height::real
		   context::Cursor)
  ::void

  (quote-paren-width)::real
  (draw-quote-markers! width::real
		       height::real
		       context::Cursor)
  ::void

  (quote-marker-width)::real

  (draw-quasiquote-box! width::real
			height::real
			context::Cursor)
  ::void

  (quasiquote-paren-width)::real
  (draw-quasiquote-markers! width::real
			    height::real
			    context::Cursor)
  ::void

  (quasiquote-marker-width)::real

  (draw-unquote-box! width::real
		     height::real
		     context::Cursor)
  ::void

  (unquote-paren-width)::real
  (draw-unquote-markers! width::real
			 height::real
			 context::Cursor)
  ::void

  (unquote-marker-width)::real

  (draw-unquote-splicing-box! width::real
			      height::real
			      context::Cursor)
  ::void

  (unquote-splicing-paren-width)::real

  (draw-unquote-splicing-markers! width::real
				  height::real
				  context::Cursor)
  ::void

  (unquote-splicing-marker-width)::real

  (draw-popup! width::real height::real)::void
  (horizontal-popup-margin)::real
  (vertical-popup-margin)::real

  (draw-line-comment! text::CharSequence
		      context::Cursor)
  ::void

  (measure-line-comment-index-position-into!
   target::Position text::CharSequence index::int)
  ::Position

  (line-comment-extent text::CharSequence)::Extent

  (line-comment-character-index-under
   x::real y::real text::CharSequence)
  ::int

  (draw-text-input!
   text::CharSequence context::Cursor)
  ::void

  (measure-text-input-index-position-into!
   target::Position text::CharSequence index::int)
  ::Position

  (text-input-extent text::CharSequence)::Extent

  (text-input-character-index-under
   x::real y::real text::CharSequence)
  ::int
  
  (draw-block-comment!
   text::CharSequence context::Cursor)
  ::void

  (measure-block-comment-index-position-into!
   target::Position text::CharSequence index::int)
  ::Position
  
  (block-comment-extent text::CharSequence)::Extent

  (block-comment-character-index-under
   x::real y::real text::CharSequence)
  ::int

  (draw-horizontal-grid! width::real)::void
  (draw-vertical-grid! height::real)::void
  (grid-border)::real
  (fill-grid-cell! width::real height::real)::void

  (draw-point! left::real top::real
	       color-rgba::int)::void

  )

(define-object (NullPainter)::Painter

  (define (styled-text-width text::CharSequence style::TextDecoration)::real
    1)

  (define (styled-text-height)::real
    1)
  
  (define (draw-styled-text! left::real top::real
			     text::CharSequence style::TextDecoration)
    ::void
    (values))

  (define (play! animation::Animation)::void (values))

  (define (playing? animation::Animation)::boolean #f)

  (define (stop-playing! animation::Animation)::void
    (values))

  (define (module-view-interline)::real 1)
  
  (define (module-view-interspace)::real 1)
  
  (define (space-width)::real 1)

  (define (paren-width)::real 1)

  (define (cursor-height)::real 1)

  (define (min-box-height)::real 1)

  (define (min-line-height)::real 1)

  (define (line-simplification-resolution)::real 1)

  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)

  (define (icon-extent)::Extent
    (Extent width: 1 height: 1))

  (define (draw-directory-icon!)::void
    (values))

  (define (draw-file-icon!)::void
    (values))

  (define (press/release-mark-extent)::Extent
    (Extent width: 1 height: 1))
  
  (define (draw-press-mark! left::real top::real)::void
    (values))
  
  (define (draw-release-mark! left::real top::real)::void
    (values))
  
  (define (clear!)::void
    (values))

  (define (request-redraw!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))

  (define (rotate! angle ::real)::void
    (values))

  (define (scale! factor ::real)::void
    (values))

  (define (with-clip w::real h::real
		     action::(maps () to: void))
    ::void
    (action))

  (define (with-intensity i::float
			  action::(maps () to: void))
    ::void
    (action))

  (define (with-stretch horizontal::float vertical::float
			action::(maps () to: void))
    ::void
    (action))
  
  (define (draw-horizontal-split! top::real)
    ::void
    (values))

  (define (draw-vertical-split! left::real)
    ::void
    (values))

  (define (draw-thick-line! x0::real y0::real
		      x1::real y1::real)
    ::void
    (values))

  (define (draw-thin-line! x0::real y0::real
		      x1::real y1::real)
    ::void
    (values))

  (define (precise-resolution-right)::ubyte 1)
  (define (precise-resolution-down)::ubyte 1)

  (define (precise-inside-out px::real py::real)
    ::(Values real real)
    (values px py))
  
  (define (precise-outside-in x::real y::real)
    ::(Values real real)
    (values x y))

  (define (precise-draw-circle!
	   px0::real py0::real r::real color::uint)
    ::void
    (values))
  
  (define (precise-fill-circle!
	   px0::real py0::real r::real color::uint)
    ::void
    (values))

  (define (precise-fill-rectangle! left::real top::real
				   right::real bottom::real
				   color::uint)
    ::void
    (values))
  
  (define (precise-draw-line!
	   px0::real py0::real px1::real py1::real 
	   color::uint)
    ::void
    (values))
  
  (define (horizontal-split-height)::real
    0)

  (define (vertical-split-width)::real
    0)

  (define (draw-quoted-text! s::CharSequence
			     context::Cursor)
    ::void
    (values))

  (define (draw-string! s::CharSequence
			context::Cursor)
    ::void
    (values))

  (define (draw-caption! caption::CharSequence)::void
    (values))

  (define (caption-extent caption::CharSequence)
    ::Extent
    (Extent width: 1 height: 1))

  (define (caption-margin-top)::real 1)

  (define (caption-margin-bottom)::real 1)

  (define (caption-horizontal-margin)::real 1)

  (define (quoted-text-extent text::CharSequence)::Extent
    (Extent width: 0 height: 0))

  (define (draw-atom! text::CharSequence
		      context::Cursor)
    ::void
    (values))

  (define (atom-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    0)

  (define (quoted-text-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    0)

  (define (atom-extent text::CharSequence)::Extent
    (Extent width: 1 height: 1))

  (define (draw-horizontal-bar! width::real
				highlighted?::boolean)
    ::void
   (values))

  (define (draw-vertical-bar! height::real
			      highlighted?::boolean)
    ::void
   (values))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (values))

  (define (draw-border! width::real
			height::real
			highlight::(subtype-of
				    gnu.text.Char
				    (either
				     #\[ #\]
				     #\t #\null)))
    ::void
    (values))

  (define (border-size)::real
    1)

  (define (height/width-ratio)::real
    1)
  
  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (values))

  (define (draw-rectangle! width::real height::real)
    ::void
    (values))

  (define (draw-dashed-rectangle! width::real height::real)::void
    (values))
  
  (define (fill-background! width::real height::real)::void
    (values))
  
  (define (draw-popup! width::real height::real)::void
    (values))

  (define (horizontal-popup-margin)::real 0)
  (define (vertical-popup-margin)::real 0)

  (define (mark-editor-cursor! +left::real +top::real
			       editor::WithCursor)
    ::void
    (values))

  (define (editor-cursor-position editor::WithCursor)
    ::Position
    (Position left: 0
	      top: 0))
  
  (define (mark-cursor! +left::real +top::real)
    ::void
    (values))

  (define (marked-cursor-position)::Position
    (Position left: 0
	      top: 0))

  (define (begin-highlight! type::HighlightType)::void
    (values))

  (define (end-highlight! type::HighlightType)::void
    (values))

  (define (enter-comment-drawing-mode!)::void
    (values))

  (define (exit-comment-drawing-mode!)::void
    (values))

  (define (in-comment-drawing-mode?)::boolean
    #f)

  (define (draw-text-input! text::CharSequence
			    context::Cursor)
    ::void
    (values))

  (define (text-input-extent text::CharSequence)::Extent
    (Extent width: 0 height: 0))

  (define (text-input-character-index-under
	   x::real y::real text::CharSequence)
    ::int
    0)
  
  (define (draw-line-comment! text::CharSequence
			      context::Cursor)
    ::void
    (values))

  (define (line-comment-extent text::CharSequence)
    ::Extent
    (Extent width: 0 height: 0))

  (define (line-comment-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    0)

  (define (draw-block-comment! text::CharSequence
			       context::Cursor)
    ::void
    (values))

  (define (block-comment-extent text::CharSequence)
    ::Extent
    (Extent width: 0 height: 0))

  (define (block-comment-character-index-under
	   x::real y::real
	   text::CharSequence)
    ::int
    0)

  (define (draw-quote-box! width::real
			   height::real
			   context::Cursor)
    ::void
    (values))

  (define (quote-paren-width)::real 1)

  (define (draw-quote-markers! width::real
			       height::real
			       context::Cursor)
    ::void
    (values))

  (define (quote-marker-width)::real 1)

  (define (draw-quasiquote-box! width::real
				height::real
				context::Cursor)
    ::void
    (values))

  (define (quasiquote-paren-width)::real 1)

  (define (draw-quasiquote-markers! width::real
				    height::real
				    context::Cursor)
    ::void
    (values))

  (define (quasiquote-marker-width)::real 1)

  (define (draw-unquote-box! width::real
			     height::real
			     context::Cursor)
    ::void
    (values))

  (define (unquote-paren-width)::real 1)

  (define (draw-unquote-markers! width::real
				 height::real
				 context::Cursor)
    ::void
    (values))

  (define (unquote-marker-width)::real 1)

  (define (draw-unquote-splicing-box!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (values))

  (define (unquote-splicing-paren-width)::real 1)

  (define (draw-unquote-splicing-markers!
	   width::real
	   height::real
	   context::Cursor)
    ::void
    (values))

  (define (unquote-splicing-marker-width)::real 1)

  (define (draw-horizontal-grid! width::real)::void
    (values))

  (define (draw-vertical-grid! height::real)::void
    (values))

  (define (grid-border)::real 1)

  (define (fill-grid-cell! width::real height::real)::void
    (values))

  (define (draw-point! left::real top::real
		       color-rgba::int)
    ::void
    (values))

  (define (measure-quoted-text-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)

  (define (measure-string-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)

  (define (measure-atom-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)

  (define (measure-line-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)

  (define (measure-block-comment-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)

  (define (measure-text-input-index-position-into!
	   target::Position text::CharSequence index::int)
    ::Position
    target)
  )

(define painter ::Painter
  (NullPainter))

(define (set-painter! p::Painter)::void
  (set! painter p))

(define-syntax-rule (with-translation (x y)
		      . actions)
  (let ((x! ::real x)
        (y! ::real y))
    (painter:translate! x! y!)
    (try-finally
     (begin . actions)
     (painter:translate! (- x!) (- y!)))))

(define-syntax-rule (with-clip (w h) . actions)
  (painter:with-clip w h (lambda () (begin . actions))))
