(import (define-interface))
(import (define-object))
(import (define-type))
(import (define-syntax-rule))
(import (define-parameter))
(import (extent))
(import (fundamental))

(define-interface Translatable ()
  (translate! x::real y::real)::void
  (current-translation-left)::real
  (current-translation-top)::real
  )

(define-interface Rotatable ()
  (rotate! angle::real)::void
  (current-rotation-angle)::real
  )

(define-interface Clippable ()
  (with-clip w::real h::real action::(maps () to: void))::void
  )

(define-interface Scalable ()
  (scale! factor::real)::void
  (current-scale)::real
  )

(define-interface Splittable ()
  (draw-horizontal-split! top::real)::void
  (draw-vertical-split! left::real)::void
  (horizontal-split-height)::real
  (vertical-split-width)::real
  )

(define-interface Painter (Splittable
			   Clippable
			   Translatable
			   Scalable
			   Rotatable)

  (play! animation::Animation)::void
  
  (mark-cursor! +left::real +top::real)::void
  (cursor-position)::Position
  (cursor-height)::real

  (line-simplification-resolution)::real
  (space-width)::real

  (paren-width)::real
  (min-box-height)::real
  (min-line-height)::real

  (icon-extent)::Extent
  (draw-directory-icon!)::void
  (draw-file-icon!)::void

  (clear!)::void

  (draw-quoted-text! s::CharSequence context::Cursor)
  ::void
  (draw-string! s::CharSequence context::Cursor)
  ::void

  (draw-caption! caption::CharSequence)::void
  (caption-extent caption::CharSequence)::Extent
  (caption-margin-top)::real
  (caption-margin-bottom)::real
  (caption-horizontal-margin)::real

  (quoted-text-extent text::CharSequence)::Extent

  (draw-atom! text::CharSequence context::Cursor)
  ::void

  (atom-character-index-under x::real y::real
			      text::CharSequence)
  ::int

  (quoted-text-character-index-under
   x::real y::real
   text::CharSequence)
  ::int

  (atom-extent text::CharSequence)::Extent

  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void

  (vertical-bar-width)::real
  (horizontal-bar-height)::real

  (draw-box! width::real height::real
	     context::Cursor)
  ::void

  (draw-rounded-rectangle! width::real
			   height::real)
  ::void

  (draw-rectangle! width::real height::real)::void

  (fill-background! width::real height::real)::void
  
  (draw-line! x0::real y0::real x1::real y1::real)
  ::void

  (enter-selection-drawing-mode!)::void
  (exit-selection-drawing-mode!)::void
  (in-selection-drawing-mode?)::boolean

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

  (line-comment-extent text::CharSequence)::Extent

  (line-comment-character-index-under
   x::real y::real
   text::CharSequence)
  ::int

  (draw-text-input! text::CharSequence
		    context::Cursor)
  ::void

  (text-input-extent text::CharSequence)::Extent

  (text-input-character-index-under
   x::real y::real
   text::CharSequence)
  ::int
  
  (draw-block-comment! text::CharSequence
		       context::Cursor)
  ::void

  (block-comment-extent text::CharSequence)::Extent

  (block-comment-character-index-under
   x::real y::real
   text::CharSequence)
  ::int

  (draw-horizontal-grid! width::real)::void
  (draw-vertical-grid! height::real)::void
  (grid-border)::real
  (fill-grid-cell! width::real height::real)::void


  (draw-point! left::real top::real
	       color-rgba::int)::void

  )

(define-object (NullPainter)::Painter
  (define (play! animation::Animation)::void (values))
  
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

  (define (clear!)::void
    (values))

  (define (translate! x::real y::real)::void
    (values))

  (define (rotate! angle ::real)::void
    (values))

  (define (current-rotation-angle)::real
    0)

  (define (scale! factor ::real)::void
    (values))

  (define (current-scale)::real
    1)

  (define (current-translation-left)::real
    0)

  (define (current-translation-top)::real
    0)

  (define (with-clip w::real h::real action::(maps () to: void))::void
    (action))

  (define (draw-horizontal-split! top::real)::void
    (values))

  (define (draw-vertical-split! left::real)::void
    (values))

  (define (draw-line! x0::real y0::real
		      x1::real y1::real)
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

  (define (draw-horizontal-bar! width::real)::void
   (values))

  (define (draw-vertical-bar! height::real)::void
   (values))

  (define (draw-box! width::real height::real
		     context::Cursor)
    ::void
    (values))

  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (values))

  (define (draw-rectangle! width::real height::real)
    ::void
    (values))

  (define (fill-background! width::real height::real)::void
    (values))
  
  (define (draw-popup! width::real height::real)::void
    (values))

  (define (horizontal-popup-margin)::real 0)
  (define (vertical-popup-margin)::real 0)

  (define (mark-cursor! +left::real +top::real)
    ::void
    (values))

  (define (cursor-position)::Position
    (Position left: 0
	      top: 0))

  (define (enter-selection-drawing-mode!)::void
    (values))

  (define (exit-selection-drawing-mode!)::void
    (values))

  (define (in-selection-drawing-mode?)::boolean
    #f)

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
		       color-rgba::int)::void
    (values))
  )

(define-parameter (the-painter) ::Painter
  (NullPainter))

(define-syntax-rule (with-translation (x y)
		      . actions)
  (let ((painter ::Painter (the-painter))
	(x! ::real x)
        (y! ::real y))
    (painter:translate! x! y!)
    (try-finally
     (begin . actions)
     (painter:translate! (- x!) (- y!)))))

(define-syntax-rule (with-clip (w h) . actions)
  (let ((painter ::Painter (the-painter)))
    (painter:with-clip w h (lambda () (begin . actions)))))
