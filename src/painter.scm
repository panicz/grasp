(import (define-interface))
(import (define-object))
(import (define-syntax-rule))
(import (define-parameter))
(import (default-value))
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
  (clip! left::real  top::real
	 width::real height::real)::void	 
  (current-clip-width)::real
  (current-clip-height)::real
  (current-clip-left)::real
  (current-clip-top)::real
  )  

(define-interface Scalable ()
  (scale! factor::real)::void
  (current-scale)::real
  )

(define-interface Splittable ()
  (draw-horizontal-line! top::real)::void
  (draw-vertical-line! left::real)::void
  (horizontal-line-height)::real
  (vertical-line-width)::real
  )
  
(define-interface Painter (Splittable
			   Clippable
			   Translatable)

  (mark-cursor! +left::real +top::real)::void
  (cursor-position)::Position
  (cursor-height)::real
  
  (space-width)::real
  
  (paren-width)::real
  (min-box-height)::real
  (min-line-height)::real
  
  (clear!)::void
  
  (draw-quoted-text! s::CharSequence context::Cursor)::void
  (draw-string! s::CharSequence context::Cursor)::void
  (quoted-text-extent text::CharSequence)::Extent
  
  (draw-atom! text::CharSequence context::Cursor)::void

  (atom-character-index-under x::real y::real text::CharSequence)::int
  (quoted-text-character-index-under x::real y::real text::CharSequence)::int
  
  (atom-extent text::CharSequence)::Extent

  (draw-horizontal-bar! width::real)::void
  (draw-vertical-bar! height::real)::void

  (vertical-bar-width)::real
  (horizontal-bar-height)::real

  (draw-box! width::real height::real context::Cursor)::void
  
  (draw-rounded-rectangle! width::real height::real)::void

  (enter-selection-drawing-mode!)::void
  (exit-selection-drawing-mode!)::void
  (in-selection-drawing-mode?)::boolean

  (draw-point! left::real top::real color-rgba::int)::void
  
  )

(define-object (NullPainter)::Painter
  (define (space-width)::real 1)
  
  (define (paren-width)::real 1)

  (define (cursor-height)::real 1)

  (define (min-box-height)::real 1)

  (define (min-line-height)::real 1)
  
  (define (vertical-bar-width)::real 1)

  (define (horizontal-bar-height)::real 1)
  
  (define (clear!)::void
    (values))
  
  (define (translate! x::real y::real)::void
    (values))

  (define (current-translation-left)::real
    0)
  
  (define (current-translation-top)::real
    0)

  (define (clip! left::real  top::real
		 width::real height::real)
    ::void
    (values))
  
  (define (current-clip-width)::real
    0)
  
  (define (current-clip-height)::real
    0)
  
  (define (current-clip-left)::real
    0)
  
  (define (current-clip-top)::real
    0)
  
  (define (draw-horizontal-line! top::real)::void
    (values))
  
  (define (draw-vertical-line! left::real)::void
    (values))

  (define (horizontal-line-height)::real
    0)
  
  (define (vertical-line-width)::real
    0)
  
  (define (draw-quoted-text! s::CharSequence
			     context::Cursor)
    ::void
    (values))
  
  (define (draw-string! s::CharSequence context::Cursor)
    ::void
    (values))

  (define (quoted-text-extent text::CharSequence)::Extent
    (Extent width: 0 height: 0))
  
  (define (draw-atom! text::CharSequence context::Cursor)::void
    (values))

  (define (atom-character-index-under x::real y::real
				      text::CharSequence)
    ::int
    0)
  
  (define (quoted-text-character-index-under x::real y::real
					     text::CharSequence)
    ::int
    0)
  
  (define (atom-extent text::CharSequence)::Extent
    (Extent width: 1 height: 1))
  
  (define (draw-horizontal-bar! width::real)::void
   (values))
  
  (define (draw-vertical-bar! height::real)::void
   (values))

  (define (draw-box! width::real height::real context::Cursor)::void
    (values))

  (define (draw-rounded-rectangle! width::real
				   height::real)
    ::void
    (values))

  (define (mark-cursor! +left::real +top::real)::void
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

  (define (draw-point! left::real top::real color-rgba::int)::void
    (void))
  )

;;(set! (default-value Painter) (NullPainter))

(define-parameter (the-painter) ::Painter (NullPainter))

(define-syntax-rule (with-translation (x y) . actions)
  (let ((painter (the-painter))
	(x! x)
        (y! y))
    (invoke painter 'translate! x! y!)
    (begin . actions)
    (invoke painter 'translate! (- x!) (- y!))))

(define-syntax-rule (with-clip (w h) . actions)
  (let ((painter (the-painter)))
    (let ((x0 (invoke painter 'current-clip-left))
	  (y0 (invoke painter 'current-clip-top))
	  (w0 (invoke painter 'current-clip-width))
	  (h0 (invoke painter 'current-clip-height))
	  (x! (invoke painter 'current-translation-left))
	  (y! (invoke painter 'current-translation-top))
	  (w! w)
	  (h! h))
      (invoke painter 'clip! x! y! w! h!)
      (begin . actions)
      (invoke painter 'clip! x0 y0 w0 h0))))
