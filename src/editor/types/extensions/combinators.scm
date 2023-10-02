(module-name (editor types extensions combinators))

(import (language define-type))
(import (language fundamental))
(import (utils functions))
(import (editor types primitive))
(import (editor interfaces indexable))
(import (editor types space))
(import (editor document cursor))
(import (editor interfaces painter))
(import (extent))
(import (language match))
(import (language infix))
(import (editor types extensions interactive))
(import (editor types extensions extension))

(define-type (Bordered element: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (element:extent))
	  (border ::real (painter:border-size)))
     (painter:draw-border! (+ inner:width (* 2 border))
			   (+ inner:height (* 2 border)))
     (with-translation (border border)
       (element:draw! (recons 'element context)))))

  ((extent)::Extent
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (element:extent))
	  (border ::real (painter:border-size)))
     (Extent width: (+ inner:width (* 2 border))
	     height: (+ inner:height (* 2 border)))))

  ((part-at index::Index)::Indexable*
   (match index
     ('element element)
     (_ (this))))
  
  ((first-index)::Index
   'element)

  ((last-index)::Index
   'element)

  ((next-index index::Index)::Index 'element)

  ((previous-index index::Index)::Index 'element)

  ((index< a::Index b::Index)::boolean #f)

  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:cursor-under* (- x border) (- y border)
			    (recons 'element path))))
  
  ((tap! finger::byte #;at x::real y::real)::boolean
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:tap! finger (- x border) (- y border))))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:press! finger (- x border) (- y border))))
  
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:second-press! finger (- x border) (- y border))))
  
  ((double-tap! finger::byte x::real y::real)::boolean
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:double-tap! finger (- x border) (- y border))))

  ((long-press! finger::byte x::real y::real)::boolean
   (let* ((painter ::Painter (the-painter))
	  (border ::real (painter:border-size)))
     (element:long-press! finger (- x border) (- y border))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (element:key-typed! key-code (recons 'element context)))

  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression))
  )

(define-type (Over back: Enchanted front: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let* ((back-context (recons 'back context))
          (front-context (recons 'front context)))
     (back:draw! back-context)
     (front:draw! front-context)))

  ((extent)::Extent
   (let ((front-extent (front:extent))
	 (back-extent (back:extent)))
     (Extent width: (max front-extent:width
			 back-extent:width)
	     height: (max front-extent:height
			  back-extent:height))))

  ((part-at index::Index)::Indexable*
   (match index
     (,(first-index) back)
     (,(last-index) front)
     (_ (this))))

  ((first-index)::Index
   'back)

  ((last-index)::Index
   'front)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
   (let ((front-extent (front:extent))
	 (back-extent (back:extent)))
     (or (and (is 0 <= x < front-extent:width)
	      (is 0 <= y < front-extent:height)
	      (front:cursor-under*
	       x y (recons (last-index) path)))
	 (and (is 0 <= x < back-extent:width)
	      (is 0 <= y < back-extent:height)
	      (back:cursor-under*
	       x y (recons (first-index) path)))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (or (front:tap! finger x y)
       (back:tap! finger x y)))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (or (front:press! finger x y)
       (back:press! finger x y)))

  ((second-press! finger::byte #;at x::real y::real)::boolean
   (or (front:second-press! finger x y)
       (back:second-press! finger x y)))
  
  ((double-tap! finger::byte x::real y::real)::boolean
   (or (front:double-tap! finger x y)
       (back:double-tap! finger x y)))

  ((long-press! finger::byte x::real y::real)::boolean
   (or (front:long-press! finger x y)
       (back:long-press! finger x y)))
  
  ((key-typed! key-code::long context::Cursor)::boolean
   (let* ((cursor (the-cursor))
	  (first-context (recons (first-index) context))
	  (last-context (recons (last-index) context)))
     (cond
      ((is first-context suffix? cursor)
       (front:key-typed! key-code first-context))
      ((is last-context suffix? cursor)
       (back:key-typed! key-code last-context))
      (else
       #f))))

  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression))
  )

(define-type (Below top: Enchanted bottom: Enchanted)
  implementing Enchanted
  with  
  ((draw! context::Cursor)::void
   (let ((top-context (recons 'top context))
         (bottom-context (recons 'bottom context))
	 (top-extent (top:extent)))
     (top:draw! top-context)
     (with-translation (0 top-extent:height)
       (bottom:draw! bottom-context))))

  ((extent)::Extent
   (let ((top-extent (top:extent))
	 (bottom-extent (bottom:extent)))
     (Extent width: (max top-extent:width
			 bottom-extent:width)
	     height: (+ top-extent:height
			bottom-extent:height))))

  ((part-at index::Index)::Indexable*
   (match index
     (,(first-index) top)
     (,(last-index) bottom)
     (_ (this))))

  ((first-index)::Index
   'top)

  ((last-index)::Index
   'bottom)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((top-extent (top:extent)))
       (or (and (is 0 <= x < top-extent:width)
		(is 0 <= y < top-extent:height)
		(top:cursor-under*
		 x y (recons (first-index) path)))
	   (let ((y (- y top-extent:height))
		 (bottom-extent (bottom:extent)))
	     (and (is 0 <= x < bottom-extent:width)
		  (is 0 <= y < bottom-extent:height)
		  (bottom:cursor-under*
		   x y (recons (last-index) path))))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent (top:extent)))
     (if (is y < top-extent:height)
	 (top:tap! finger x y)
	 (bottom:tap! finger x (- y top-extent:height)))))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent (top:extent)))
     (if (is y < top-extent:height)
	 (top:press! finger x y)
	 (bottom:press! finger x (- y top-extent:height)))))
  
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent (top:extent)))
     (if (is y < top-extent:height)
	 (top:second-press! finger x y)
	 (bottom:second-press! finger x (- y top-extent:height)))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (let ((top-extent (top:extent)))
     (if (is y < top-extent:height)
	 (top:double-tap! finger x y)
	 (bottom:double-tap! finger x (- y top-extent:height)))))
  
  ((long-press! finger::byte x::real y::real)::boolean
   (let ((top-extent (top:extent)))
     (if (is y < top-extent:height)
	 (top:long-press! finger x y)
	 (bottom:long-press! finger x (- y top-extent:height)))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (let* ((cursor (the-cursor))
	  (first-context (recons (first-index) context))
	  (last-context (recons (last-index) context)))
     (cond
      ((is first-context suffix? cursor)
       (top:key-typed! key-code first-context))
      ((is last-context suffix? cursor)
       (bottom:key-typed! key-code last-context))
      (else
       #f))))

  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression))
  )


(define-type (Beside left: Enchanted right: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let ((left-context (recons 'left context))
         (right-context (recons 'right context))
	 (left-extent (left:extent)))
     (left:draw! left-context)
     (with-translation (left-extent:width 0)
       (right:draw! right-context))))

  ((extent)::Extent
   (let ((left-extent (left:extent))
	 (right-extent (right:extent)))
     (Extent width: (+ left-extent:width
		       right-extent:width)
	     height: (max left-extent:height
			  right-extent:height))))
  
  ((part-at index::Index)::Indexable*
   (match index
     (,(first-index) left)
     (,(last-index) right)
     (_ (this))))

  ((first-index)::Index
   'left)

  ((last-index)::Index
   'right)
  
  ((next-index index::Index)::Index
   (last-index))
   
  ((previous-index index::Index)::Index
   (first-index))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((left-extent (left:extent)))
       (or (and (is 0 <= x < left-extent:width)
		(is 0 <= y < left-extent:height)
		(left:cursor-under* x y (recons (first-index)
						path)))
	   (let ((x (- x left-extent:width))
		 (right-extent (right:extent)))
	     (and (is 0 <= x < right-extent:width)
		  (is 0 <= y < right-extent:height)
		  (right:cursor-under* x y (recons (last-index)
						   path))))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((tap! finger::byte  x::real y::real)::boolean
   (let ((left-extent (left:extent)))
     (if (is x < left-extent:width)
	 (left:tap! finger x y)
	 (right:tap! finger (- x left-extent:width) y))))

  ((press! finger::byte x::real y::real)::boolean
   (let ((left-extent (left:extent)))
     (if (is x < left-extent:width)
	 (left:press! finger x y)
	 (right:press! finger (- x left-extent:width) y))))
   
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let ((left-extent (left:extent)))
     (if (is x < left-extent:width)
	 (left:second-press! finger x y)
	 (right:second-press! finger (- x left-extent:width) y))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (let ((left-extent (left:extent)))
     (if (is x < left-extent:width)
	 (left:double-tap! finger x y)
	 (right:double-tap! finger (- x left-extent:width) y))))
   
  ((long-press! finger::byte x::real y::real)::boolean
   (let ((left-extent (left:extent)))
     (if (is x < left-extent:width)
	 (left:long-press! finger x y)
	 (right:long-press! finger (- x left-extent:width) y))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (let* ((cursor (the-cursor))
	  (first-context (recons (first-index) context))
	  (last-context (recons (last-index) context)))
     (cond
      ((is first-context suffix? cursor)
       (left:key-typed! key-code first-context))
      ((is last-context suffix? cursor)
       (right:key-typed! key-code last-context))
      (else
       #f))))
  
  ((as-expression)::cons
   (invoke-special Base 'to-list cons to-expression))
  )
