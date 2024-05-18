(module-name (editor types extensions combinators))

(import (language match))
(import (language infix))
(import (language define-type))
(import (language define-object))
(import (language fundamental))
(import (utils functions))
(import (editor types primitive))
(import (editor interfaces elements))
(import (editor types spaces))
(import (editor document cursor))
(import (editor interfaces painting))
(import (editor types extensions extensions))

(define-type (Bordered element: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let* ((inner ::Extent (extent+ element))
	  (border ::real (painter:border-size)))
     (painter:draw-border! (+ inner:width (* 2 border))
			   (+ inner:height (* 2 border)))
     (with-translation (border border)
       (element:draw! (recons 'element context)))))

  ((extent)::Extent
   (let* ((inner ::Extent (extent+ element))
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
   (let* ((border ::real (painter:border-size)))
     (element:cursor-under* (- x border) (- y border)
			    (recons 'element path))))
  
  ((tap! finger::byte #;at x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:tap! finger (- x border) (- y border))))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:press! finger (- x border) (- y border))))
  
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:second-press! finger (- x border) (- y border))))
  
  ((double-tap! finger::byte x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:double-tap! finger (- x border) (- y border))))

  ((long-press! finger::byte x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:long-press! finger (- x border) (- y border))))

  ((key-typed! key-code::long context::Cursor)::boolean
   (element:key-typed! key-code (recons 'element context)))

  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))

  ((scroll-up! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:scroll-up! (- x border) (- y border))))
  
  ((scroll-down! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:scroll-down! (- x border) (- y border))))
  
  ((scroll-left! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:scroll-left! (- x border) (- y border))))
  
  ((scroll-right! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:scroll-right! (- x border) (- y border))))

  ((zoom-in! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:zoom-in! (- x border) (- y border))))
  
  ((zoom-out! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:zoom-out! (- x border) (- y border))))

  ((rotate-left! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:rotate-left! (- x border) (- y border))))
  
  ((rotate-right! x::real y::real)::boolean
   (let* ((border ::real (painter:border-size)))
     (element:rotate-right! (- x border) (- y border))))

  )
  
(define-object (Stretched element::Enchanted size::Extent)::Maximizable
  (define (set-size! width::real height::real)::void
    (set! size:width width)
    (set! size:height height))
  
  (define (draw! context::Cursor)::void
    (let ((inner ::Extent (extent+ element)))
      (painter:with-stretch
	  (/ size:width inner:width)
	  (/ size:height inner:height)
	(lambda ()
	  (element:draw! (recons 'element context))))))

  (define (render!)::void
    (draw! '()))
  
  (define (extent)::Extent
    size)

  (define (part-at index::Index)::Indexable*
    (match index
      ('element element)
      (_ (this))))
  
  (define (first-index)::Index
    'element)

  (define (last-index)::Index
    'element)

  (define (next-index index::Index)::Index 'element)

  (define (previous-index index::Index)::Index 'element)

  (define (index< a::Index b::Index)::boolean #f)

  (define (index< a::Index b::Index)::boolean
    (and (is a eq? (first-index))
	 (isnt b eq? (first-index))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y))
	   (path* (recons 'element path)))
      (or (element:cursor-under* x* y* path*)
	  path*)))
  
  (define (tap! finger::byte #;at x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:tap! finger x* y*)))
  
  (define (press! finger::byte #;at x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:press! finger x* y*)))
  
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:second-press! finger x* y*)))
  
  (define (double-tap! finger::byte x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:double-tap! finger x* y*)))

  (define (long-press! finger::byte x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:long-press! finger x* y*)))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (element:key-typed! key-code (recons 'element context)))

  (define (value)::Object
    (invoke-special Base 'to-list cons to-expression))

  (define (scroll-up! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:scroll-up! x* y*)))
  
  (define (scroll-down! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:scroll-down! x* y*)))
  
  (define (scroll-left! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:scroll-left! x* y*)))
  
  (define (scroll-right! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:scroll-right! x* y*)))

  (define (zoom-in! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:zoom-in! x* y*)))
  
  (define (zoom-out! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:zoom-out! x* y*)))

  (define (rotate-left! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:rotate-left! x* y*)))
  
  (define (rotate-right! x::real y::real)::boolean
    (let* ((inner ::Extent (extent+ element))
	   (x* (* (/ size:width inner:width) x))
	   (y* (* (/ size:height inner:height) y)))
      (element:rotate-right! x* y*)))
  
  (MaximizableWidget))

(define-type (Over back: Enchanted front: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let* ((back-context (recons 'back context))
          (front-context (recons 'front context)))
     (back:draw! back-context)
     (front:draw! front-context)))

  ((extent)::Extent
   (let ((front-extent ::Extent (extent+ front))
	 (back-extent ::Extent (extent+ back)))
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
   (let ((front-extent ::Extent (extent+ front))
	 (back-extent ::Extent (extent+ back)))
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

  ((scroll-up! x::real y::real)::boolean
   (or (front:scroll-up! x y)
       (back:scroll-up! x y)))
  
  ((scroll-down! x::real y::real)::boolean
   (or (front:scroll-down! x y)
       (back:scroll-down! x y)))
  
  ((scroll-left! x::real y::real)::boolean
   (or (front:scroll-left! x y)
       (back:scroll-left! x y)))
  
  ((scroll-right! x::real y::real)::boolean
   (or (front:scroll-right! x y)
       (back:scroll-right! x y)))

  ((zoom-in! x::real y::real)::boolean
   (or (front:zoom-in! x y)
       (back:zoom-in! x y)))
  
  ((zoom-out! x::real y::real)::boolean
   (or (front:zoom-out! x y)
       (back:zoom-out! x y)))

  ((rotate-left! x::real y::real)::boolean
   (or (front:rotate-left! x y)
       (back:rotate-left! x y)))
  
  ((rotate-right! x::real y::real)::boolean
   (or (front:rotate-right! x y)
       (back:rotate-right! x y)))

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

  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))
  )

(define-type (Below top: Enchanted bottom: Enchanted)
  implementing Enchanted
  with  
  ((draw! context::Cursor)::void
   (let ((top-context (recons 'top context))
         (bottom-context (recons 'bottom context))
	 (top-extent ::Extent (extent+ top)))
     (top:draw! top-context)
     (with-translation (0 top-extent:height)
       (bottom:draw! bottom-context))))

  ((extent)::Extent
   (let ((top-extent ::Extent (extent+ top))
	 (bottom-extent ::Extent (extent+ bottom)))
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
     (let ((top-extent ::Extent (extent+ top)))
       (or (and (is 0 <= x < top-extent:width)
		(is 0 <= y < top-extent:height)
		(top:cursor-under*
		 x y (recons (first-index) path)))
	   (let ((y (- y top-extent:height))
		 (bottom-extent ::Extent (extent+ bottom)))
	     (and (is 0 <= x < bottom-extent:width)
		  (is 0 <= y < bottom-extent:height)
		  (bottom:cursor-under*
		   x y (recons (last-index) path))))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((tap! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:tap! finger x y)
	 (bottom:tap! finger x (- y top-extent:height)))))
  
  ((press! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:press! finger x y)
	 (bottom:press! finger x (- y top-extent:height)))))
  
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:second-press! finger x y)
	 (bottom:second-press! finger x (- y top-extent:height)))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:double-tap! finger x y)
	 (bottom:double-tap! finger x (- y top-extent:height)))))
  
  ((long-press! finger::byte x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:long-press! finger x y)
	 (bottom:long-press! finger x (- y top-extent:height)))))

  ((scroll-up! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:scroll-up! x y)
	 (bottom:scroll-up! x (- y top-extent:height)))))
  
  ((scroll-down! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:scroll-down! x y)
	 (bottom:scroll-down! x (- y top-extent:height)))))
  
  ((scroll-left! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:scroll-left! x y)
	 (bottom:scroll-left! x (- y top-extent:height)))))
  
  ((scroll-right! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:scroll-right! x y)
	 (bottom:scroll-right! x (- y top-extent:height)))))

  ((zoom-in! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:zoom-in! x y)
	 (bottom:zoom-in! x (- y top-extent:height)))))
  
  ((zoom-out! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:zoom-out! x y)
	 (bottom:zoom-out! x (- y top-extent:height)))))

  ((rotate-left! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:rotate-left! x y)
	 (bottom:rotate-left! x (- y top-extent:height)))))
  
  ((rotate-right! x::real y::real)::boolean
   (let ((top-extent ::Extent (extent+ top)))
     (if (is y < top-extent:height)
	 (top:rotate-right! x y)
	 (bottom:rotate-right! x (- y top-extent:height)))))
  
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

  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))
  )


(define-type (Beside left: Enchanted right: Enchanted)
  implementing Enchanted
  with
  ((draw! context::Cursor)::void
   (let ((left-context (recons 'left context))
         (right-context (recons 'right context))
	 (left-extent ::Extent (extent+ left)))
     (left:draw! left-context)
     (with-translation (left-extent:width 0)
       (right:draw! right-context))))

  ((extent)::Extent
   (let ((left-extent ::Extent (extent+ left))
	 (right-extent ::Extent (extent+ right)))
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
     (let ((left-extent ::Extent (extent+ left)))
       (or (and (is 0 <= x < left-extent:width)
		(is 0 <= y < left-extent:height)
		(left:cursor-under* x y (recons (first-index)
						path)))
	   (let ((x (- x left-extent:width))
		 (right-extent ::Extent (extent+ right)))
	     (and (is 0 <= x < right-extent:width)
		  (is 0 <= y < right-extent:height)
		  (right:cursor-under* x y (recons (last-index)
						   path))))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  ((tap! finger::byte  x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:tap! finger x y)
	 (right:tap! finger (- x left-extent:width) y))))

  ((press! finger::byte x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:press! finger x y)
	 (right:press! finger (- x left-extent:width) y))))
   
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:second-press! finger x y)
	 (right:second-press! finger (- x left-extent:width) y))))

  ((double-tap! finger::byte x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:double-tap! finger x y)
	 (right:double-tap! finger (- x left-extent:width) y))))
   
  ((long-press! finger::byte x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:long-press! finger x y)
	 (right:long-press! finger (- x left-extent:width) y))))

  ((scroll-up! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:scroll-up! x y)
	 (right:scroll-up! (- x left-extent:width) y))))
  
  ((scroll-down! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:scroll-down! x y)
	 (right:scroll-down! (- x left-extent:width) y))))
  
  ((scroll-left! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:scroll-left! x y)
	 (right:scroll-left! (- x left-extent:width) y))))
  
  ((scroll-right! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:scroll-right! x y)
	 (right:scroll-right! (- x left-extent:width) y))))

  ((zoom-in! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:zoom-in! x y)
	 (right:zoom-in! (- x left-extent:width) y))))
  
  ((zoom-out! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:zoom-out! x y)
	 (right:zoom-out! (- x left-extent:width) y))))

  ((rotate-left! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:rotate-left! x y)
	 (right:rotate-left! (- x left-extent:width) y))))
  
  ((rotate-right! x::real y::real)::boolean
   (let ((left-extent ::Extent (extent+ left)))
     (if (is x < left-extent:width)
	 (left:rotate-right! x y)
	 (right:rotate-right! (- x left-extent:width) y))))
  
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
  
  ((value)::Object
   (invoke-special Base 'to-list cons to-expression))
  )

(define (beside first::Enchanted second::Enchanted . rest)::Beside
  (let ((n ::int (length rest)))
    (match n
     (0 (Beside left: first right: second))
     (1 (Beside left: first
		right: (Beside left: second
			       right: (car rest))))
     (2 (Beside left: (Beside left: first
			      right: second)
		right: (apply beside rest)))
     (_
      (let ((n-2/2 ::int (quotient (- n 2) 2)))
	(Beside left: (apply beside first second
			      (take n-2/2 rest))
		right: (apply beside (drop n-2/2 rest))))))))
     
(define (below first::Enchanted second::Enchanted . rest)::Below
  (let ((n ::int (length rest)))
    (match n
     (0 (Below top: first bottom: second))
     (1 (Below top: first
	       bottom: (Below top: second
			      bottom: (car rest))))
     (2 (Below top: (Below top: first
			   bottom: second)
	       bottom: (apply below rest)))
     (_
      (let ((n-2/2 ::int (quotient (- n 2) 2)))
	(Below top: (apply below first second
			   (take n-2/2 rest))
	       bottom: (apply below (drop n-2/2 rest))))))))

(define (bordered element::Enchanted)::Bordered
  (Bordered element: element))
