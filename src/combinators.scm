(import (define-type))
(import (fundamental))
(import (primitive))
(import (indexable))
(import (space))
(import (cursor))
(import (painter))
(import (extent))
(import (match))
(import (infix))

(define-type (Over back: Tile front: Tile)
  implementing Tile
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

  ((part-at index::Index)::Tile
   (match index
     (,(first-index) back)
     (,(last-index) front)))

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
  )

(define-type (Below top: Tile bottom: Tile)
  implementing Tile
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

  ((part-at index::Index)::Tile
   (match index
     (,(first-index) top)
     (,(last-index) bottom)))

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
  
  )


(define-type (Beside left: Tile right: Tile)
  implementing Tile
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
  
  ((part-at index::Index)::Tile
   (match index
     (,(first-index) left)
     (,(last-index) right)))

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
	   (let ((x (- x left-extent:height))
		 (right-extent (right:extent)))
	     (and (is 0 <= x < right-extent:width)
		  (is 0 <= y < right-extent:height)
		  (right:cursor-under* x y (recons (last-index)
						   path))))))))
  
  ((index< a::Index b::Index)::boolean
   (and (is a eq? (first-index))
	(isnt b eq? (first-index))))

  )
