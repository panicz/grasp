(import (define-type))
(import (define-interface))
(import (fundamental))
(import (indexable))
(import (space))
(import (text))
(import (match))
(import (for))
(import (infix))
(import (painter))
(import (extent))

(define-type (ExpressionComment expression: Tile)
  implementing Comment
  with
  ((draw! context::Cursor)::void
   ;; powinnismy po prostu zmienic jakies parametry
   ;; ekranu i wywolac
   (expression:draw! context))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (expression:cursor-under* x y path))

  ((extent)::Extent
   (expression:extent))

  ((advance! traversal::Traversal)::void
   (expression:advance! traversal))
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\;)
   (show expression))
  
  ((first-index)::Index
   (expression:first-index))

  ((last-index)::Index
   (expression:last-index))
  
  ((part-at index::Index)::Index
   (expression:part-at index))

  ((next-index index::Index)::Index
   (expression:next-index index))

  ((previous-index index::Index)::Index
   (expression:previous-index index))

  ((index< a::Index b::Index)::Index
   (expression:index< a b)))

(define-type (BlockComment content: Text)
  implementing Comment
  with
  ((draw! context::Cursor)::void
   #!void)
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   #!null)

  ((extent)::Extent
   (Extent width: 1
	   height: 1))

  ((advance! traversal::Traversal)::void
   (traversal:advance/extent! (extent)))
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\|)
   (for c::gnu.text.Char in content
     (out:append c))
   (out:append #\|)
   (out:append #\#))
  
  ((part-at index::Index)::Indexable*
   (this))
  
  ((first-index)::Index
   (content:first-index))

  ((last-index)::Index
   (content:last-index))

  ((next-index index::Index)::Index
   (content:next-index index))

  ((previous-index index::Index)::Index
   (content:previous-index index))

  ((index< a::Index b::Index)::boolean
   (content:index< a b))
  )

(define-type (LineComment content: Text)
  implementing Comment
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:draw-line-comment! content context)))

  ((extent)::Extent
   (let ((painter ::Painter (the-painter)))
     (painter:line-comment-extent content)))

  ((advance! traversal::Traversal)::void
   (traversal:advance/extent! (extent))
   (traversal:new-line!))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   #!null)

  ((print out::gnu.lists.Consumer)::void
   (out:append #\;)
   (for c in content
     (out:append (as char c)))
   (out:append #\newline))

  ((part-at index::Index)::Indexable*
   (this))
  
  ((first-index)::Index
   (content:first-index))

  ((last-index)::Index
   (content:last-index))

  ((next-index index::Index)::Index
   (content:next-index index))

  ((previous-index index::Index)::Index
   (content:previous-index index))

  ((index< a::Index b::Index)::boolean
   (content:index< a b))
  )
