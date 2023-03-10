(import (define-cache))
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
(import (examples))
(import (print))

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

(define-type (BlockComment content: Text := (Text))
  implementing Comment
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:draw-block-comment! content context)))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (let ((painter ::Painter (the-painter))
	 (inner ::Extent (extent)))
     (and (is 0 <= x < inner:width)
	  (is 0 <= y < inner:height)
	  (hash-cons (painter:block-comment-character-index-under
		      x y content)
		     path))))

  ((extent)::Extent
   (let ((painter ::Painter (the-painter)))
     (painter:block-comment-extent content)))

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
   (< a b))
  )

(define-type (LineComment content: Text := (Text))
  implementing TextualComment
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:draw-line-comment! content context)))

  ((extent)::Extent
   (let ((painter ::Painter (the-painter)))
     (painter:line-comment-extent content)))

  ((advance! traversal::Traversal)::void
   (let ((size ::Extent (extent)))
     (traversal:advance-by! size:width)
     (traversal:new-line!)))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (let ((painter ::Painter (the-painter))
	 (inner ::Extent (extent)))
     (and (is 0 <= x < inner:width)
	  (is 0 <= y < inner:height)
	  (hash-cons (painter:line-comment-character-index-under
		      x y content)
		     path))))

  ((print out::gnu.lists.Consumer)::void
   (out:append #\;)
   (for c in content
     (out:append (as char c)))
   (out:append #\newline))

  ((part-at index::Index)::Indexable*
   (this))

  ((first-index)::Index
   0)

  ((last-index)::Index
   (string-length content))

  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  ((previous-index index::Index)::Index
   (max (- index 1) (first-index)))

  ((index< a::Index b::Index)::boolean
   (< a b))

  ((insert-char! c::char index::int)::void
   (content:insert-char! c index))
  
  ((delete-char! index::int)::char
   (content:delete-char! index))
  
  ((char-ref index::int)::char
   (content:char-ref index))
  
  ((text-length)::int
   (content:text-length))
  
  ((split! position::int)::Textual
   (let ((splitted ::Text (content:split! position)))
     (LineComment content: splitted)))
   
  ((merge! following::Textual)::boolean
   (and-let* ((next ::LineComment following))
     (content:merge! next:content)))
  )

