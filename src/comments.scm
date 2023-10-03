(import (language assert))
(import (language define-cache))
(import (language define-type))
(import (language define-interface))
(import (language fundamental))
(import (indexable))
(import (space))
(import (text))
(import (language match))
(import (language for))
(import (language infix))
(import (painter))
(import (extent))
(import (language examples))
(import (print))
(import (utils functions))

(define-type (ExpressionComment expression: Tile)
  implementing Comment
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:enter-comment-drawing-mode!)
     (expression:draw! (hash-cons #\; context))
     (painter:exit-comment-drawing-mode!)))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (expression:cursor-under* x y (hash-cons #\; path)))

  ((extent)::Extent
   (expression:extent))

  ((expand! traversal::Traversal)::void
   (traversal:expand! (extent)))

  ((breaks-line?)::boolean #f)
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\;)
   (show expression))

  ((toString)::String
   (string-append "#;" (show->string expression)))

  ((part-at index::Index)::Indexable*
   (assert (eqv? index #\;))
   expression)

  ((first-index)::Index #\;)

  ((last-index)::Index #\;)

  ((next-index index::Index)::Index #\;)

  ((previous-index index::Index)::Index #\;)

  ((index< a::Index b::Index)::Index #f))

(define-type (BlockComment content: Text := (Text))
  implementing TextualComment
  with
  ((draw! context::Cursor)::void
   (let ((painter ::Painter (the-painter)))
     (painter:draw-block-comment! content context)))
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((painter ::Painter (the-painter))
	   (inner ::Extent (extent)))
       (and (is 0 <= x < inner:width)
	    (is 0 <= y < inner:height)
	    (let ((index (painter:block-comment-character-index-under
			  x y content)))
	      (hash-cons index path))))))
  
  ((extent)::Extent
   (let ((painter ::Painter (the-painter)))
     (painter:block-comment-extent content)))

  ((expand! traversal::Traversal)::void
   (traversal:expand! (extent)))

  ((breaks-line?)::boolean #f)
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\|)
   (for c::gnu.text.Char in content
     (out:append c))
   (out:append #\|)
   (out:append #\#))

  ((toString)::String
   (string-append "#|" content "|#"))
  
  ((part-at index::Index)::Indexable*
   (this))
  
  ((first-index)::Index 0)

  ((last-index)::Index
   (string-length content))

  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  ((previous-index index::Index)::Index
   (max (- index 1) (first-index)))

  ((index< a::Index b::Index)::boolean
   (is a < b))
  
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
     (BlockComment content: splitted)))
   
  ((merge! following::Textual)::boolean
   (and-let* ((next ::BlockComment following))
     (content:merge! next:content)))

  ((removable?)::boolean
   (is (text-length) <= 0))

  ((remove-from! fragments::list)::list
   (let ((that (this)))
     (cond
      ((first-cell (lambda (l)
		     (and-let* ((`(,n::integer
				   ,,@(is _ eq? that)
				   ,m::integer . ,_) l))))
		   fragments)
       => (lambda (l)
	    (set! (car l) (as int (+ (car l) (caddr l))))
	    (set! (cdr l) (cdddr l))
	    fragments))
      (else
       (WARN "unable to remove "that" from "fragments)
       fragments))))
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

  ((expand! traversal::Traversal)::void
   (traversal:expand! (extent))
   (traversal:new-line!))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((painter ::Painter (the-painter))
	   (inner ::Extent (extent)))
       (and (is 0 <= x < inner:width)
	    (is 0 <= y < inner:height)
	    (hash-cons (painter:line-comment-character-index-under
			x y content)
		       path)))))

  ((breaks-line?)::boolean #t)
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\;)
   (for c in content
     (out:append (as char c)))
   (out:append #\newline))

  ((toString)::String
   (string-append ";" content "\n"))
  
  ((part-at index::Index)::Indexable*
   (this))

  ((first-index)::Index 0)

  ((last-index)::Index
   (string-length content))

  ((next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  ((previous-index index::Index)::Index
   (max (- index 1) (first-index)))

  ((index< a::Index b::Index)::boolean
   (is a < b))

  ((insert-char! c::char index::int)::void
   (unless (eqv? c #\newline)
     (content:insert-char! c index)))
  
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

  ((removable?)::boolean
   (is (text-length) <= 1))

  ((remove-from! fragments::list)::list
   (let ((that (this)))
     (remove! (is _ eq? that) fragments)))
   )
