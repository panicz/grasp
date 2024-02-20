(module-name (editor types comments))

(import (language assert))
(import (language define-cache))
(import (language define-type))
(import (language define-object))
(import (language define-interface))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types spaces))
(import (editor types texts))
(import (language match))
(import (language for))
(import (language infix))
(import (editor interfaces painting))
(import (editor types primitive))

(import (language examples))
(import (utils print))
(import (utils functions))

(define-object (ExpressionComment expression ::Tile)::Comment

  (define (clone)::Element
    (ExpressionComment (copy expression)))

  (define (draw! context::Cursor)::void
   (painter:enter-comment-drawing-mode!)
   (expression:draw! (hash-cons #\; context))
   (painter:exit-comment-drawing-mode!))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
   (expression:cursor-under* x y (hash-cons #\; path)))

  (define (extent)::Extent
   (expression:extent))

  (define (expand! traversal::Traversal)::void
   (traversal:expand! (extent)))

  (define (breaks-line?)::boolean #f)
  
  (define (print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\;)
   (show expression))

  (define (toString)::String
   (string-append "#;" (show->string expression)))

  (define (part-at index::Index)::Indexable*
   (assert (eqv? index #\;))
   expression)

  (define (first-index)::Index #\;)

  (define (last-index)::Index #\;)

  (define (next-index index::Index)::Index #\;)

  (define (previous-index index::Index)::Index #\;)

  (define (index< a::Index b::Index)::Index #f))

(define-object (BlockComment content ::Text)::TextualComment

  (define (clone)::Element
    (BlockComment (copy content)))
  
  (define (draw! context::Cursor)::void
   (painter:draw-block-comment! content context))
  
  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((inner ::Extent (extent)))
       (and (is 0 <= x < inner:width)
	    (is 0 <= y < inner:height)
	    (let ((index (painter:block-comment-character-index-under
			  x y content)))
	      (hash-cons index path))))))
  
  (define (extent)::Extent
   (painter:block-comment-extent content))

  (define (expand! traversal::Traversal)::void
   (traversal:expand! (extent)))

  (define (breaks-line?)::boolean #f)
  
  (define (print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\|)
   (for c::gnu.text.Char in content
     (out:append c))
   (out:append #\|)
   (out:append #\#))

  (define (toString)::String
   (string-append "#|" content "|#"))
  
  (define (part-at index::Index)::Indexable*
   (this))
  
  (define (first-index)::Index 0)

  (define (last-index)::Index
   (string-length content))

  (define (next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  (define (previous-index index::Index)::Index
   (max (- index 1) (first-index)))

  (define (index< a::Index b::Index)::boolean
   (is a < b))
  
  (define (insert-char! c::char index::int)::void
   (content:insert-char! c index))
  
  (define (delete-char! index::int)::char
   (content:delete-char! index))
  
  (define (char-ref index::int)::char
   (content:char-ref index))
  
  (define (text-length)::int
   (content:text-length))
  
  (define (split! position::int)::Textual
   (let ((splitted ::Text (content:split! position)))
     (BlockComment splitted)))
   
  (define (merge! following::Textual)::boolean
   (and-let* ((next ::BlockComment following))
     (content:merge! next:content)))

  (define (removable?)::boolean
   (is (text-length) <= 0))

  (define (remove-from! fragments::list)::list
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

(define-object (LineComment content ::Text)::TextualComment

  (define (clone)::Element
    (LineComment (copy content)))
  
  (define (draw! context::Cursor)::void
   (painter:draw-line-comment! content context))

  (define (extent)::Extent
   (painter:line-comment-extent content))

  (define (expand! traversal::Traversal)::void
   (traversal:expand! (extent))
   (traversal:new-line!))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
   (otherwise #!null
     (let ((inner ::Extent (extent)))
       (and (is 0 <= x < inner:width)
	    (is 0 <= y < inner:height)
	    (hash-cons (painter:line-comment-character-index-under
			x y content)
		       path)))))

  (define (breaks-line?)::boolean #t)
  
  (define (print out::gnu.lists.Consumer)::void
   (out:append #\;)
   (for c in content
     (out:append (as char c)))
   (out:append #\newline))

  (define (toString)::String
   (string-append ";" content "\n"))
  
  (define (part-at index::Index)::Indexable*
   (this))

  (define (first-index)::Index 0)

  (define (last-index)::Index
   (string-length content))

  (define (next-index index::Index)::Index
   (min (+ index 1) (last-index)))

  (define (previous-index index::Index)::Index
   (max (- index 1) (first-index)))

  (define (index< a::Index b::Index)::boolean
   (is a < b))

  (define (insert-char! c::char index::int)::void
   (unless (eqv? c #\newline)
     (content:insert-char! c index)))
  
  (define (delete-char! index::int)::char
   (content:delete-char! index))
  
  (define (char-ref index::int)::char
   (content:char-ref index))
  
  (define (text-length)::int
   (content:text-length))
  
  (define (split! position::int)::Textual
   (let ((splitted ::Text (content:split! position)))
     (LineComment splitted)))
   
  (define (merge! following::Textual)::boolean
   (and-let* ((next ::LineComment following))
     (content:merge! next:content)))

  (define (removable?)::boolean
   (is (text-length) <= 1))

  (define (remove-from! fragments::list)::list
   (let ((that (this)))
     (remove! (is _ eq? that) fragments)))
   )
