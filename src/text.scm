(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (match))
(import (examples))
(import (infix))
(import (extent))
(import (fundamental))
(import (indexable))
(import (space))
(import (cursor))
(import (for))
(import (painter))
(import (functions))
(import (print))

(define-object (Text)::Tile
  (define (draw! context::Cursor)
    (invoke (the-painter) 'draw-quoted-text!
	    (this)
	    context))

  (define (extent)::Extent
    (invoke (the-painter) 'quoted-text-extent (this)))

  (define (part-at index::Index)::Indexable*
    (this))

  (define (first-index)::Index
    #\[)
   
  (define (last-index)::Index
    #\])

  (define (next-index index::Index)::Index
    (match index
      (,(first-index) 0)
      (,(last-index) (last-index))
      (,@(is _ < (string-length (this)))
       (+ index 1))
      (_
       (last-index))))
  
  (define (previous-index index::Index)::Index
    (match index
      (0 (first-index))
      (,(last-index) (string-length (this)))
      (,(first-index) (first-index))
      (_ (- index 1))))

  (define (index< a::Index b::Index)::boolean
    (or (and (is a eqv? (first-index))
	     (isnt b eqv? (first-index)))
	(and (number? a) (number? b)
	     (is a < b))
	(and (is b eqv? (last-index))
	     (isnt a eqv? (last-index)))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let ((inner (extent))
	  (painter (the-painter)))
      (and (is 0 <= x < inner:width)
	   (is 0 <= y < inner:height)
	   (hash-cons (painter:quoted-text-character-index-under x y
								 (this))
		   path)
	   )))
  
  (gnu.lists.FString))
