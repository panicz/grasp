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

(define-object (Text)::TextualTile
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
	   (hash-cons (painter:quoted-text-character-index-under
		       x y (this))
		      path)
	   )))

  (define (insert-char! c::char index::int)::void
    (insert index c #t))
  
  (define (delete-char! index::int)::char
    (delete index (+ index 1)))
  
  (define (char-ref index::int)::char
    (invoke-special gnu.lists.AbstractCharVector (this)
		    'charAt index))

  (define (text-length)::int
    (invoke-special gnu.lists.FString (this) 'size))
  
  (define (split! start::int)::Text
    (let ((copy ::Text (Text))
	  (n (text-length)))
      (for i from start below n
	   (copy:appendCharacter (char-ref i)))
      (delete (max 0 (- n length)) n)
      copy))

  (define (merge! next::Textual)::boolean
    (and-let* ((next ::Text next))
      (for c in next
	(invoke-special gnu.lists.FString (this)
			'appendCharacter c))
      #t))
  
  (gnu.lists.FString))
