(module-name (editor types texts))

(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language attributes))
(import (language define-cache))
(import (language match))
(import (language examples))
(import (language infix))

(import (language fundamental))
(import (editor interfaces elements))
(import (editor document cursor))
(import (language for))
(import (editor interfaces painting))
(import (utils functions))
(import (utils print))

(define-object (Text)::TextualTile

  (define (draw! context::Cursor)
    (painter:draw-quoted-text!
     (this)
     context))

  (define (extent)::Extent
    (painter:quoted-text-extent (this)))

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
    (otherwise #!null
      (let ((inner (extent)))
	(and (is 0 <= x < inner:width)
	     (is 0 <= y < inner:height)
	     (hash-cons (painter:quoted-text-character-index-under
			 x y (this))
			path)
	     ))))

  (define (measure-position #;of cursor::Cursor
				 #;into target::Position
					#;within context::Cursor)
    ::Position
    (match cursor
      (`(,index::integer . ,_)
       (painter:measure-quoted-text-index-position-into!
	target name index))
      (_
       target)))
  
  (define (insert-char! c::char index::int)::void
    (insert index c #t))
  
  (define (delete-char! index::int)::char
    (let ((result (char-ref index)))
      (delete index (+ index 1))
      result))
  
  (define (char-ref index::int)::char
    (invoke-special gnu.lists.AbstractCharVector (this)
		    'charAt index))

  (define (text-length)::int
    (invoke-special gnu.lists.FString (this) 'size))
  
  (define (split! start::int)::Textual
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
			'appendCharacter (char->integer c)))
      #t))

  (define (clone)::Element
    (let ((new ::Text (Text)))
      (new:append (this))
      new))
  
  (gnu.lists.FString))

(define (text string::CharSequence)::Text
  (let ((result ::Text (Text)))
    (result:append string)
    result))
