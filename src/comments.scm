(import (define-type))
(import (define-interface))
(import (fundamental))
(import (indexable))
(import (space))
(import (text))
(import (match))
(import (for))
(import (infix))

(define-type (ExpressionComment spaces: Space
				expression: Element)  
  implementing Comment
  with
  ((draw! context::Cursor)::void
   ;; powinnismy po prostu zmienic jakies parametry
   ;; ekranu i wywolac najpierw
   ;; (spaces:draw! (recons (first-index) context))
   ;; a nastepnie
   ;; (expression:draw! (recons (last-index) context))
   #!void)
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   #!null)

  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\;)
   (invoke (as Space spaces) 'print out)
   (show expression))
  
  ((first-index)::Index
   'spaces)

  ((last-index)::Index
   'expression)
  
  ((part-at index::Index)::Index
   (match index
     ('spaces spaces)
     ('expression expression)))

  ((next-index index::Index)::Index
   (last-index))

  ((previous-index index::Index)::Index
   (first-index))

  ((index< a::Index b::Index)::Index
   (and (is a eq? (first-index))
	(isnt b eq? (first-index)))))

(define-type (BlockComment content: Text)
  implementing Comment
  with
  ((draw! context::Cursor)::void
   #!void)
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   #!null)
  
  ((print out::gnu.lists.Consumer)::void
   (out:append #\#)
   (out:append #\|)
   (for c::gnu.text.Char in content
     (out:append c))
   (out:append #\|)
   (out:append #\#))
  
  ((part-at index::Index)::Indexable*
   (content:part-at index))
  
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
   #!void)
  
  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   #!null)

  ((print out::gnu.lists.Consumer)::void
   (out:append #\;)
   (for c in content
     (out:append (as char c)))
   (out:append #\newline))

  ((part-at index::Index)::Indexable*
   (content:part-at index))
  
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
