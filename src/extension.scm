(import (hash-table))
(import (define-interface))
(import (define-property))
(import (define-type))
(import (define-object))
(import (keyword-arguments))
(import (mapping))
(import (fundamental))
(import (indexable))
(import (primitive))
(import (infix))
(import (match))
(import (functions))
(import (cursor))
(import (print))
(import (interactive))
(import (text))
(import (space))

(define-interface Enchanted (Interactive Tile)
  (as-expression)::cons)

(define-object (Magic)::Enchanted
  (define (typename)::String "Magic")

  (define (fields->string)::String "")

  (define (fields->list kons::procedure
			transform::procedure)
    ::list
    '())

  (define (hashCode)::int
    (*:hashCode 'Magic))
  
  (define (embedded-in? object)::boolean
    (instance? object Magic))

  (define (assign source::Struct)::Struct
    (this))
  
  (define (key-pressed key::int)::boolean
    #f)
  
  (define (key-released key::int)::boolean
    #f)

  (define (key-typed unicode::int)::boolean
    #f)
  
  (define (tapped x::real y::real)::boolean #f)
  (define (pressed x::real y::real)::boolean #f)
  (define (released x::real y::real)::boolean #f)
  
  (define (dragged-over x::real y::real item::Tile*)::boolean #f)
  (define (dragged-out x::real y::real item::Tile*)::boolean #f)
  (define (dropped x::real y::real item::Tile*)::boolean #f)
  
  (define (held x::real y::real)::boolean #f)
  (define (double-tapped x::real y::real)::boolean #f)
  (define (second-pressed x::real y::real)::boolean #f)

  (define (draw! context::Cursor)::void #!abstract)

  (define (extent)::Extent #!abstract)

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((size (extent)))
	(and (is 0 <= x < size:width)
	     (is 0 <= y < size:height)
	     (recons (invoke (this) 'first-index) path)))))

  (define (as-expression)::cons
    (cons (Atom "Magic") (empty)))
  
  (Simple))

(define-simple-class Extension ()
  ((enchant source::cons)::Enchanted
   #!abstract)
  )

(define-constant extension
  (mapping (keyword)
	   (begin
	     (WARN "no extension for "keyword)
	     #f)))

(define-property (origin enchanted) enchanted)
