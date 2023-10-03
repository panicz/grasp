(import (utils hash-table))
(import (language define-interface))
(import (language define-property))
(import (language define-type))
(import (language define-object))
(import (language keyword-arguments))
(import (language mapping))
(import (language fundamental))
(import (indexable))
(import (primitive))
(import (language infix))
(import (language match))
(import (utils functions))
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

  (define (tap! finger::byte  x::real y::real)::boolean #f)
  (define (press! finger::byte x::real y::real)::boolean #f)
  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #f)
  (define (double-tap! finger::byte x::real y::real)::boolean #f)
  (define (long-press! finger::byte x::real y::real)::boolean #f)
  (define (key-typed! key-code::long context::Cursor)::boolean #f)
  
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

(define (to-expression object)
  (match object
    (magic::Enchanted
     (magic:as-expression))
    (struct::ListSerializable
     (struct:to-list (lambda (a d) (cons a d))
		     to-expression))
    (cell::cons
     cell)
    (cell::pair
     (cons (to-expression (car pair))
           (to-expression (cdr pair))))
    (s::symbol
     (Atom (symbol->string s)))
    (,@(null? object)
     (empty))
    (n::number
     (Atom (number->string n)))
    (t::Text
     t)
    (s::string
     (text s))))

(define-constant extension
  (mapping (keyword)
	   (begin
	     (WARN "no extension for "keyword)
	     #f)))

(define-property (origin enchanted) enchanted)
