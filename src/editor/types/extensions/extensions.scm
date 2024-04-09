(module-name (editor types extensions extensions))

(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-property))
(import (language define-type))
(import (language define-object))
(import (language keyword-arguments))
(import (language mapping))
(import (language fundamental))
(import (language infix))
(import (language match))

(import (utils functions))
(import (utils print))
(import (utils hash-table))

(import (editor interfaces elements))
(import (editor types primitive))
(import (editor document cursor))
(import (editor types texts))
(import (editor types spaces))

(define-interface Enchanted (Interactive ShadowedTile)
  ;; in the case of extension, the "value" method
  ;; of the Shadowed interface should return a cons-cell
  )

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

  (define (scroll-up! left::real top::real)::boolean #f)
  (define (scroll-down! left::real top::real)::boolean #f)
  (define (scroll-left! left::real top::real)::boolean #f)
  (define (scroll-right! left::real top::real)::boolean #f)

  (define (zoom-in! left::real top::real)::boolean #f)
  (define (zoom-out! left::real top::real)::boolean #f)
  (define (rotate-left! left::real top::real)::boolean #f)
  (define (rotate-right! left::real top::real)::boolean #f)
  
  (define (draw! context::Cursor)::void #!abstract)

  (define (extent)::Extent #!abstract)

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (let ((size (extent)))
	(and (is 0 <= x < size:width)
	     (is 0 <= y < size:height)
	     (recons (invoke (this) 'first-index) path)))))
  
  (define (value)::Object
    (cons (Atom "Magic") (empty)))
  
  (Simple))

(define-object (Dummy item::Tile)::Enchanted

  (define (part-at index::Index)::Indexable*
    (let ((target (content:part-at index)))
      (if (eq? target content)
	  (this)
	  target)))

  (define (first-index)::Index
    (content:first-index))

  (define (last-index)::Index
    (content:last-index))

  (define (next-index index::Index)::Index
    (content:next-index index))

  (define (previous-index index::Index)::Index
    (content:previous-index index))

  (define (index< a::Index b::Index)::boolean
    (content:index< a b)) ;>
  
  (define (draw! context::Cursor)::void
    (item:draw! context))

  (define (extent)::Extent
    (item:extent))

  (define (cursor-under* x::real y::real
			 path::Cursor)
    ::Cursor*
    (item:cursor-under* x y path))

  (define (value)::Object
    (cons (Atom "Dummy")
	  (cons item (empty))))
  
  (Magic))

(define-object (SimpleExtension content::Enchanted)::Enchanted
  (define (tap! finger::byte  x::real y::real)::boolean
    (content:tap! finger x y))

  (define (press! finger::byte x::real y::real)::boolean
    (content:press! finger x y))

  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (content:second-press! finger #;at x y))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (content:double-tap! finger x y))

  (define (long-press! finger::byte x::real y::real)::boolean
    (content:long-press! finger x y))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (content:key-typed! key-code context))

  (define (scroll-up! left::real top::real)::boolean
    (content:scroll-up! left top))

  (define (scroll-down! left::real top::real)::boolean
    (content:scroll-down! left top))

  (define (scroll-left! left::real top::real)::boolean
    (content:scroll-left! left top))

  (define (scroll-right! left::real top::real)::boolean
    (content:scroll-right! left top))

  (define (zoom-in! left::real top::real)::boolean
    (content:zoom-in! left top))

  (define (zoom-out! left::real top::real)::boolean
    (content:zoom-out! left top))

  (define (rotate-left! left::real top::real)::boolean
    (content:rotate-left! left top))

  (define (rotate-right! left::real top::real)::boolean
    (content:rotate-right! left top))

  (define (draw! context::Cursor)::void
    (content:draw! context))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (content:cursor-under* x y path))

  (define (extent)::Extent
    (content:extent))

  (define (typename)::String #!abstract)

  (define (part-at index::Index)::Indexable*
    (let ((target (content:part-at index)))
      (if (eq? target content)
	  (this)
	  target)))

  (define (first-index)::Index
    (content:first-index))

  (define (last-index)::Index
    (content:last-index))

  (define (next-index index::Index)::Index
    (content:next-index index))

  (define (previous-index index::Index)::Index
    (content:previous-index index))

  (define (index< a::Index b::Index)::boolean
    (content:index< a b)) ;>

  (define (value)::Object
    #!abstract)

  (Magic))

(define-syntax dropping-type-signatures
  (syntax-rules (::)
    ((_ all one () (processed ...))
     (all processed ...))

    ((_ all one (arg :: type args ...) (processed ...))
     (dropping-type-signatures
      all one (args ...) (processed ... (one arg))))

    ((_ all one (arg args ...) (processed ...))
     (dropping-type-signatures
      all one (args ...) (processed ... (one arg))))
    ))

(define (disenchanted item #;Tile)::Tile
  (match item
    (magic::Enchanted
     (disenchanted (magic:value)))
    (`(,head . ,tail)
     (cons (disenchanted head) (disenchanted tail)))
    (,@null?
     (empty))
    (_
     item)))

(define-syntax define-simple-extension
  (syntax-rules ()
    ((_ (name args ...) body)
     (define-object (name args ...)::Enchanted 
       (define (typename)::String
	 (symbol->string 'name))

       (define (value)::cons
	 (cons (Atom (symbol->string 'name))
	       (dropping-type-signatures list*
					 disenchanted
					 (args ...)
					 ())))
       (SimpleExtension body)))))

(define-simple-class Extension ()
  ((enchant source::cons)::Enchanted
   #!abstract)
  )

(define (list-expression p::pair)
  (let ((result (cons (to-expression (car p))
		      (empty))))
    (let loop ((p p)
	       (tip result))
      (cond
       ((pair? (cdr p))
	(set-cdr! tip (cons (to-expression
			     (cadr p))
			    (cdr tip)))
	(loop (cdr p) (cdr tip)))
       ((null? (cdr p))
	(cons (Atom "list") result))
       (else
	(error "list-expression called on an improper list "p))))))

(define (to-expression object)
  (match object
    (magic::Enchanted
     (magic:value))
    (struct::ListSerializable
     (struct:to-list (lambda (a d) (cons a d))
		     to-expression))
    (cell::cons
     cell)
    (cell::pair
     (if (list? cell)
	 (list-expression cell)
	 (cons (to-expression (car cell))
               (to-expression (cdr cell)))))
    (s::symbol
     (Atom (symbol->string s)))
    ('()
     (empty))
    (n::number
     (Atom (number->string n)))
    (#!null
     (Atom "#!null"))
    (#t
     (Atom "#true"))
    (#f
     (Atom "#false"))
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
