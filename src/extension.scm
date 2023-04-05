(import (hash-table))
(import (define-interface))
(import (define-property))
(import (define-type))
(import (define-object))
(import (mapping))
(import (fundamental))
(import (indexable))
(import (primitive))
(import (infix))
(import (match))
(import (functions))
(import (cursor))
(import (document-operations))
(import (print))
(import (interactive))

(define-interface Enchanted (Interactive Tile))

(define-object (Magic)::Enchanted
  (define (typename)::String "Magic")

  (define (fields->string)::String "")

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

  (Simple))

(define-interface Extension ()
  (create-from source::cons)::Enchanted
  )

(define-mapping (extension keyword)
  (begin
    (WARN "no extension for "keyword)
    #f))

(define-property (origin enchanted)
  enchanted)

(define (enchant-expression! #!optional
			     #;at (cursor::Cursor (the-cursor))
				  #;in (document (the-document)))
  (parameterize ((the-cell-access-mode CellAccessMode:Evaluating))
    (let ((target (the-expression)))
      (if (Interactive? target)
	  (let ((original (origin target)))
	    (unset! (origin target))
	    (replace-expression! at: cursor
				 with: original
				 in: document))
	  (and-let* ((target (if (pair? target)
				 target
				 (innermost-composition
				  in: document
				  at: cursor)))
		     (`(,name . ,_) target)
		     ((symbol? name))
		     (extension (extension name))
		     (illustration (invoke (as Extension
					       extension)
					   'create-from target)))
	    (set! (origin illustration) target)
	    (replace-expression! at: cursor
				 with: illustration
				 in: document))))))
