(module-name (editor types extensions button))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (extent))
(import (language match))
(import (language infix))
(import (utils functions))
(import (language fundamental))
(import (editor interfaces indexable))
(import (editor interfaces painter))
(import (editor document cursor))
(import (editor types primitive))
(import (editor types extensions extension))
(import (utils print))
(import (language for))
(import (language while))
(import (editor types space))
(import (editor types text))
(import (editor input input))

(define-object (ColumnGrid items::(sequence-of Enchanted))
  ::Enchanted
  (define (extent)::Extent
    (let* ((painter ::Painter (the-painter))
           (grid-border ::real (painter:grid-border))
           (max-width ::real 0)
           (total-height ::real grid-border))
      (for item::Enchanted in items
        (let ((inner ::Extent (item:extent)))
          (set! max-width (max max-width inner:width))
	  (set! total-height
	        (+ total-height inner:height grid-border))))
      (Extent width: (+ max-width (* 2 grid-border))
              height: total-height)))

  (define (draw! context::Cursor)::void
    (let* ((painter ::Painter (the-painter))
           (grid-border ::real (painter:grid-border))
	   (total ::Extent (extent))
           (n ::int 0)
           (x0 ::real (painter:current-translation-left))
	   (y0 ::real (painter:current-translation-top)))
      (for item::Enchanted in items
	(let ((inner ::Extent (item:extent)))
	  (painter:fill-grid-cell! total:width (+ inner:height
						  (* 2 grid-border)))
	  (painter:draw-horizontal-grid! total:width)
	  (painter:draw-vertical-grid! (+ inner:height
					  (* 2 grid-border)))
	  (with-translation ((- total:width grid-border) 0)
	    (painter:draw-vertical-grid! (+ inner:height
					    (* 2 grid-border))))


	  (with-translation (grid-border grid-border)
	    (item:draw! (recons n context)))
	  (painter:translate! 0 (+ grid-border inner:height))
	  (set! n (+ n 1))))
      (painter:draw-horizontal-grid! total:width)
      (painter:translate!
       (- x0 (painter:current-translation-left))
       (- y0 (painter:current-translation-top)))))

  (define (propagate finger::byte x::real y::real
                     action::(maps (Enchanted byte real real int)
				   to: Object))
    (let* ((painter ::Painter (the-painter))
           (grid-border ::real (painter:grid-border))
	   (ceiling ::real grid-border)
	   (n ::real 0))
      (call/cc
       (lambda (return)
         (for item::Enchanted in items
	   (let ((inner ::Extent (item:extent)))
	     (when (is ceiling <= y < (+ ceiling inner:height))
	       (return (action item finger
			       (- x grid-border) (- y ceiling)
			       n)))
	     (set! ceiling (+ ceiling inner:height grid-border))
	     (set! n (+ n 1))))
	 #f))))

  (define (part-at index::Index)::Indexable*
    (items index))

  (define (first-index)::Index 0)
  (define (last-index)::Index (- (length items) 1))

  (define (next-index index::Index)::Index
    (min (+ index 1) (last-index)))

  (define (previous-index index::Index)::Index
    (max 0 (- index 1)))

  (define (index< a::Index b::Index)::boolean
    (is a < b))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (otherwise #!null
      (propagate 0 x y
		 (lambda (item::Enchanted
			  finger::byte
			  x::real y::real
			  index::int)
		   ::boolean
		   (item:cursor-under* x y (recons index path))))))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    (propagate finger x y
	       (lambda (child::Enchanted
			finger::byte
			x::real y::real
			index::int)
		 ::boolean
		 (child:tap! finger x y))))

  (define (press! finger::byte #;at x::real y::real)::boolean
    (propagate finger x y
	       (lambda (child::Enchanted
			finger::byte
			x::real y::real
			index::int)
		 ::boolean
		 (child:press! finger x y))))

  (define (second-press! finger::byte #;at x::real y::real)::boolean
    (propagate finger x y
	       (lambda (child::Enchanted
			finger::byte
			x::real y::real
			index::int)
		 ::boolean
		 (child:second-press! finger x y))))

  (define (double-tap! finger::byte x::real y::real)::boolean
    (propagate finger x y
	       (lambda (child::Enchanted
			finger::byte
			x::real y::real
			index::int)
		 ::boolean
		 (child:double-tap! finger x y))))

  (define (long-press! finger::byte x::real y::real)::boolean
    (propagate finger x y
	       (lambda (child::Enchanted
			finger::byte
			x::real y::real
			index::int)
		 ::boolean
		 (child:long-press! finger x y))))

  (define (key-typed! key-code::long context::Cursor)::boolean
    ;; na razie tego nie obslugujemy; docelowo warto by bylo
    ;; przemyslec obsluge klawiatury
    #f)

  (define (clone)::Element
    (ColumnGrid items))

  (define (as-expression)
    (cons (Atom "ColumnGrid")
	  (cons (fold-left (lambda (l x)
			     (set-cdr! l (cons (to-expression x) '()))
			     (cdr l))
			   (cons (Atom "list") (empty))
			   items)
		(empty))))
  )

(define-object (Caption content::string)::Enchanted
  (define (draw! context::Cursor)::void
    (let ((painter ::Painter (the-painter)))
      (painter:draw-caption! content)))

  (define (extent)::Extent
    (let ((painter ::Painter (the-painter)))
      (painter:caption-extent content)))

  (define (as-expression)::cons
    (cons (Atom "Caption") (cons (if (Text? content)
				     content
				     (text content))
				 (empty))))

  (define (typename)::String
    "Caption")

  (define (fields->string)::String
    content)

  (define (clone)::Element
    (Caption content))
  
  (Magic))

(define-type (Link
	      on-tap: (maps (Link byte real real) to: boolean)
	      := always
              on-double-tap: (maps (Link byte real real) to: boolean)
	      := always
              on-press: (maps (Link byte real real) to: boolean)
	      := always
              on-second-press: (maps (Link byte real real) to: boolean)
	      := always
              on-long-press: (maps (Link byte real real) to: boolean)
	      := always
              on-key-typed: (maps (Link long Cursor) to: boolean)
	      := always
              content: Enchanted)
  implementing Enchanted
  with
  ((tap! finger::byte x::real y::real)::boolean
   (on-tap (this) finger x y))
  ((press! finger::byte x::real y::real)::boolean
   (on-press (this) finger x y))
  ((second-press! finger::byte #;at x::real y::real)::boolean
   (on-second-press (this) finger x y))
  ((double-tap! finger::byte x::real y::real)::boolean
   (on-double-tap (this) finger x y))
  ((long-press! finger::byte x::real y::real)::boolean
   (on-long-press (this) finger x y))
  ((key-typed! key-code::long context::Cursor)::boolean
   (on-key-typed (this) key-code context))

  ((draw! context::Cursor)::void
   (invoke content 'draw! (recons (first-index) context)))

  ((extent)::Extent
   (content:extent))

  ((cursor-under* x::real y::real path::Cursor)::Cursor*
   (content:cursor-under* x y (recons (first-index) path)))
  ((part-at index::Index)::Indexable* (this))

  ((first-index)::Index 0)
  ((last-index)::Index 0)

  ((next-index index::Index)::Index 0)
  ((previous-index index::Index)::Index 0)

  ((index< a::Index b::Index)::boolean #f)

  ((as-expression)
   (origin (this)))
  )


(define-type (Button action: (maps () to: void)
		     label: string)
  extending Magic
  with
  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (painter:caption-extent label))
	  (horizontal-margin
	   ::real (painter:caption-horizontal-margin))
	  (top-margin ::real
		      (painter:caption-margin-top))
	  (bottom-margin ::real
			 (painter:caption-margin-bottom)))
    (painter:draw-rounded-rectangle!
      (+ inner:width (* horizontal-margin 2))
      (+ inner:height (+ top-margin bottom-margin)))
    (with-translation (horizontal-margin top-margin)
      (painter:draw-caption! label))))

  ((as-expression)::cons
   (origin (this)))

  ((extent)::Extent
   (let* ((painter ::Painter (the-painter))
	  (inner ::Extent (painter:caption-extent label))
	  (horizontal-margin
	   ::real (painter:caption-horizontal-margin))
	  (top-margin ::real
		      (painter:caption-margin-top))
	  (bottom-margin ::real
			 (painter:caption-margin-bottom)))

     (Extent width: (+ inner:width (* horizontal-margin 2))
	     height: (+ inner:height (+ top-margin
					bottom-margin)))))

  ((key-pressed key::char)::boolean
   (cond ((eq? key #\newline)
	  (action)
	  #t)
	 (else
	  #f)))

  ((press! finger::byte x::real y::real)::boolean
   #t)

  ((tap! finger::byte x::real y::real)::boolean
   (action)
   #t))

(set! (extension 'Button)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as Button (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Button from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))

(define-type (FileButton target: java.io.File
                         action: (maps (java.io.File) to: void))
  extending Magic
  with
  ((draw! context::Cursor)::void
   (let* ((painter ::Painter (the-painter))
          (icon ::Extent (painter:icon-extent))
	  (caption ::String (label)))
     (draw-icon!)
     (with-translation (icon:width 0)
       (painter:draw-caption! caption))))

  ((as-expression)::cons
   (origin (this)))

  ((extent)::Extent
   (let* ((painter ::Painter (the-painter))
          (icon ::Extent (painter:icon-extent))
	  (label ::String (label))
	  (caption ::Extent (painter:caption-extent label)))
     (Extent width: (+ icon:width caption:width)
             height: (max icon:height caption:height))))

  ((key-pressed key::char)::boolean
   (cond ((eq? key #\newline)
	  (action target)
	  #t)
	 (else
	  #f)))

  ((label)::String
   (target:getName))

  ((draw-icon!)::void
   (let ((painter ::Painter (the-painter)))
     (painter:draw-file-icon!)))

  ((press! finger::byte x::real y::real)::boolean
   #t)

  ((tap! finger::byte x::real y::real)::boolean
   (action target)
   #t)

  implementing ($bracket-apply$ java.lang.Comparable
				FileButton)
  with
  ((compareTo other::FileButton)::int
   (if (other:target:isDirectory)
      +1
      (target:compareTo other:target))))

(define-object (DirectoryButton)::Enchanted
  (define (typename)::String "DirectoryButton")
  (define (draw-icon!)::void
    (let ((painter ::Painter (the-painter)))
      (painter:draw-directory-icon!)))

  (define (compareTo other::FileButton)::int
    (if (not (other:target:isDirectory))
      -1
      (target:compareTo other:target)))
  (define (clone)::Element
    (DirectoryButton))
  
  (FileButton))


(define-object (ParentDirectoryButton)::Enchanted
  (define (typename)::String "ParentDirectoryButton")
  (define (label) "..")
  (define (compareTo other::FileButton)::int -1)
  (define (clone)::Element
    (ParentDirectoryButton))

  (DirectoryButton))

(define-object (TextInput)::Enchanted

  (define (draw! context::Cursor)
    (invoke (the-painter) 'draw-text-input!
	    (this)
	    context))

  (define (extent)::Extent
    (invoke (the-painter) 'text-input-extent (this)))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (recons path
	    (invoke (the-painter) 'text-input-character-index-under
	    x y (this))))

  (define (part-at index::Index)::Indexable* (this))

  (define (first-index)::Index (as int 0))

  (define (last-index)::Index (string-length (this)))

  (define (next-index index::Index)::Index
    (as int (min (last-index) (+ index 1))))

  (define (previous-index index::Index)::Index
    (as int (max 0 (- index 1))))

  (define (index< a::Index b::Index)::boolean
    (is a < b))

  (define (tap! finger::byte #;at x::real y::real)::boolean
    #t)

  (define (press! finger::byte #;at x::real y::real)::boolean #t)

  (define (second-press! finger::byte #;at x::real y::real)::boolean
    #t)

  (define (double-tap! finger::byte x::real y::real)::boolean
    #t)

  (define (long-press! finger::byte x::real y::real)::boolean
    #t)

  (define (key-typed! key-code::long context::Cursor)::boolean
    (let ((input ::gnu.text.Char (unicode-input))
	  (key-name (key-code-name (java.lang.Integer key-code))))
      (cond
       ((eq? key-name 'backspace)
	(and-let* ((`(,index . ,stem) (the-cursor))
		   ((integer? index))
		   ((is (first-index) < index <= (last-index))))
	  (delete (previous-index index) index)
	  (set! (the-cursor) (recons (previous-index index) stem))
	  #t))

       ((eq? key-name 'left)
        (and-let* ((`(,index . ,stem) (the-cursor)))
          (set! (the-cursor) (recons (previous-index index) stem))
          #t))

       ((eq? key-name 'right)
        (and-let* ((`(,index . ,stem) (the-cursor)))
          (set! (the-cursor) (recons (next-index index) stem))
          #t))

       ((eq? key-name 'delete)
	(and-let* ((`(,index . ,_) (the-cursor))
		   ((integer? index))
		   ((is (first-index) <= index < (last-index))))
	  (delete index (next-index index))
	  #t))

       ((eq? key-name 'enter)
	#f)

       ((isnt input eq? #\null)
	(and-let* ((`(,index . ,stem) (the-cursor))
		   ((integer? index))
		   ((is (first-index) <= index <= (last-index))))
	  (insert index (input:intValue) #t)
          (set! (the-cursor) (recons (next-index index) stem))
	  #t)))))

  (define (as-expression)::cons
    (cons (Atom "text-input")
	  (recons (text (this))
		  (EmptyListProxy (EmptySpace)))))

    (define (clone)::Element
    (let ((new ::TextInput (TextInput)))
      (new:append (this))
      new))
  
  (gnu.lists.FString))

(define (text-input string::CharSequence)::TextInput
  (let ((result ::TextInput (TextInput)))
    (result:append string)
    result))
