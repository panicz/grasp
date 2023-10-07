(module-name (editor types extensions quotations))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (utils functions))
(import (language fundamental))

(import (editor interfaces painting))
(import (editor types primitive))
(import (editor interfaces elements))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor types extensions interactions))
(import (editor types extensions extensions))
(import (utils print))

(define-object (Quotation expression ::Tile)::Enchanted
  
  (define (draw-box! painter::Painter
		     width::real
		     height::real
		     context::Cursor)
    ::void
    #!abstract)

  (define (paren-width painter::Painter)::real
    #!abstract)

  (define (marker-width painter::Painter)::real
    #!abstract)

  (define (draw-markers! painter::Painter
			 width::real
			 height::real
			 context::Cursor)
    ::void
    #!abstract)

  (define (as-expression)::cons
    (cons (Atom (typename))
	  (cons expression (empty))))

  (define (typename)::String #!abstract)
  
  (define (fields->string)::String
    (string-append " "(expression:toString)))
  
  (define (part-at index::Index)::Indexable*
    (if (or (eqv? index #\[)
	    (eqv? index #\]))
	(this)
	(if (gnu.lists.LList? expression)
	    (let ((target ::Indexable (expression:part-at index)))
	      (if (eq? target expression)
		  (this)
		  target))
	    expression)))
    
  (define (first-index)::Index #\[)

  (define (last-index)::Index #\])

  (define (next-index index::Index)::Index
    (if (gnu.lists.LList? expression)
	(expression:next-index index)
	(match index
	  (#\[ 0)
	  (_ #\]))))
    
  (define (previous-index index::Index)::Index
    (if (gnu.lists.LList? expression)
	(expression:previous-index index)
	(match index
	  (#\] 0)
	  (_ #\[))))
    
  (define (index< a::Index b::Index)::boolean
    (and (isnt a eqv? b)
	 (or (eqv? a #\[)
	     (eqv? b #\])
	     (and (isnt a eqv? #\])
		  (isnt b eqv? #\[)
		  (expression:index< a b)))))

  (define (draw! context::Cursor)::void
    (let ((painter ::Painter (the-painter)))
      (if (gnu.lists.LList? expression)
	  (let* ((inner ::Extent (sequence-extent expression))
		 (paren-width ::real (paren-width painter)))
	    (draw-box! painter
		       (+ inner:width (* 2 paren-width))
		       inner:height
		       context)
	    (with-translation (paren-width 0)
	      (draw-sequence! expression context: context)))
	  (let* ((inner ::Extent (extent+ expression))
		 (marker-width (marker-width painter)))
	    (draw-markers! painter
			   inner:width
			   inner:height
			   context)
	    (with-translation (marker-width 0)
	      (expression:draw! (recons 0 context)))))))

  (define (cursor-under* x::real y::real path::Cursor)::Cursor*
    (let* ((painter ::Painter (the-painter))
	   (inner ::Extent (if (gnu.lists.LList? expression)
			       (sequence-extent expression)
			       (extent+ expression)))
	   (lag-width ::real (if (gnu.lists.LList? expression)
				 (paren-width painter)
				 (marker-width painter)))
	   (path*  (if (gnu.lists.LList? expression)
		       path
		       (recons 0 path))))
      (otherwise #!null
      	(and (is 0 <= y < inner:height)
	     (or (and (is 0 <= x < lag-width)
		      (recons (first-index) path))
		 
		 (and (is 0 <= (- x lag-width) < inner:width)
		      (cursor-under (- x lag-width) y
				    expression
				    context: path*))
		 (and (is 0 <= (- x lag-width inner:width)
			  < lag-width)
		      (recons (last-index) path)))))))

  (define (extent)::Extent
    (let* ((painter ::Painter (the-painter))
	   (inner ::Extent (if (gnu.lists.LList? expression)
			       (sequence-extent expression)
			       (extent+ expression)))
	   (lag-width ::real (if (gnu.lists.LList? expression)
				 (paren-width painter)
				 (marker-width painter))))
      (Extent width: (+ inner:width (* 2 lag-width))
	      height: inner:height)))
    
  (Magic))


(define-object (Quote expression ::Tile)::Enchanted

  (define (draw-box! painter::Painter
		     width::real
		     height::real
		     context::Cursor)
    ::void
    (painter:draw-quote-box! width height context))

  (define (paren-width painter::Painter)::real
    (painter:quote-paren-width))

  (define (marker-width painter::Painter)::real
    (painter:quote-marker-width))

  (define (draw-markers! painter::Painter
			 width::real
			 height::real
			 context::Cursor)
    ::void
    (painter:draw-quote-markers! width height context))

  (define (typename)::String "quote")

  (define (toString)::String
    (string-append "'" (show->string expression)))
  
  (Quotation expression))

(define-object (Quasiquote expression ::Tile)::Enchanted

  (define (draw-box! painter::Painter
		     width::real
		     height::real
		     context::Cursor)
    ::void
    (painter:draw-quasiquote-box! width height context))

  (define (paren-width painter::Painter)::real
    (painter:quasiquote-paren-width))

  (define (marker-width painter::Painter)::real
    (painter:quasiquote-marker-width))

  (define (draw-markers! painter::Painter
			 width::real
			 height::real
			 context::Cursor)
    ::void
    (painter:draw-quasiquote-markers! width height context))

  (define (typename)::String "quasiquote")

  (define (toString)::String
    (string-append "`" (show->string expression)))
  
  (Quotation expression))

(define-object (Unquote expression ::Tile)::Enchanted

  (define (draw-box! painter::Painter
		     width::real
		     height::real
		     context::Cursor)
    ::void
    (painter:draw-unquote-box! width height context))

  (define (paren-width painter::Painter)::real
    (painter:unquote-paren-width))

  (define (marker-width painter::Painter)::real
    (painter:unquote-marker-width))

  (define (draw-markers! painter::Painter
			 width::real
			 height::real
			 context::Cursor)
    ::void
    (painter:draw-unquote-markers! width height context))

  (define (typename)::String "unquote")

  (define (toString)::String
    (string-append "," (show->string expression)))
  
  (Quotation expression))

(define-object (UnquoteSplicing expression ::Tile)::Enchanted

  (define (draw-box! painter::Painter
		     width::real
		     height::real
		     context::Cursor)
    ::void
    (painter:draw-unquote-splicing-box! width height context))

  (define (paren-width painter::Painter)::real
    (painter:unquote-splicing-paren-width))

  (define (marker-width painter::Painter)::real
    (painter:unquote-splicing-marker-width))

  (define (draw-markers! painter::Painter
			 width::real
			 height::real
			 context::Cursor)
    ::void
    (painter:draw-unquote-splicing-markers!
     width height context))

  (define (typename)::String "unquote-splicing")

  (define (toString)::String
    (string-append ",@" (show->string expression)))
  
  (Quotation expression))

(set! (extension 'quote)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (otherwise #!null
	    (parameterize ((cell-access-mode CellAccessMode:Editing))
	      (and-let* ((`(,_ ,expression) source))
		(Quote expression))))
	  (ex java.lang.Throwable
	      (WARN "Unable to create Quote from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))

(set! (extension 'quasiquote)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (otherwise #!null
	    (parameterize ((cell-access-mode CellAccessMode:Editing))
	      (and-let* ((`(,_ ,expression) source))
		(Quasiquote expression))))
	  (ex java.lang.Throwable
	      (WARN "Unable to create Quasiquote from "
		    source": "(java.lang.String:valueOf ex))
	      #!null)))))

(set! (extension 'unquote)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (otherwise #!null
	    (parameterize ((cell-access-mode CellAccessMode:Editing))
	      (and-let* ((`(,_ ,expression) source))
		(Unquote expression))))
	  (ex java.lang.Throwable
	      (WARN "Unable to create Unquote from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))

(set! (extension 'unquote-splicing)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (otherwise #!null
	    (parameterize ((cell-access-mode CellAccessMode:Editing))
	      (and-let* ((`(,_ ,expression) source))
		(UnquoteSplicing expression))))
	  (ex java.lang.Throwable
	      (WARN "Unable to create UnquoteSplicing from "
		    source": "(java.lang.String:valueOf ex))
	      #!null)))))
