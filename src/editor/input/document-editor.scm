(module-name (editor input document-editor))

(import (language define-object))
(import (language attributes))
(import (language define-type))
(import (language define-parameter))

(import (language fundamental))
(import (language while))
(import (language for))
(import (language match))
(import (language infix))
(import (language keyword-arguments))

(import (language parameterize-up))
(import (utils functions))
(import (utils print))
(import (utils hash-table))

(import (editor types primitive))
(import (editor document cursor))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor input transforms))
(import (editor input input))

(import (editor types extensions widgets))
(import (editor types extensions combinators))

(import (editor document history-tracking))
(import (editor types spaces))

(import (editor document documents))
(import (editor types extensions extensions))

(import (editor document parse))
(import (editor document document-operations))
(import (editor input screen))
(import (editor input splits))
(import (editor input pop-ups))

(define-object (Point x::real y::real color::long)::Layer
  (define (render!)
    (painter:draw-point! x y color))
  (IgnoreInput))

(define-object (Stroke finger ::byte source-pane ::Pane)::Layer
  (define points ::($bracket-apply$ List Position) (ArrayList))

  (define (add-point! p::Position)::void
    (points:add p))

  (define (render!)::void
    (for i from 1 below (points:size)
         (let ((p0 ::Position (points (- i 1)))
	       (p1 ::Position (points i)))
           (painter:draw-thick-line! p0:left p0:top p1:left p1:top))))

  (IgnoreInput))

(define-object (Drawing stroke::Stroke)::Drag

  (define (move! x::real y::real dx::real dy::real)::void
    (stroke:add-point! (Position left: x top: y)))

  (define (drop! x::real y::real vx::real vy::real)::void
    (safely
     (escape-with break
       (for recognizer::Recognizer in the-recognizers
	 (call-with-values
	     (lambda ()
	       (recognizer:recognizes stroke:points screen))
	   (lambda result
	     (and-let* ((`(,value . ,rest) result)
			(value))
	       (apply recognizer:action recognizer
		      stroke:points screen result)
	       (break)))))))
    (screen:remove-overlay! stroke))

  (screen:add-overlay! stroke))

(define-object (Selected items::cons position::Position)::Layer

  (define (render!)::void
    (parameterize ((the-document items))
      (with-translation (position:left position:top)
	(draw-sequence! items))))

  (IgnoreInput))

(define-object (DragAround selected::Selected)::Drag

  (define (move! x::real y::real dx::real dy::real)::void
    (let ((position ::Position selected:position))
      (set! position:left (+ position:left dx))
      (set! position:top (+ position:top dy))))

  (define (drop! x::real y::real vx::real vy::real)::void
    ;; musimy sobie przetransformowac wspolrzedne
    ;; do wspolrzednych edytora oraz wybrac dokument
    (screen:remove-overlay! selected)
    (screen:drop-at! x y selected:items)
    )

  (screen:add-overlay! selected))

(define-object (Resize box::cons path::Cursor anchor::real
		       left::real top::real
		       editor::Editor)::Drag

		       (define initial ::Extent (copy (extent+ box)))

		       (define width ::real initial:width)
		       (define height ::real initial:height)

		       (define ending ::LineEnding
			 (line-ending-embracing anchor #;from box))

		       #;(define p ::Point
		       (let-values (((xe ye) (the-transform-stack:inside-out
		       (+ left ending:reach
		       (painter:paren-width))
		       (+ top anchor))))
		       (Point xe ye #xff0000)))

		       (define (move! x::real y::real dx::real dy::real)::void
			 (safely
			  (let*-values (((zx zy) (editor:outside-in 0 0))
					((dx* dy*) (editor:outside-in dx dy)))
			    (set! width (+ width (- dx* zx)))
			    (set! height (+ height (- dy* zy)))
			    (resize! box width height ending))))

		       (define (drop! x::real y::real vx::real vy::real)::void
			 #;(screen:remove-overlay! p)
			 (let ((final ::Extent (extent+ box))
			       (history ::History (history (the-document))))
			   (when (isnt final equal? initial)
			     (history:record! (ResizeBox at: path
							 from: initial
							 to: (copy final)
							 with-anchor: anchor)))))
		       #;(screen:add-overlay! p)
		       )

(define-object (Translate target::Transform)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (target:translate! dx dy))

  (define (drop! x::real y::real vx::real vy::real)::void
    (values)))


(define (file-list directory::java.io.File
                   file-action::(maps (java.io.File) to: void)
		   directory-action::(maps (java.io.File) to: void))
  ::Enchanted
  (let* ((filenames ::(array-of String)
		    (directory:list))
         (n ::int (length filenames))
         (buttons ::(array-of FileButton)
		  ((array-of FileButton)
		   length: (+ n 1))))
    (set! (buttons 0) (ParentDirectoryButton
                       target: (directory:getParentFile)
		       action: directory-action))
    (for i from 0 below n
	 (let ((file (java.io.File directory (filenames i))))
           (set! (buttons (+ i 1))
		 (if (file:isDirectory)
		     (DirectoryButton target: file
				      action: directory-action)
		     (FileButton target: file
				 action: file-action)))))
    (Array:sort buttons)
    (ColumnGrid buttons)))

(define (open-file-browser directory::java.io.File
			   editor::DocumentEditor)
  ::PopUp
  (let ((window ::PopUp #!null))
    (set! window
	  (popup-scroll
	   (file-list directory
		      (lambda (file::java.io.File)
			::void
			(screen:clear-overlay!)
			(editor:load-file file))
		      (lambda (directory::java.io.File)
			::void
			(screen:remove-overlay! window)
			(let ((new-window (open-file-browser
					   directory
					   editor))
			      (position ::Position
					(last-known-pointer-position
					 0)))
			  (new-window:center-around! position:left
						     position:top)
			  (screen:add-overlay! new-window))))))
    window))

(define (save-file-browser directory::java.io.File name-hint::string editor::DocumentEditor)::PopUp 
  
  (let* ((window ::PopUp #!null)
         (text-field ::Scroll (text-field 0 name-hint))
         (button (Button label: "Save"
			 action: (lambda _
				   (screen:clear-overlay!)
				   (save-document!
				    editor:document
				    (java.io.File
				     directory
				     text-field:content)))))
	 (files (file-list
		 directory
		 (lambda (file::java.io.File)::void
			 (set! text-field:content
			       (text-input
				(file:getName))))
		 (lambda (dir::java.io.File)::void
			 (screen:remove-overlay!
			  window)
			 (let ((new-window (save-file-browser
					    dir
					    text-field:content
					    editor))
			       (position ::Position
					 (last-known-pointer-position
					  0)))
			   (new-window:center-around! position:left
						      position:top)
			   (screen:add-overlay! new-window)))))
	 (inner ::Extent (extent+ files))
	 (browser ::Scroll (Scroll content: files
				   width: inner:width
				   height: inner:height))
	 (top (Beside left: text-field right: button))
	 (upper ::Extent (extent+ top))
	 (content (Below top: top
                         bottom: browser))
         (popup (PopUp content: content))
	 (outer ::Extent (extent+ popup))
	 (available ::Extent (screen:extent))
	 (button-size ::Extent (extent+ button))
	 (min-width ::real (+ (* 7 (painter:space-width))
			      button-size:width)))
    (set! browser:width (max min-width
			     (- browser:width
				(max 0 (- outer:width
					  available:width)))))
    (set! browser:height (- browser:height
                            (max 0 (- outer:height
				      (- upper:height)
				      available:height))))
    (set! text-field:width (- browser:width
			      button-size:width))
    (set! window popup)
    (and-let* ((`(,tip . ,root) (screen:overlay-cursor popup)))
      (screen:set-overlay-cursor!
       popup (recons (text-field:content:last-index) root)))
    window))

(define (builtin-open-file finger::byte editor::Editor)
  (lambda _
    (let ((keeper ::Keeper (the-keeper)))
      (keeper:with-read-permission
       (lambda _
	 (let ((window ::PopUp (open-file-browser
				(keeper:initial-directory)
				editor))
	       (position ::Position
			 (last-known-pointer-position
			  finger)))
	   (window:center-around! position:left position:top)
	   (screen:add-overlay! window)))))))

(define-parameter (open-file)::(maps (byte java.io.File Editor)
				     to: (maps _ to: void))
  builtin-open-file)

(define (builtin-save-file finger::byte editor::Editor)
  (lambda _
    (let ((keeper ::Keeper
		  (the-keeper)))
      (keeper:with-write-permission
       (lambda _
	 (safely
	  (let ((window ::PopUp (save-file-browser
				 (keeper:initial-directory)
				 "filename.scm"
				 editor))
		(position ::Position
			  (last-known-pointer-position
			   finger)))
	    (window:center-around! position:left position:top)
	    (screen:add-overlay! window))))))
    #t))

(define-parameter (save-file)::(maps (byte java.io.File Editor)
				     to: (maps _ to: void))
  builtin-save-file)

(define (document-switcher editor::Editor)::PopUp
  (let* ((choices (map (lambda (document::Document)
			 (Link content:
			       (Caption
				(if document:source
				    (document:source:getName)
				    "(unnamed)"))
			       on-tap:
			       (lambda _
				 (screen:clear-overlay!)
				 (editor:switch-to!
				  document)
				 #t)))
		       open-documents)))
    (popup-scroll (ColumnGrid choices))))

(define (open-search-window)
  (screen:add-overlay!
   (PopUp
    name: "search"
    content:
    (beside
     (text-field (* (painter:space-width) 20) "")
     (below
      (Button label: "⬑"
	      action: (lambda _ (WARN "previous")))
      (Button label: "⬎"
	      action: (lambda _ (WARN "next"))))))))

(define-object (CursorMarker)::WithCursor
  (define marked ::Position
    (Position left: 0
	      top: 0))

  (define distance-to-previous-line ::real 0)

  (define distance-to-next-line ::real 0)

  (define (to-next-line)::real
    distance-to-next-line)

  (define (to-previous-line)::real
    distance-to-previous-line)

  (define (mark-cursor! left::real top::real)::void
    (let* ((original-traversal ::Traversal (the-traversal)))
      (set! marked:left left)
      (set! marked:top top)
      (set! distance-to-previous-line
	    (min (painter:min-line-height)
		 (original-traversal:preceding-line-height)))

      (set! original-traversal:on-end-line
	    (lambda (continued?::boolean)
	      (let* ((traversal ::Traversal (the-traversal))
		     (parent-top ::real (+ traversal:top
					   traversal:parent-top))
		     (distance ::real (- traversal:max-line-height
					 (- top parent-top))))
		(cond
		 (continued?
		  (set! distance-to-next-line distance)
		  (set! traversal:on-end-line nothing))
		 ((isnt traversal:parent eq? #!null)
		  (set! traversal:parent:on-end-line
			traversal:on-end-line)
		  (set! traversal:on-end-line nothing))
		 (else
		  (WARN "to the top!"))))))))

  (define (marked-cursor-position)::Position
    marked)

  (define column ::real 0)

  (define (set-cursor-column! left::real)::void
    (set! column left))

  (define (cursor-column)::real
    column)
  )

(define-type (DocumentEditingContext
	      cursor: Cursor := '(#\[ 1)
	      selection-range: int := 0
	      transform: Transform := ((default-transform))))

(define-object (DocumentEditor)::Editor

  (define (pane-under x::real y::real)::Embeddable
    (this))

  (define post-draw-actions ::java.util.List
    (java.util.ArrayList))

  (define (add-post-draw-action! action::(maps () to: void))::void
    (post-draw-actions:add (post-draw-actions:size) action))

  (define document ::Document (Document (empty) #!null))
  (define cursor ::Cursor '(#\[ 1))

  (define transform ::Transform ((default-transform)))

  (define selection-range ::integer 0)

  (define (drop-at! x::real y::real object::pair)::boolean
    (and-let* ((items ::cons object)
	       (xd yd (transform:outside-in x y))
	       (cursor (document:cursor-under* xd yd '()))
	       (`(,tip . ,precursor) cursor)
	       (parent ::Element (cursor-ref document precursor))
	       (location ::Element (parent:part-at tip)))
      (parameterize/update-sources ((the-document document)
				    (the-cursor cursor))
	(cond
	 ((isnt parent eq? location)
	  (WARN "reached "location" in "parent" at "cursor))

	 ((is parent Space?)
	  (let* ((action ::Insert (Insert element: items
					  at: cursor))
		 (history ::History (history document)))
	    (history:record! action)
	    (set! (the-cursor) (action:apply! document))))

	 ((is parent cons?)
	  (cond
	   ((eqv? tip (parent:first-index))
	    (insert! items
		     at: (recons (parent:next-index tip)
				 cursor)))
	   ((eqv? tip (parent:last-index))
	    (insert! items
		     at: (recons
			  (parent:previous-index tip)
			  cursor)))
	   (else
	    (WARN "unhandled "tip" in "parent))))))))

  (define (outside-in x::real y::real)::(Values real real)
    (transform:outside-in x y))

  (define previously-edited
    (attribute (document::Document)
	       ::Document
	       (or (and-let* ((`(,_ ,next . ,_)
			       (first-cell (is (car _) eq? document)
					   open-documents )))
		     next)
		   (and-let* ((`(,first . ,_) open-documents)
			      ((isnt first eq? document)))
		     first)
		   document)))

  (define editing-context
    (attribute+ (document::Document)
		::DocumentEditingContext
		(DocumentEditingContext)))

  (define (clone)
    (let* ((new-editing-context (copy editing-context))
	   (new-context ::DocumentEditingContext
			(new-editing-context document)))
      (set! new-context:cursor cursor)
      (set! new-context:transform (copy transform))
      (set! new-context:selection-range selection-range)
      (DocumentEditor document: document
		      cursor: new-context:cursor
		      selection-range: new-context:selection-range
		      previously-edited: (copy previously-edited)
		      transform: new-context:transform
		      editing-context: new-editing-context)))

  (define (switch-to! target::Document)::void
    (unless (eq? target document)
      (set! (previously-edited target) document)
      (let ((previous-context ::DocumentEditingContext
			      (editing-context document)))
	(set! previous-context:transform transform)
	(set! previous-context:cursor cursor)
	(set! previous-context:selection-range selection-range))
      (set! document target)
      (let ((next-context ::DocumentEditingContext
			  (editing-context target)))
	(set! transform next-context:transform)
	(set! cursor next-context:cursor)
	(set! selection-range next-context:selection-range))
      ))

  (define (load-file file::java.io.File)::void
    (safely
     (switch-to! (open-document-file file))))

  (define (load-from-port port::gnu.kawa.io.InPort source)::void
    (safely
     (switch-to! (load-document-from-port port source))))

  (define (draw-debug-cursor-points!)
    (safely
     (and-let* ((position ::Position (invoke (this)
					     'marked-cursor-position))
		(left ::real position:left)
		(top ::real position:top)
		(column ::real (invoke-special CursorMarker (this)
					       'cursor-column))
		(previous ::real (invoke-special CursorMarker (this)
						 'to-previous-line))
		(current ::real (invoke-special CursorMarker (this)
						'to-next-line)))
       #;(painter:draw-point! left top #x000000)
       #;(painter:draw-point! column top #xff0000)
       (painter:draw-point! column (+ top current) #x00ff00)
       #;(painter:draw-point! column (- top previous) #x0000ff))))

  (define (render!)::void
    (parameterize ((the-document document)
		   (the-cursor cursor)
		   (the-editor (this))
		   (the-selection-range selection-range))
      (with-post-transform transform
	(with-view-edges-transformed transform
	  (transform:within painter
			    (lambda ()
			      (document:draw! '())
			      (for action::procedure in post-draw-actions
				(action))
			      (post-draw-actions:clear)
			      (draw-debug-cursor-points!)
			      ))))))

  (define (tap! finger::byte #;at xe::real ye::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
					; trzeba dojsc dlaczego to nie dziala
				      #;(the-cursor cursor)
				      (the-editor (this))
				      #;(the-selection-range
				      selection-range))
	  (and-let* ((x y (transform:outside-in xe ye))
		     (target-cursor (cursor-under x y))
		     (target (the-expression
			      at: target-cursor))
		     (editor ::Editor (this))
		     (x0 y0 (document-position-of-element-pointed-by
			     target-cursor (car document)))
		     (x* y* (transform:inside-out x0 y0)))
	    (match target
	      (enchanted::Interactive
	       (enchanted:tap! finger x y))
	      (else
	       (set! cursor target-cursor)
	       (set! selection-range 0)
	       (editor:set-cursor-column! xe)
	       #t)))))))

  (define (press! finger::byte #;at xe::real ye::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let-values (((selection-start selection-end)
			(the-selection))
		       ((x y) (transform:outside-in xe ye)))
	    (and-let* ((path (cursor-under x y))
		       (xd yd (document-position-of-element-pointed-by
			       path (car document)))
		       (`(,tip . ,subpath) path)
		       (parent ::Element (the-expression
					  at: subpath))
		       (target ::Element (parent:part-at tip)))
	      (cond
	       #;((isnt parent eq? target)
	       (WARN "reached non-final item on press"))

	       #;((isnt dragging clean?)
	       (WARN "should start scrolling or zooming "
	       (keys dragging)))

	       ((screen:has-layer
		 (lambda (layer::Layer)
		   (and-let* (((Stroke source-pane: ,(this))
			       layer))
		     layer)))
		=> (lambda (stroke::Stroke)
		     (screen:remove-overlay! stroke)
		     (screen:undrag! stroke:finger)
		     (let ((p0 ::Position (Position left: xe top: ye))
			   (p1 ::Position (stroke:points
					   (- (length stroke:points)
					      1)))
			   (editor ::DocumentEditor (this)))
		       (screen:drag!
			stroke:finger
			(object (Drag)
			  ((move! x::real y::real
				  dx::real dy::real)
			   ::void
			   (let ((p1x ::real (+ p1:left dx))
				 (p1y ::real (+ p1:top dy)))
			     (editor:transform:stretch!
			      p0:left p0:top p1:left p1:top
			      p0:left p0:top p1x  p1y)
			     (set! p1:left p1x)
			     (set! p1:top p1y)))
			  ((drop! x::real y::real
				  dx::real dy::real)
			   ::void
			   (values))))

		       (screen:drag!
			finger
			(object (Drag)
			  ((move! x::real y::real
				  dx::real dy::real)
			   ::void
			   (let ((p0x ::real (+ p0:left dx))
				 (p0y ::real (+ p0:top dy)))
			     (editor:transform:stretch!
			      p0:left p0:top p1:left p1:top
			      p0x  p0y  p1:left p1:top)
			     (set! p0:left p0x)
			     (set! p0:top p0y)))
			  ((drop! x::real y::real
				  dx::real dy::real)
			   ::void
			   (values)))))))

	       ((Enchanted? target)
		(let ((target ::Enchanted target))
		  (target:press! finger (- x xd) (- y yd))))
	       
	       ((is target Space?)
		(screen:drag! finger
			      (Drawing (Stroke finger (this)))))

	       ((is selection-start cursor< path
		    cursor< selection-end)
		(WARN "should move selection"))

	       ((or (is target Atom?)
		    (and (or (is target cons?)
			     (is target EmptyListProxy?))
			 (eqv? tip (target:first-index))))
		;; powinnismy powiekszyc spacje poprzedzajaca
		;; wydobywany element o szerokosc tego elementu
		;; podzielona przez (painter:space-width)
		(set! (the-cursor) (cursor-climb-back
				    (cursor-retreat (tail path))))
		(let* ((removed ::Remove (remove-element!
					  at: subpath))
		       (selection (Selected
				   removed:element
				   (copy
				    (last-known-pointer-position
				     finger)))))
		  (screen:drag! finger (DragAround selection))))

	       ((and (is target cons?)
		     (eqv? tip (target:last-index)))
		(let ((extent ::Extent (extent+ target)))
		  (screen:drag! finger
				(Resize target subpath
					(- y yd
					   (painter:min-line-height))
					xd yd (this)))))
	       (else
		(screen:drag! finger
			      (Drawing (Stroke finger (this))))
		;;(set! (the-cursor) path)
		)))
	    #t)))))

  (define (second-press! finger::byte #;at xe::real ye::real)
    ::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let-values (((selection-start selection-end)
			(the-selection))
		       ((x y) (transform:outside-in xe ye)))
	    (and-let* ((path (cursor-under x y))
		       (xd yd (document-position-of-element-pointed-by
			       path (car document)))
		       (`(,tip . ,subpath) path)
		       (parent ::Element (the-expression
					  at: subpath))
		       (target ::Element (parent:part-at tip)))
	      (cond
	       ((or (isnt parent eq? target)
		    (is target Space?))
		(screen:drag! finger
			      (Translate transform)))

	       #;((isnt dragging clean?)
	       (WARN "should start scrolling or zooming "
	       (keys dragging)))

	       ((Enchanted? target)
		(let ((target ::Enchanted target))
		  (target:second-press! finger (- x xd) (- y yd))))

	       ((or (is target Atom?)
		    (and (or (is target cons?)
			     (is target EmptyListProxy?))
			 (eqv? tip (target:first-index))))
		(let* ((selection (Selected
				   (cons (copy target) (empty))
				   (copy
				    (last-known-pointer-position
				     finger)))))
		  (screen:drag! finger (DragAround selection))))))
	    #t)))))

  (define (double-tap! finger::byte xe::real ye::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (and-let* ((x y (transform:outside-in xe ye))
		     (path (cursor-under x y))
		     (`(,tip . ,subpath) path)
		     (parent ::Element (the-expression
					at: subpath))
		     (target ::Element (parent:part-at tip)))
	    (DUMP parent)
	    (cond
	     ((Maximizable? parent)
	      (screen:maximize! parent))
	     
	     ((isnt (transform:get-angle) = 0.0)
	      (painter:play!
	       (Transition of: transform
			   from: (copy transform)
			   to: (let ((target ::Transform (copy transform)))
				 (target:set-angle! 0.0)
				 target)
			   around: (Position left: xe top: ye)
			   duration/ms: 500))
	      #t)
	     ((or (isnt (transform:get-left) = 0)
		  (is (transform:get-top) > 0))
	      (painter:play!
	       (Transition of: transform
			   from: (copy transform)
			   to: (let ((target ::Transform (copy transform))
				     (document ::Extent (extent+ document)))
				 (target:set-left! 0.0)
				 (target:set-top! 0.0)
				 (target:set-scale!
				  (min (/ (screen:width) document:width)
				       (/ (screen:height) document:height)))
				 target)
			   duration/ms: 500))
	      #t)
	     (else
	      #f)))))))

  (define (long-press! finger::byte x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (safely
	   (invoke (current-message-handler) 'clear-messages!)
	   (let* ((content
		   ::Enchanted
		   (ColumnGrid
		    `(,(Link content: (Caption "New")
			     on-tap: (lambda _ (WARN "New") #t))
		      ,(Link content: (Caption "Open...")
			     on-tap: ((open-file) finger (this)))
		      ,@(if (is (length open-documents) < 1)
			    '()
			    `(,(Link
				content:
				(Caption "Switch to...")
				on-tap:
				(lambda _
				  (safely
				   (screen:add-overlay!
				    (document-switcher (this))))
				  #t))))
		      ,(Link content: (Caption "Save as...")
			     on-tap: ((save-file) finger (this)))
		      ,(Link content: (Caption "Close")
			     on-tap:
			     (lambda _
			       (if (document-saved? document)
				   (close-document! document)
				   ;; tutaj powinnismy wyswietlic
				   (screen:add-overlay!
				    (PopUp
				     content:
				     (below
				      (Caption
				       (string-append
					"Do you want to save "
					(as-string document:source)
					"?"))
				      (beside
				       (Button
					label: "yes"
					action:
					(lambda _
					  (and-let* ((f::java.io.File
						      document:source))
					    (save-document!
					     document f))
					  (close-document! document)))
				       (Button
					label: "no"
					action:
					(lambda _
					  (close-document!
					   document))))))))
			       #t))
		      )))
		  (window ::PopUp (PopUp content: content)))
	     (window:center-around! x y)
	     (screen:add-overlay! window))))
	;; dodanie menu kontekstowego
	#t)))

  (define (scroll-up! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let* ((h ::real (painter:min-line-height)))
	    (transform:translate! 0 h)
	    #t)))))

  (define (scroll-down! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let* ((h ::real (painter:min-line-height)))
	    (transform:translate! 0 (- h))
	    #t)))))

  (define (scroll-left! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let* ((w ::real (painter:space-width)))
	    (transform:translate! w 0)
	    #t)))))

  (define (scroll-right! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (let* ((w ::real (painter:space-width)))
	    (transform:translate! (- w) 0))
	  #t))))

  (define (zoom-in! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (transform:scale! 1.25 0 0)
	  #t))))

  (define (zoom-out! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (transform:scale! 0.8 0 0)
	  #t))))

  (define (rotate-left! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (transform:rotate! -5.0 x y)
	  #t))))

  (define (rotate-right! x::real y::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  (transform:rotate! 5.0 x y)
	  #t))))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources ((the-document document)
				      (the-cursor cursor)
				      (the-editor (this))
				      (the-selection-range
				       selection-range))
	  ((hash-ref keymap key-code
		     (lambda () insert-character-input!)))
	  #t))))

  (define (can-split-beside? line::Area)::boolean
    (let* ((vicinity ::real
		     (painter:line-simplification-resolution))
	   (3vicinity (* vicinity 3)))
      (and (is 0 < line:left <= line:right < (the-pane-width))
	   (is line:top <= 3vicinity)
	   (is (- (the-pane-height) line:bottom) <= 3vicinity))))

  (define (split-beside! line::Area)::Embeddable
    (if (can-split-beside? line)
	(let*-values (((ratio::real) (/ (/ (+ line:left line:right)
					   2)
					(the-pane-width)))
		      ((new::DocumentEditor) (copy (this)))
		      ((split::Split) (SplitBeside first: (this)
						   last: new
						   at: ratio))
		      ((left line right) (split:part-sizes))
		      ((shift) (+ left line)))
	  (new:transform:translate! (- shift) 0)
	  split)
	(this)))

  (define (can-split-below? line::Area)::boolean
    (let* ((vicinity ::real
		     (painter:line-simplification-resolution))
	   (3vicinity (* vicinity 3)))
      (and (is 0 < line:top <= line:bottom < (the-pane-height))
	   (is line:left <= 3vicinity)
	   (is (- (the-pane-width) line:right) <= 3vicinity))))

  (define (split-below! line::Area)::Embeddable
    (if (can-split-below? line)
	(let*-values (((ratio::real) (/ (/ (+ line:top line:bottom)
					   2)
					(the-pane-height)))
		      ((new::DocumentEditor) (copy (this)))
		      ((split::Split) (SplitBelow first: (this)
						  last: new
						  at: ratio))
		      ((top line bottom) (split:part-sizes))
		      ((shift) (+ top line)))
	  (new:transform:translate! 0 (- shift))
	  split)
	(this)))

  (CursorMarker))

(define (open-beside !file-names::(list-of java.io.File))::Embeddable
  ;;(assert (isnt file-names null?))
  (let ((n (length !file-names)))
    (cond
     ((= n 1)
      (DocumentEditor
       document:
       (open-document-file (car !file-names))))
     ((even? n)
      (let ((other-file-names (split! !file-names at: (/ n 2))))
	(SplitBeside first: (open-beside !file-names)
		     last: (open-beside other-file-names))))
     ((odd? n)
      (SplitBeside at: (/ 1.0 n)
		   first: (DocumentEditor
			   document:
			   (open-document-file (car !file-names)))
		   last: (open-beside (cdr !file-names)))))))

(define (adjust-view!)
  (and-let* ((editor ::DocumentEditor (the-editor))
	     (cursor ::Position (editor:marked-cursor-position))
	     (cursor-height ::real (painter:cursor-height))
	     (editor-left ::real (the-pane-left))
	     (editor-top ::real (the-pane-top))
	     (editor-width ::real (the-pane-width))
	     (editor-height ::real (the-pane-height))
	     (xe ye (the-transform-stack:inside-out cursor:left cursor:top)))
    (unless (is 0 <= ye < (- editor-height cursor-height))
      (painter:play!
       (Transition of: editor:transform
		   from: (copy editor:transform)
		   to: (let ((target ::Transform
				     (copy editor:transform)))
			 (target:translate! 0 (- (/ (- editor-height
						       cursor-height)
						    2) ye))
			 target)
		   duration/ms: 500)))))

