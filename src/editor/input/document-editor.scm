(module-name (editor input document-editor))

(import (language define-object))
(import (language attributes))
(import (language define-type))
(import (language define-parameter))
(import (language define-syntax-rule))

(import (language fundamental))
(import (language while))
(import (language for))
(import (language match))
(import (language infix))
(import (language keyword-arguments))
(import (language mapping))

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

(import (editor utils search))

(define-object (Point x::real y::real color::long)::Layer
  (define (render!)::void
    (painter:draw-point! x y color))
  (DeadLayer))

(define-object (Stroke finger ::byte source-pane ::Pane)::Layer
  (define points ::($bracket-apply$ List Position) (ArrayList))

  (define (add-point! p::Position)::void
    (points:add p))

  (define (render!)::void
    (for i from 1 below (points:size)
         (let ((p0 ::Position (points (- i 1)))
	       (p1 ::Position (points i)))
           (painter:draw-thick-line! p0:left p0:top
				     p1:left p1:top))))

  (DeadLayer))

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

  (DeadLayer))

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

(define-object (Resize box::Resizable path::Cursor grab::real
		       left::real top::real
		       editor::Editor)
  ::Drag
  (define document (the-document))
  (define initial ::Extent (copy (box:extent)))

  (define width ::real initial:width)
  (define height ::real initial:height)

  (define anchor ::ResizeAnchor
    (box:resize-anchor grab))

  #;(define p ::Point
  (let-values (((xe ye) (the-transform-stack:inside-out
  (+ left anchor:reach
  (painter:paren-width))
  (+ top grab))))
  (Point xe ye #xff0000)))

  (define (move! x::real y::real dx::real dy::real)::void
    (safely
     (let*-values (((zx zy) (editor:outside-in 0 0))
		   ((dx* dy*) (editor:outside-in dx dy)))
       (set! width (+ width (- dx* zx)))
       (set! height (+ height (- dy* zy)))
       (box:set-size! width height anchor))))

  (define (drop! x::real y::real vx::real vy::real)::void
    #;(screen:remove-overlay! p)
    (let ((final ::Extent (box:extent))
	  (history ::History (history document)))
      (unless (equal? initial final)
	
	(history:record! (ResizeBox at: path
				    from: initial
				    to: (copy final)
				    with-anchor: grab)))))
  #;(screen:add-overlay! p)
  
  )

(define-object (Translate target::Transform)::Drag
  (define (move! x::real y::real dx::real dy::real)::void
    (target:translate! dx dy))

  (define (drop! x::real y::real vx::real vy::real)::void
    (values)))

(define-object (ParentDirectory directory::String)
  (define (getName)::String
    "..")
  (java.io.File directory))

(define (file-list files::(sequence-of java.io.File)
		   file-action::(maps (java.io.File) to: void)
		   directory-action::(maps (java.io.File) to: void))
  ::Enchanted
  (let* ((n ::int (length files))
	 (buttons ::(array-of FileButton)
		  ((array-of FileButton) length: n))
	 (i ::int 0))
    (for file::java.io.File in files
      (set! (buttons i)
	    (cond
	     ((is file ParentDirectory?)
	      (ParentDirectoryButton
	       target: file
	       action: directory-action))
	     ((file:isDirectory)
	      (DirectoryButton target: file
			       action: directory-action))
	     (else
	      (FileButton target: file
			  action: file-action))))
      (set! i (+ i 1)))
    (Array:sort buttons)
    (ColumnGrid buttons)))

(define (open-root-selector action ::(maps (java.io.File)
					   to: void)
			    editor ::Editor)
  ::PopUp
  (let* ((window ::PopUp #!null)
	 (keeper ::Keeper (the-keeper))
	 (roots ::(list-of java.io.File)
		(keeper:file-system-roots)))
    (set! window
      (popup-scroll
       (file-list
	roots
	action
	action)))
    window))

  
(define (content-with-parent* directory::java.io.File)
  ::(sequence-of java.io.File)
  (safely
   (let ((files (only (lambda (f::java.io.File)
			(and (f:canRead)
			     (or (not (f:isDirectory))
				 (f:canExecute))))
		      (directory:listFiles)))
	 (parent (ParentDirectory (directory:getParent))))
     (if (and (parent:canRead)
	      (parent:canExecute))
	 `(,parent . ,files)
	 files))))

(define (replace-window! window ::PopUp
			 #;with new-window ::PopUp)
  ::PopUp
  (screen:remove-overlay! window)
  (let ((position ::Position
		  (last-known-pointer-position
		   0)))
    (new-window:center-around! position:left
			       position:top)
    (screen:add-overlay! new-window)
    new-window))

(define (open-file-browser directory::java.io.File
			   editor::DocumentEditor)
  ::PopUp
  (let* ((window ::PopUp #!null)
	 (keeper ::Keeper (the-keeper))
	 (dir-name (directory:getAbsolutePath))
	 (roots ::(list-of java.io.File)
		(keeper:file-system-roots)))
    (set! window
      (popup-scroll
       (below
	(DirectoryButton
	 target:
	 (element-maximizing
	  (lambda (root::java.io.File)
	    (string-prefix-length
	     (root:getAbsolutePath)
	     dir-name))
	  roots)
	 action:
	 (lambda _
	   (let ((selector #!null))
	     (set! selector
	       (open-root-selector
		(lambda (directory::java.io.File)
		  ::void
		  (replace-window! selector #;with
				   (open-file-browser
				    directory
				    editor)))
		editor))
	   (replace-window! window selector))))
	(file-list
	 (content-with-parent* directory)
	 (lambda (file::java.io.File)
	   ::void
	   (screen:clear-overlay!)
	   (editor:load-file file))
	 (lambda (directory::java.io.File)
	   ::void
	   (replace-window! window #;with
			    (open-file-browser
			     directory
			     editor)))))))
    window))

(define (save-file-browser directory::java.io.File
			   name-hint::string
			   editor::DocumentEditor)
  ::PopUp
  (let* ((window ::PopUp #!null)
	 (keeper ::Keeper (the-keeper))
	 (roots ::(list-of java.io.File)
		(keeper:file-system-roots))
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
		 (content-with-parent* directory)
		 (lambda (file::java.io.File)
		   ::void
		   (set! text-field:content
		     (text-input
		      (file:getName))))
		 (lambda (dir::java.io.File)
		   ::void
		   (replace-window!
		    window
		    #;with
		    (save-file-browser
		     dir
		     text-field:content
		     editor)))))
	 (inner ::Extent (extent+ files))
	 (browser ::Scroll (Scroll content: files
				   width: inner:width
				   height: inner:height))
	 (top (Beside left: text-field right: button))
	 (upper ::Extent (extent+ top))
	 (content (below
		   top
		   (DirectoryButton
		    target: directory
		    action:
		    (lambda _
		      (let ((selector ::PopUp #!null))
			(set! selector
			  (open-root-selector
			   (lambda (directory::java.io.File)
			     ::void
			     (replace-window!
			      selector
			      (save-file-browser
			       directory
			       text-field:content
			       editor)))
			   editor))
		      (replace-window! window selector))))
		   browser))
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
	 (let* ((filename (or (and-let*
				  ((document ::Document
					     editor:document)
				   (file ::java.io.File
					 document:source)
				   (parent ::java.io.File
					   (file:getParentFile))
				   ((parent:isDirectory)))
				(parent:getAbsoluteFile))
			      (keeper:initial-directory)))
		(window::PopUp
		 (open-file-browser filename editor))
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
	  (let ((window::PopUp
		 (save-file-browser
		  (or (and-let* ((document ::Document
					   editor:document)
				 (file ::java.io.File
				       document:source)
				 (parent ::java.io.File
					 (file:getParentFile))
				 ((parent:isDirectory)))
			parent)
		      (keeper:initial-directory))
		  (or (and-let* ((document ::Document
					   editor:document)
				 (file ::java.io.File
				       document:source))
			(file:getName))
		      "filename.scm")
		  editor))
		(position ::Position
			  (last-known-pointer-position
			   finger)))
	    (window:center-around! position:left position:top)
	    (screen:add-overlay! window))))))))

(define-parameter (save-file)::(maps (byte java.io.File Editor)
				     to: (maps _ to: void))
  builtin-save-file)

(define (document-switcher editor::Editor)::PopUp
  (let* ((choices (map (lambda (document::Document)
			 (Link content:
			       (Caption
				(cond
				 ((eq? document:source #!null)
				  "(unnamed)")
				 ((java.io.File?
				   document:source)
				  (document:source:getName))
				 (else
				  (document:source:toString))))
			       on-tap:
			       (lambda _
				 (screen:clear-overlay!)
				 (editor:switch-to!
				  document)
				 #t)))
		       open-documents)))
    (popup-scroll (ColumnGrid choices))))

(define (open-search-window)
  (let* ((search-input (text-field
			(* (painter:space-width) 20) ""))
	 (editor (the-editor))
	 (⬑ (Button
	     label: "⬑"
	     action:
	     (lambda _
	       (safely
		(and-let* (((Highlight end: end)
			    (editor:highlight-back!))
			   ((Position left: x top: y)
			    (cursor-position
			     end in: editor:document))
			   ((Extent width: w height: h)
			    (screen-extent editor)))
		  (editor:pan-to!
		   (editor:transform:translating
		    0 y (editor:transform:get-left) (/ h 2))
		   500))))))
	 (⬎ (Button
	     label: "⬎"
	     action:
	     (lambda _
	       (safely		      
		(and-let* (((Highlight start: start)
			    (editor:highlight-next!))
			   ((Position left: x top: y)
			    (cursor-position
			     start in: editor:document))
			   ((Extent width: w height: h)
			    (screen-extent editor)))
		  (editor:pan-to!
		   (editor:transform:translating
		    0 y (editor:transform:get-left) (/ h 2))
		   500))))))
	 (popup (PopUp
		 content:
		 (beside
		  search-input (below ⬑ ⬎))))
	 (hijack
	  (HijackLayerInput popup
	    ((close!)::void
	     (editor:set-highlights! '()))
	    ((key-typed! key-code::long context::Cursor)
	     ::boolean
	     (match (key-code-name key-code)
	       ('escape
		(popup:remove-from-overlay!))
	       (_
		(search-input:key-typed! key-code context)
		(safely
		 (and-let*
		     ((pattern (parse-string
				search-input:content))
		      ((isnt pattern empty?))
		      (editor ::DocumentEditor)
		      (highlights ::(list-of Highlight)
				  (all-matches
				   of: pattern
				   in: editor:document)))
		   (editor:set-highlights! highlights)))
		#t))))))
    (screen:add-overlay! hijack)))

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
	      transform: Transform := ((default-transform))))

(define-syntax-rule (with-editor-context body + ...)
  ;; this should normally be defined inside
  ;; the DocumentEditor, but unfortunately
  ;; our language doesn't support it for now
  ;; (in either case, this is where our
  ;; variables come from)
  (with-post-transform (the transform)
    (with-view-edges-transformed (the transform)
      (parameterize ((the-editor (this))
		     (the-document (the document)))
	(parameterize/update-sources
	    ((the-cursor (the cursor))
	     (the-selection (the selection-highlight))
	     (the-highlights (the highlights)))
	  body + ...)))))

(define-object (DocumentEditor)::Editor

  (define (active)::Embeddable
    (this))
  
  (define (pane-under x::real y::real)::Embeddable
    (this))

  (define post-draw-actions ::java.util.List
    (java.util.ArrayList))

  (define (add-post-draw-action! action::(maps () to: void))
    ::void
    (post-draw-actions:add (post-draw-actions:size) action))

  (define document ::Document (Document (empty) #!null))
  (define cursor ::Cursor '(#\[ 1))

  (define (close-document! target)::void
    (for document::Document in (keys previously-edited)
      (when (eq? (previously-edited document) target)
	(unset! (previously-edited document))))
    (let ((replacement (previously-edited target)))
      (when (eq? replacement target)
	(set! replacement (new-document)))
      (when (eq? target document)
	(set! document replacement)))
    (unset! (previously-edited target)))
  
  (define (update-cursor-column!)::void
    (let ((cursor-position::Position
	   (painter:marked-cursor-position)))
      (add-post-draw-action!
       (lambda ()
	 (invoke-special CursorMarker (this)
			 'set-cursor-column!
			 cursor-position:left)))))

  (define selection-highlight ::Highlight
    (Highlight type: HighlightType:Selection
	       start: cursor
	       end: cursor))

  (define (move-cursor-up!)::void
    (let ((current ::Position (invoke-special
			       CursorMarker (this)
			       'marked-cursor-position)))
      (set! cursor
	    (cursor-under (invoke-special
			   CursorMarker (this)
			   'cursor-column)
			  (- current:top
			     (invoke-special
			      CursorMarker (this)
			      'to-previous-line))))
      (set! selection-highlight:start cursor)
      (set! selection-highlight:end cursor)))

  (define (move-cursor-down!)::void
    (let ((current ::Position (invoke-special
			       CursorMarker (this)
			       'marked-cursor-position)))
      (set! cursor
	    (cursor-under (invoke-special
			   CursorMarker (this)
			   'cursor-column)
			  (+ current:top
			     (invoke-special
			      CursorMarker (this)
			      'to-next-line))))
      (set! selection-highlight:start cursor)
      (set! selection-highlight:end cursor)))
  
  (define (move-cursor-right!)::void
    (set! cursor (cursor-advance cursor document))
    (set! selection-highlight:start cursor)
    (set! selection-highlight:end cursor)
    (update-cursor-column!))

  (define (move-cursor-left!)::void
    (set! cursor (cursor-retreat cursor document))
    (set! selection-highlight:start cursor)
    (set! selection-highlight:end cursor)
    (update-cursor-column!))

  (define (unnest-cursor-right!)::void
    (and-let* ((`(,tip ,top . ,root) cursor)
	       (parent ::Indexable (cursor-ref document root))
	       (target ::Indexable (parent:part-at top))
	       (item ::Indexable (target:part-at tip)))
      ;;(assert (eq? target item))
      (set! cursor
	    (cond
	     ((Textual? item)
	      (recons (parent:last-index) root))
	     ((eqv? tip (parent:last-index))
	      (recons (parent:last-index) root))
	     (else
	      (recons* (parent:last-index) top root))))
      (set! selection-highlight:start cursor)
      (set! selection-highlight:end cursor)
      (update-cursor-column!)))

  (define (expand-selection-right!)::void
    (let ((new-cursor (cursor-advance cursor document)))
      (if (equal? cursor selection-highlight:end)
	  (set! selection-highlight:end new-cursor)
	  (set! selection-highlight:start new-cursor))
      (set! cursor new-cursor))
    (DUMP selection-highlight)
    (update-cursor-column!))

  (define (expand-selection-left!)::void
    (let ((new-cursor (cursor-retreat cursor document)))
      (if (equal? cursor selection-highlight:start)
	  (set! selection-highlight:start new-cursor)
	  (set! selection-highlight:end new-cursor))
      (set! cursor new-cursor))
    (DUMP selection-highlight)
    (update-cursor-column!))
  
  (define transform ::Transform ((default-transform)))

  (define (pan-to! target::Transform duration/ms::real)
    ::void
    (painter:play!
     (Transition of: transform
		 from: (copy transform)
		 to: target
		 duration/ms: duration/ms)))
  
  (define highlights ::(list-of Highlight)
    `(,selection-highlight))

  (define (set-highlights! hs ::(list-of Highlight))::void
    (set! highlights
	  (insert-ordered! selection-highlight hs
			   ;;(is _:start document:cursor< _:start)
			   (lambda (a::Highlight b::Highlight)
			     ::boolean
			     (cursor< a:start b:start document))))
    (and-let* ((current ::Highlight
			(or (find (lambda (highlight::Highlight)
				    (and
				     (eq? highlight:type
					  HighlightType:OtherFinding)
				     (cursor<= cursor
					       highlight:start
					       document)))
				  highlights)
			    (find (is _:type eq?
				      HighlightType:OtherFinding)
				  highlights))))
      (set! current:type
	    HighlightType:CurrentFinding)))

  (define (highlight-next!)::(maybe Highlight)
    (otherwise #!null
      (and-let* ((`(,current::Highlight . ,rest)
		  (first-cell
		   (lambda (cell::pair)
		     ::boolean
		     (and-let*
			 ((`(,(Highlight
			       type:
			       HighlightType:CurrentFinding)
			     . ,_) cell))))
		   highlights)))
	(set! current:type HighlightType:OtherFinding)
	(and-let* ((next (or
			  (find (is _:type eq?
				    HighlightType:OtherFinding)
				rest)
			  (find (is _:type eq?
				    HighlightType:OtherFinding)
				highlights))))
	  ;; tutaj jeszcze powinnismy wycentrowac widok
	  (set! next:type HighlightType:CurrentFinding)
	  next))))

  (define findings-highlights ::EnumSet
    (EnumSet:of HighlightType:CurrentFinding
		HighlightType:OtherFinding))

  (define (highlight-back!)::(maybe Highlight)
    (or
     (and-let* ((`(,previous::Highlight . ,rest)
		 (first-cell
		  (lambda (cell)
		    (and-let*
			((`(,(Highlight
			      type:
			      HighlightType:OtherFinding)
			    . ,rest) cell)
			 (`(,(Highlight
			      type:
			      HighlightType:CurrentFinding)
			    . ,_) (drop-while
				   (isnt _:type in
					 findings-highlights)
				   rest)))))
		  highlights))
		(current::Highlight
	   (find (is _:type eq?
		     HighlightType:CurrentFinding)
		 rest)))
       (set! previous:type HighlightType:CurrentFinding)
       (set! current:type HighlightType:OtherFinding)
       	;; tutaj jeszcze powinnismy wycentrowac widok
       previous)
     (and-let* ((`(,current::Highlight . ,rest)
		 (first-cell
		  (lambda (cell)
		    (and-let*
			((`(,(Highlight
			      type:
			      HighlightType:CurrentFinding)
			    . ,_) cell))))
		  highlights))
		(`(,last::Highlight . ,_)
		 (last-cell
		  (lambda (cell)
		    (and-let*
			((`(,(Highlight
			      type:
			      HighlightType:OtherFinding)
			    . ,_) cell))))
		  rest)))
       (set! current:type HighlightType:OtherFinding)
       (set! last:type HighlightType:CurrentFinding)
       	;; tutaj jeszcze powinnismy wycentrowac widok
       last)
     #!null))

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
      (DocumentEditor document: document
		      cursor: new-context:cursor
		      previously-edited: (copy
					  previously-edited)
		      transform: new-context:transform
		      editing-context: new-editing-context)))

  (define (switch-to! target::Document)::void
    (unless (eq? target document)
      (set! (previously-edited target) document)
      (let ((previous-context ::DocumentEditingContext
			      (editing-context document)))
	(set! previous-context:transform transform)
	(set! previous-context:cursor cursor))
      (set! document target)
      (let ((next-context ::DocumentEditingContext
			  (editing-context target)))
	(set! transform next-context:transform)
	(set! cursor next-context:cursor))
      ))

  (define (load-file file::java.io.File)::void
    (safely
     (switch-to! (open-document-file file))))

  (define (new-file)::void
    (safely
     (switch-to! (new-document))))
  
  (define (load-from-port port::gnu.kawa.io.InPort source)
    ::void
    (safely
     (switch-to! (load-document-from-port port source))))

  (define (draw-debug-cursor-points!)
    (safely
     (and-let*
	 ((position ::Position (invoke (this)
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
		   (the-selection (the selection-highlight))
		   (the-highlights (the highlights))
                   (the-editor (this)))
      (with-post-transform transform
        (with-view-edges-transformed transform
	  (transform:within
	   painter
	   (lambda ()
	     (let ((position ::Position (screen-position
					 (this)))
		   (doc ::Extent (extent+ document))
		   (margin (- (painter:space-width)))
		   (extent ::Extent (screen-extent (this))))
	       (set! position:left (the-pane-left))
	       (set! position:top (the-pane-top))
	       (set! extent:width (the-pane-width))
	       (set! extent:height (the-pane-height))
	       (with-translation (margin margin)
		 (painter:fill-background! doc:width
					   doc:height)))
	     (document:draw! '())
	     (for action::procedure in post-draw-actions
	       (action))
	     (post-draw-actions:clear)
	     (draw-debug-cursor-points!)
	     ))))))

  (define (tap! finger::byte #;at xe::real ye::real)::boolean
    (with-post-transform transform
      (with-view-edges-transformed transform
	(parameterize/update-sources
	    ((the-document document)
	     ;; trzeba dojsc dlaczego to nie dziala
	     #;(the-cursor cursor)
	     (the-editor (this)))
	  (and-let* ((x y (transform:outside-in xe ye))
		     (target-cursor (cursor-under x y))
		     (target (cursor-ref document target-cursor))
		     (editor ::Editor (this))
		     (x0 y0 (document-position-of-element-pointed-by
			     target-cursor (car document)))
		     (x* y* (transform:inside-out x0 y0)))
	    (match target
	      (enchanted::Interactive
	       (enchanted:tap! finger x y))
	      (else
	       (set! cursor target-cursor)
	       (set! selection-highlight:start cursor)
	       (set! selection-highlight:end cursor)
	       (editor:set-cursor-column! xe)
	       #t)))))))

  (define (press! finger::byte #;at xe::real ye::real)::boolean
    (with-editor-context
     (and-let* (((Highlight start: selection-start
			    end: selection-end) (the-selection))
		(x y (transform:outside-in xe ye))
		(path (cursor-under x y))
		(xd yd (document-position-of-element-pointed-by
			path (car document)))
		(`(,tip . ,subpath) path)
		(parent ::Element (cursor-ref document subpath))
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

	  ((and-let* ((it ::Resizable target)
		      ((it:can-be-resized?)))
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
	   ))
	 #t)))

  (define (second-press! finger::byte #;at xe::real ye::real)
    ::boolean
    (with-editor-context
     (and-let* (((Highlight start: selection-start
			    end: selection-end) (the-selection))
		(x y (transform:outside-in xe ye))
		(path (cursor-under x y))
		(xd yd (document-position-of-element-pointed-by
			path (car document)))
		(`(,tip . ,subpath) path)
		(parent ::Element (cursor-ref document subpath))
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
     #t))

  (define (double-tap! finger::byte xe::real ye::real)
    ::boolean
    (with-editor-context
     (and-let* ((x y (transform:outside-in xe ye))
		(path (cursor-under x y))
		(`(,tip . ,subpath) path)
		(parent ::Element (cursor-ref document subpath))
		(target ::Element (parent:part-at tip)))
       (cond
	((Maximizable? parent)
	 (screen:maximize! parent))
	
	((isnt (transform:get-angle) = 0.0)
	 (painter:play!
	  (Transition of: transform
		      from: (copy transform)
		      to: (let ((target ::Transform
					(copy transform)))
			    (target:set-angle! 0.0)
			    target)
		      around: (Position left: xe top: ye)
		      duration/ms: 500)))
	((or (isnt (transform:get-left)
		   = (painter:space-width))
	     (is (transform:get-top) > 0))
	 (painter:play!
	  (Transition of: transform
		      from: (copy transform)
		      to: (let ((target ::Transform
					(copy transform))
				(document ::Extent
					  (extent+
					   document)))
			    (target:set-left!
			     (painter:space-width))
			    (target:set-scale!
			     (/ (screen:width)
				document:width))
			    (cond
			     ((is (* (target:get-scale)
				     (target:get-top))
				  < (- (screen:height)
				       document:height))
			      (target:set-top! (- (screen:height)
						  document:height)))
			     ((is (target:get-top) > 0)
			      (target:set-top! 0.0)))
			    target)
		      duration/ms: 500))))
       #t)))

  (define (long-press! finger::byte x::real y::real)::boolean
    (with-editor-context
     (safely
      (invoke (current-message-handler) 'clear-messages!)
      (let* ((editor ::DocumentEditor (this))
	     (content
	      ::Enchanted
	      (ColumnGrid
	       `(,(Link
		   content: (Caption "New")
		   on-tap: (lambda _
			     ::void
			     (screen:clear-overlay!)
			     (editor:new-file)))
		 ,(Link
		   content: (Caption "Open...")
		   on-tap: ((open-file) finger editor))
		 ,@(if (is (length open-documents) < 1)
		       '()
		       `(,(Link
			   content:
			   (Caption "Switch to...")
			   on-tap:
			   (lambda _
			     (safely
			      (screen:add-overlay!
			       (document-switcher editor)))
			     #t))))
		 ,(Link
		   content: (Caption "Save as...")
		   on-tap: ((save-file) finger editor))
		 ,(Link
		   content: (Caption "Close")
		   on-tap:
		   (lambda _
		     (safely
		      (if (document-saved? document)
			  (shut-document! document)
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
				 (and-let*
				     ((f::java.io.File
				       document:source))
				   (save-document!
				    document f))
				 (shut-document!
				  document)))
			      (Button
			       label: "no"
			       action:
			       (lambda _
				 (shut-document!
				  document)))))))))
		     #t))
		 )))
	     (window ::PopUp (PopUp content: content)))
	(window:center-around! x y)
	(screen:add-overlay! window))))
    ;; dodanie menu kontekstowego
    #t)

  (define (scroll-up! x::real y::real)::boolean
    (with-editor-context
     (let* ((h ::real (painter:min-line-height)))
       (transform:translate! 0 h)
       #t)))

  (define (scroll-down! x::real y::real)::boolean
    (with-editor-context
     (let* ((h ::real (painter:min-line-height)))
       (transform:translate! 0 (- h))
       #t)))

  (define (scroll-left! x::real y::real)::boolean
    (with-editor-context
     (let* ((w ::real (painter:space-width)))
       (transform:translate! w 0)
       #t)))

  (define (scroll-right! x::real y::real)::boolean
    (with-editor-context
     (let* ((w ::real (painter:space-width)))
       (transform:translate! (- w) 0))
     #t))

  (define (zoom-in! x::real y::real)::boolean
    (with-editor-context
     (transform:scale! 1.25 0 0)
     #t))

  (define (zoom-out! x::real y::real)::boolean
    (with-editor-context
     (transform:scale! 0.8 0 0)
     #t))

  (define (rotate-left! x::real y::real)::boolean
    (with-editor-context
     (transform:rotate! -5.0 x y)
     #t))

  (define (rotate-right! x::real y::real)::boolean
    (with-editor-context
     (transform:rotate! 5.0 x y)
     #t))

  (define (key-typed! key-code::long context::Cursor)::boolean
    (with-editor-context
     ((hash-ref keymap key-code
		(lambda () insert-character-input!)))
     #t))

  (define (can-split-beside? line::Area)::boolean
    (let* ((vicinity ::real
		     (painter:line-simplification-resolution))
	   (extent ::Extent (screen-extent (this)))
	   (3vicinity (* vicinity 3)))
      (and (is 0 < line:left <= line:right < extent:width)
	   (is line:top <= 3vicinity)
	   (is (- extent:height line:bottom) <= 3vicinity))))

  (define (split-beside! line::Area)::Embeddable
    (if (can-split-beside? line)
	(let*-values (((extent::Extent)
		       (screen-extent (this)))
		      ((ratio::real)
		       (/ (/ (+ line:left line:right)
			     2.0)
			  extent:width))
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
      (editor:pan-to!
       (let ((target ::Transform
		     (copy editor:transform)))
	 (target:translate! 0 (- (/ (- editor-height
				       cursor-height)
				    2) ye))
	 target)
       500 #;ms))))

