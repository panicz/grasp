(module-name (editor types extensions module-viewer))

(import (language assert))
(import (language infix))
(import (language define-object))
(import (language define-type))
(import (language define-interface))
(import (language attributes))
(import (language define-parameter))
(import (language define-cache))
(import (language keyword-arguments))

(import (language match))
(import (language for))
(import (utils functions))
(import (language examples))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor interfaces painting))
(import (editor types spaces))

(import (editor types extensions extensions))
(import (editor document parse))
(import (language mapping))
(import (utils print))
(import (editor types extensions widgets))
(import (utils hash-table))

(import (editor document universe))

(define-object (ModuleViewer)
  ::Enchanted
  (define total ::Extent (Extent width: 0 height: 0))
  
  (define (extent)::Extent total)

  (define module-position ::(maps ((either Document ModuleTag)) to: Position)
    (attribute+ (module ::(either Document ModuleTag))::Position
		(Position left: 0 top: 0)))

  (define module-extent ::(maps ((either Document ModuleTag)) to: Extent)
    (attribute+ (module ::(either Document ModuleTag))::Extent
		(Extent width: 0 height: 0)))

  (define (draw-document-box! document ::Document measure ::Extent)::void
    (let* ((caption (match document:source
		      (file::java.io.File
		       ;; TODO - docelowo chcemy tutaj zbudowac
		       ;; "skrocony unikatowy identyfikator pliku"
		       ;; wzgledem zbioru otwartych plikow
		       ;; (mniej wiecej tak jak to robi Emacs)
		       (file:getName))
		      (#!null
		       "#!null")
		      (something-else
		       (something-else:toString))))
	   (extent ::Extent (painter:caption-extent caption))
	   (top ::real (painter:caption-margin-top))
	   (left ::real (painter:caption-horizontal-margin))
	   (width ::real (+ (* 2 left) extent:width))
	   (height ::real (+ top extent:height (painter:caption-margin-bottom))))
      (painter:draw-rectangle! width height)
      (with-translation (left top)
	(painter:draw-caption! caption))
      (set! measure:width width)
      (set! measure:height height)))

  (define (draw-module-box! module-tag ::ModuleTag measure ::Extent)::void
    (let* ((caption-extents (map (lambda (part::symbol)
				   (let ((caption (symbol->string part)))
				     `(,caption ,(painter:caption-extent caption))))
				 module-tag))
	   (top ::real (painter:caption-margin-top))
	   (left ::real (painter:caption-horizontal-margin))
	   (width ::real (+ (* 2 left)
			    (fold-left (lambda (w::real caption+extent)
					 (and-let* ((`(,caption ,extent::Extent)
						     caption+extent))
					   (max w extent:width)))
				       0
				       caption-extents)))
	   (height ::real (+ top (painter:caption-margin-bottom)
			     (fold-left (lambda (h::real caption+extent)
					  (and-let* ((`(,caption ,extent::Extent)
						      caption+extent))
					    (+ h extent:height)))
					0
					caption-extents))))
      (painter:draw-dashed-rectangle! width height)
      
      (for (caption extent) in caption-extents
	(with-translation (left top)
	  (painter:draw-caption! caption))
	(set! top (+ top extent:height)))
      
      (set! measure:width width)
      (set! measure:height height)))

  (define (draw! context ::Cursor) ::void
    (let ((top ::real 0)
	  (viewer-width ::real 0))
      (for layer in (project-layers)
	(let ((layer-height ::real 0)
	      (left ::real 0))
	  (for module in layer
	    (let ((position (module-position module))
		  (measured (module-extent module)))
	      (set! position:left left)
	      (set! position:top top)
	      (with-translation (left top)
		(match module
		  (document::Document
		   (draw-document-box! document measured))
		  (_
		   (draw-module-box! module measured))))
	      (set! layer-height (max layer-height measured:height))
	      (set! left (+ left measured:width (painter:module-view-interspace)))))
	  (set! viewer-width (max viewer-width left))
	  (set! top (+ top layer-height (painter:module-view-interline)))))
      (set! total:width viewer-width)
      (set! total:height top)))
  
  (Magic))

(set! (extension 'ModuleViewer)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (as ModuleViewer (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create ModuleViewer from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
