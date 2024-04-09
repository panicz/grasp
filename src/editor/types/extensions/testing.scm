(module-name (editor types extensions testing))

(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language infix))
(import (language match))
(import (language for))
(import (utils functions))
(import (language fundamental))

(import (editor interfaces painting))
(import (editor types primitive))
(import (editor interfaces elements))
(import (editor types texts))
(import (editor types spaces))
(import (editor document cursor))
(import (editor input transforms))
(import (editor input pane))

(import (editor types extensions extensions))
(import (utils print))

(define-type (Movement from: Position
		       via: (list-of Position)
		       to: Position)
  extending Magic
  with
  
  ((draw! context::Cursor)
   (and-let* ((`(,last-point::Position . ,path) via))
     (for p::Position in path
       (painter:draw-thin-line! last-point:left last-point:top
				p:left p:top)
       (set! last-point p)))
   (when to
     (painter:draw-release-mark! to:left to:top))
   (when from
     (painter:draw-press-mark! from:left from:top)))
  
  ((extent)::Extent
   (let ((width ::real 0)
	 (height ::real 0)
	 (margin ::Extent
		 (painter:press/release-mark-extent)))
     (for p::Position in via
       (set! width (max width p:left))
       (set! height (max height p:top)))
     (Extent width: (+ width margin:width)
	     height: (+ height margin:height))))

  ((press! finger::byte x::real y::real)::boolean
   ;; no dobra, to musimy sobie przechwycic transformacje
   (set! from (Position left: x top: y))
   (set! to #!null)
   (set! via `(,from))
   (let ((tip via)
	 (transform (only-scale&rotation
		     the-transform-stack)))
     (screen:drag!
      finger
      (object (Drag)
	((move! x::real y::real dx::real dy::real)::void
	 (and-let* ((dx* dy* (transform:outside-in dx dy))
		    (`(,(Position left: left top: top)) tip))
	   (set-cdr! tip `(,(Position left: (max 0 (+ left dx*))
				      top: (max 0 (+ top dy*)))))
	   (set! tip (cdr tip))))
	((drop! x::real y::real vx::real vy::real)::void
	 (and-let* ((`(,(Position left: left top: top)) tip))
	   (set! to (Position left: left top: top))))
	))))

  ((value)::Object
   (let ((me ::Base (this)))
     (me:to-list (lambda (a d)
		   (cons a d))
		 to-expression)))
  )

(set! (extension 'Movement)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as Movement (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Movement from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
