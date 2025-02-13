(module-name (editor types extensions physics))

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
(import (editor input screen))

(import (editor types extensions extensions))
(import (editor types extensions combinators))
(import (editor types extensions canvas))
(import (extra collisions))

(import (utils print))

(define-object (PhysicsStage width ::real
			     height ::real
			     content ::(list-of
					Animate))
  ::Animation
  (define (set-size! w::real h::real anchor::ResizeAnchor)
    ::void
    (set! width w)
    (set! height h)
    (invoke-special
     PreciseCanvas (this) 'set-size!
     w h anchor))

  (define (advance! timestep/ms::int)::boolean
    (for item ::Animate in content
	 (item:advance! timestep/ms)))

  (PreciseCanvas width height content))

(set! (extension 'PhysicsStage)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (bordered
	       (as AnimateCanvas (eval source))) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Movement from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
