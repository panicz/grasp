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
  (define (draw! context ::Cursor) ::void
    (values))

  (define size ::Extent (Extent width: 10 height: 5))
  
  (define (extent)::Extent size)
  
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
