(module-name (editor types extensions canvas))

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
(import (utils print))

(define-type (Circle radius: real color: uint)
  extending Position
  implementing Renderable
  with
  ((render!)
   (painter:precise-fill-circle! left top radius color)))

(define-object (PreciseCanvas width ::real
			      height ::real
			      content ::(list-of
					 Renderable))
  ::Enchanted
  (define (draw! context ::Cursor) ::void  
    (for item::Renderable in content
      (item:render!))
    )
  
  (define size ::Extent (Extent width: width
				height: height))
  (define (extent)::Extent size)
  (Magic))

(set! (extension 'PreciseCanvas)
      (object (Extension)
	((enchant source ::cons)::Enchanted
	 (try-catch
	  (or (as PreciseCanvas (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Movement from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
