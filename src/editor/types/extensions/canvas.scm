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
(import (editor types extensions extensions))

(define-type (Circle radius: real color: uint)
  extending Position
  implementing Renderable
  with
  ((render!)
   (painter:fill-circle! left top radius color)))

(define-type (Canvas size: Extent content: (list-of Renderable))
  extending Magic with
  ((draw! context ::Cursor) ::void  
   (for item::Renderable in content
     (item:render!))
   )
  ((extent)::Extent size)
  )

(set! (extension 'Canvas)
      (object (Extension)
	((enchant source::cons)::Enchanted
	 (try-catch
	  (or (as Canvas (eval source)) #!null)
	  (ex java.lang.Throwable
	      (WARN "Unable to create Movement from "source": "
		    (java.lang.String:valueOf ex))
	      #!null)))))
