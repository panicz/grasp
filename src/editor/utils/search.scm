(module-name (editor utils search))

(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (editor interfaces elements))
(import (editor types primitive))
(import (utils print))

(define (matches pattern
		 subject
		 #!key
		 (bindings::(!maps (symbol-name ::String)
				   to: Object)
			    (mapping (Object) #!null)))
  ::(maybe (maps (Object) to: Object))
  (otherwise #!null
    (match pattern
      (enchanted::Enchanted
       (matches (enchanted:value) subject bindings: bindings))
      (`(,,@(is _ match/equal? 'unquote) ,identifier)
       (cond
	((and (isnt identifier Atom?)
	      (isnt identifier symbol?))
	 (and (match/equal? identifier subject)
	      bindings))
	((is identifier match/equal? '_)
	 bindings)
	((is bindings overridden-at? (identifier:toString))
	 (and (match/equal? (bindings (identifier:toString))
			    subject)
	      bindings))
	(else
	 (set! (bindings (identifier:toString)) subject)
	 bindings)))

      (`(,head-pattern . ,tail-pattern)
       (and-let* ((`(,head-subject . ,tail-subject) subject)
		  (bindings* (matches head-pattern
				      head-subject
				      bindings: bindings)))
	 (matches tail-pattern tail-subject bindings: bindings*)))
      (_
       (and (match/equal? pattern subject)
	    bindings)))))
