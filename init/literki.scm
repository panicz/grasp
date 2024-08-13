;; This file contains the default initialization,
;; and in particular - the key bindings.
;; It also contains a list of modules that
;; should be available to the interpreter
;; by default.

(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-property))
(import (language define-cache))
(import (language define-parameter))
(import (language fundamental))
(import (language infix))
(import (language match))
(import (language for))
(import (language while))
(import (language examples))

(import (utils functions))
(import (utils conversions))
(import (utils print))

(import (editor input input))
(import (editor interfaces elements))
(import (editor types extensions extensions))
(import (editor interfaces painting))
(import (editor document editor-operations))
(import (editor input screen))
(import (editor document parse))
(import (editor document documents))
(import (editor document cursor))
(import (editor types extensions widgets))
(import (editor input gestures))
(import (editor types primitive))
(import (editor input evaluation))
(import (editor types extensions visual-stepper))
(import (editor types extensions testing))
(import (editor document history-tracking))
(import (editor types spaces))
(import (editor document copy-paste))
(import (editor types extensions combinators))

(import (extra tile-board))

(define-syntax $lookup$
  (syntax-rules ()
    (($lookup$ object method)
     (lambda args
       (apply invoke object method args)))))

(slot-set! screen 'top (LetterTileBoard "POLA" say ask))
(invoke (slot-ref screen 'top) 'set-size!
	(slot-ref (slot-ref screen 'size) 'width)
	(slot-ref (slot-ref screen 'size) 'height))
