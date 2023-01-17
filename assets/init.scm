;; This file contains the default initialization,
;; and in particular - the key bindings.
;; It also contains a list of modules that
;; should be available to the interpreter
;; by default.

(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (define-parameter))
(import (default-value))
(import (fundamental))
(import (infix))
(import (match))
(import (functions))
(import (for))
(import (while))
(import (print))
(import (input))
(import (conversions))
(import (editor-operations))

(set-key! 'left move-cursor-left!)
(set-key! 'right move-cursor-right!)
(set-key! 'up move-cursor-up!)
(set-key! 'down move-cursor-down!)

(set-key! '(ctrl z) undo!)
(set-key! '(ctrl y) redo!)

#|
(set-key! 'backspace delete-backward!)
(set-key! 'delete delete-forward!)

(set-key! '(ctrl enter) evaluate!)

(set-key! '(ctrl mouse-wheel-up) zoom-in!)
(set-key! '(ctrl mouse-wheel-down) zoom-out!)

(set-key! 'mouse-wheel-up scroll-up!)
(set-key! 'mouse-wheel-down scroll-down!)
|#

(WARN "loaded init.scm")
