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
(import (examples))
(import (editor-operations))
(import (pane))
(import (parse))

(set-key! 'left move-cursor-left!)

(set-key! 'right move-cursor-right!)
(set-key! 'up move-cursor-up!)
(set-key! 'down move-cursor-down!)

(set-key! '(shift left) expand-selection-left!)
(set-key! '(shift right) expand-selection-right!)

(set-key! '(ctrl z) undo!)
(set-key! '(ctrl y) redo!)

(set-key! '(ctrl q) exit)

(set-key! 'F12 exit)

(set-key! 'backspace delete-backward!)
(set-key! 'delete delete-forward!)

(set-key! '(ctrl /) (lambda ()
		      (WARN "cursor: "(the-cursor)", selection: "
			    (the-selection-anchor))))

#|

(set-key! '(ctrl enter) evaluate!)

(set-key! '(ctrl mouse-wheel-up) zoom-in!)
(set-key! '(ctrl mouse-wheel-down) zoom-out!)

(set-key! 'mouse-wheel-up scroll-up!)
(set-key! 'mouse-wheel-down scroll-down!)
|#

(when (is (the-screen) instance? Editor)
    (let ((editor ::Editor (as Editor (the-screen))))
      (slot-set! editor 'document
		 (with-input-from-string "\
(define (! n)
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1
      (* n (! (- n 1))))) 
(e.g. (factorial 5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document))))

(WARN "loaded init.scm")
