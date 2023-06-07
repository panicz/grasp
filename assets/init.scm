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
(import (indexable))
(import (extension))
(import (editor-operations))
(import (pane))
(import (parse))
(import (cursor))
(import (button))

(set-key! 'left (lambda ()
		  (let ((cursor (the-cursor)))
		    (move-cursor-left!)
		    (WARN "moved cursor from "
			  cursor" to "(the-cursor)))))

(set-key! 'right (lambda ()
		   (let ((cursor (the-cursor)))
		     (move-cursor-right!)
		     (WARN "moved cursor from "
			  cursor" to "(the-cursor)))))

(set-key! 'up move-cursor-up!)
(set-key! 'down move-cursor-down!)

(set-key! '(shift left) expand-selection-left!)
(set-key! '(shift right) expand-selection-right!)

(set-key! '(ctrl z) undo!)
(set-key! '(ctrl y) redo!)

(set-key! '(ctrl q) exit)

(set-key! 'F12 exit)

(set-key! 'tab
	  (lambda ()
	    (let ((target (the-expression)))
	      (if (is target Enchanted?)
		  (perform&record!
		   (DisenchantExpression))
		  (perform&record!
		   (EnchantExpression))))))

(set-key! 'backspace delete-backward!)
(set-key! 'delete delete-forward!)

(set-key! 'F1 (lambda ()
		(WARN "cursor: "(the-cursor)
		      ", expression: "(the-expression))))

#|

(set-key! '(ctrl enter) evaluate!)

(set-key! '(ctrl mouse-wheel-up) zoom-in!)
(set-key! '(ctrl mouse-wheel-down) zoom-out!)

(set-key! 'mouse-wheel-up scroll-up!)
(set-key! 'mouse-wheel-down scroll-down!)
|#

(define-syntax $lookup$
  (syntax-rules ()
    (($lookup$ object method)
     (lambda args
       (apply invoke object method args)))))

(screen:set-content!
 (Editor document: (with-input-from-string #;"
          #|FAC|# #|  
TOR
  |# #|
IAL|# #|FAC
TOR
IAL|#
  ;  A factorial of an integer number n
  ;  is a product 1 * 2 * 3 * 4 * ... * n.
  ;  It represents the number of permutations
  ;  of an n-element set.""
`',x
(quote ())
(quote (a b))
(quote (a . b))
(define (! n) ; -> int
\"Computes the product 1*...*n.
It represents the number of per-
mutations of an n-element set.\"
  (if (<= n 0)
      1 #| BASE CASE |#
      (* n (! (- n 1))))) #|factorial
is a serious
business,
son|#
#;(e.g. #;(! #;5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")
" parse-document)))

(WARN "loaded init.scm")

;; (#\] 3 2 1)
