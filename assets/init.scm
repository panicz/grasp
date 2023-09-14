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
(import (painter))
(import (editor-operations))
(import (pane))
(import (parse))
(import (document))
(import (cursor))
(import (button))
(import (recognizer))
(import (extent))
(import (stepper))

(define-syntax $lookup$
  (syntax-rules ()
    (($lookup$ object method)
     (lambda args
       (apply invoke object method args)))))


(set-key! 'left (lambda ()
		  (move-cursor-left!)
		  (adjust-view!)))

(set-key! 'right (lambda ()
		   (move-cursor-right!)
		   (adjust-view!)))

(set-key! 'up
	  (lambda ()
	    (move-cursor-up!)
	    (adjust-view!)))
	    
(set-key! 'down
	  (lambda ()
	    (move-cursor-down!)
	    (adjust-view!)))

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

(invoke
 (the-recognizers) 'add
 (Recognizer
  name: "horizontal-line"
  recognizes:
  (lambda (points::(sequence-of Position))
    (let* ((painter ::Painter (the-painter))
	   (vicinity ::real
		     (painter:line-simplification-resolution))
	   (simplified ::java.util.List
		       (simplify points vicinity)))
      (and-let* (((is (length simplified) = 2))
		 (p0 ::Position (simplified 0))
		 (p1 ::Position (simplified 1))
		 ((is (abs (- (slot-ref p0 'top)
			      (slot-ref p1 'top)))
		      <= (* vicinity 2)))
		 (line ::Area (area simplified)))
	(screen:can-split-below? line))))
  action:
  (lambda (own::Recognizer
	   points::(sequence-of Position))
    (let* ((line ::Area (area points)))
      (screen:split-below! line)))))

(invoke
 (the-recognizers) 'add
 (Recognizer
  name: "vertical-line"
  recognizes:
  (lambda (points::(sequence-of Position))
    (let* ((painter ::Painter (the-painter))
	   (vicinity ::real
		     (painter:line-simplification-resolution))
	   (simplified ::java.util.List
		       (simplify points vicinity)))
      (and-let* (((is (length simplified) = 2))
		 (p0 ::Position (simplified 0))
		 (p1 ::Position (simplified 1))
		 ((is (abs (- (slot-ref p0 'left)
			      (slot-ref p1 'left)))
		      <= (* vicinity 2)))
		 (line ::Area (area simplified)))
	(screen:can-split-beside? line))))
  action:
  (lambda (own::Recognizer
	   points::(sequence-of Position))
    (let* ((line ::Area (area points)))
       (screen:split-beside! line)))))


#|

(set-key! '(ctrl enter) evaluate!)

(set-key! '(ctrl mouse-wheel-up) zoom-in!)
(set-key! '(ctrl mouse-wheel-down) zoom-out!)

(set-key! 'mouse-wheel-up scroll-up!)
(set-key! 'mouse-wheel-down scroll-down!)
|#

(screen:set-content!
 (Editor document: (Document (car (with-input-from-string #;"
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

(e.g.
 (parameterize ((the-cursor (cursor 0 1 3 1 1))
		(the-selection-anchor (cursor 0 1 3 1 1)))
   (grasped \"\\
(define (! n)
  (if (<= n 0)
      1
      (* n (! (- n 1)))))
\"))
===>
\"
╭        ╭     ╮                      ╮
│ define │ ! n │                      │
│        ╰ ^   ╯                      │
│   ╭    ╭        ╮                 ╮ │
│   │ if │ <= n 0 │                 │ │
│   │    ╰        ╯                 │ │
│   │                               │ │
│   │       1                       │ │
│   │                               │ │
│   │       ╭     ╭   ╭       ╮ ╮ ╮ │ │
│   │       │ * n │ ! │ - n 1 │ │ │ │ │
╰   ╰       ╰     ╰   ╰       ╯ ╯ ╯ ╯ ╯
\")
" parse-document))
			     #!null)))

(WARN "loaded init.scm")

;; (#\] 3 2 1)
