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
(import (editor input pane))
(import (editor document parse))
(import (editor document documents))
(import (editor document cursor))
(import (editor types extensions widgets))
(import (editor input gestures))
(import (editor types primitive))
(import (editor input evaluation))
(import (editor types extensions visual-stepper))
(import (editor document history-tracking))
(import (editor types spaces))
(import (editor document copy-paste))


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
(set-key! '(ctrl x) cut-selection!)
(set-key! '(ctrl c) copy-selection!)
(set-key! '(ctrl v) paste-selection!)

(set-key! '(ctrl q) exit)

(set-key! 'page-up
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:scroll-up! left top))))

(set-key! 'page-down
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:scroll-down! left top))))

(set-key! '(ctrl page-up)
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:zoom-in! left top))))

(set-key! '(ctrl page-down)
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	    (screen:zoom-out! left top))))

(set-key! '(shift page-up)
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:scroll-left! left top))))

(set-key! '(shift page-down)
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:scroll-right! left top))))

(set-key! '(alt page-up)
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:rotate-left! left top))))

(set-key! '(alt page-down)
	  (lambda ()
	      (let* ((pivot ::Position (last-known-pointer-position 0))
		     (left ::real (slot-ref pivot 'left))
		     (top ::real (slot-ref pivot 'top)))
		(screen:rotate-right! left top))))

(set-key! '(ctrl e) evaluate-expression!)

(set-key! '(ctrl enter) evaluate-expression!)

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
		      ", expression: "(cursor-ref))))

(set-key! 'F2 (lambda ()
		(DUMP (last-operation))))

(invoke
 the-recognizers 'add
 split-pane-by-horizontal-line)

(invoke
 the-recognizers 'add
 split-pane-by-vertical-line)

(invoke
 the-recognizers 'add
 evaluate-expression-by-wedge)

(screen:set-content!
 (DocumentEditor document: (Document (car (with-input-from-string #;"
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
(Stepper (! 5))
#;(e.g. #;(! #;5) ===> 120)
(Button action: (lambda () (WARN \"button pressed!\"))
        label: \"Press me!\")

(Movement from: (Position left: 20 top: 20)
          to: (Position left: 120 top: 170)
          via: '())

(e.g.
 (parameterize ((the-cursor (cursor 0 1 3 1 1)))
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
