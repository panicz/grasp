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
(import (utils functions))
(import (language for))
(import (language while))
(import (utils print))
(import (editor input input))
(import (utils conversions))
(import (language examples))
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

(import (editor types extensions visual-stepper))
(import (editor document history-tracking))
(import (editor types spaces))

(define-syntax $lookup$
  (syntax-rules ()
    (($lookup$ object method)
     (lambda args
       (apply invoke object method args)))))


(invoke
 (default-context) 'define! (Atom "ack")
  (car (parse-string "\
(lambda (m n)
  (if (<= m 0)
  (+ n 1)
  (if (= n 0)
  (ack (- m 1) 1)
  (ack (- m 1) (ack m (- n 1))))))")))

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

(set-key! '(ctrl e)
	  (lambda _
	    (and-let* ((`(,tip ,top . ,root) (the-cursor))
		       (parent ::Indexable (the-expression at: root))
		       (target ::Indexable (parent:part-at top))
		       ((eq? target (target:part-at tip)))
		       (target (if (Space? target)
				   (let* ((first-index ::Index (target:first-index))
					  (preceding-cursor (cursor-retreat (recons*
									first-index
									top root))))
				     (the-expression at: preceding-cursor))
				   target)))
	      (WARN "parent: "target))))
	  
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
(Stepper (! 5))
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
