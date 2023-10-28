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
(import (editor input evaluation))
(import (editor types extensions visual-stepper))
(import (editor document history-tracking))
(import (editor types spaces))

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
	(and (screen:can-split-below? line)
	     line))))
  action:
  (lambda (own::Recognizer
	   points::(sequence-of Position)
	   line::Area)
    (screen:split-below! line))))

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
	(and (screen:can-split-beside? line)
	     line))))
  action:
  (lambda (own::Recognizer
	   points::(sequence-of Position)
	   line::Area)
    (screen:split-beside! line))))

(invoke
 (the-recognizers) 'add
 (Recognizer
  name: "eval"
  recognizes:
  (lambda (points::(sequence-of Position))
    (and-let* ((painter ::Painter (the-painter))
	       (vicinity ::real
			 (painter:line-simplification-resolution))
	       (simplified ::java.util.List
			   (simplify points vicinity))
	       ((is (length simplified) = 3))
	       (top ::Embeddable (slot-ref screen 'top))
	       ((Position left: p0-left top: p0-top) (simplified 0))
	       ((Position left: p1-left top: p1-top) (simplified 1))
	       ((Position left: p2-left top: p2-top) (simplified 2))
	       ((is p0-left < p1-left < p2-left))
	       ((is p1-top < p0-top))
	       ((is p1-top < p2-top))
	       (editor0 ::Editor (top:pane-under p0-left p0-top))
	       (editor1 ::Editor (top:pane-under p1-left p1-top))
	       (editor2 ::Editor (top:pane-under p2-left p2-top))
	       ((eq? editor0 editor1))
	       ((eq? editor1 editor2))
	       (document ::Document (slot-ref editor1 'document))
	       (x0 y0 (top:map p0-left p0-top))
	       (x1 y1 (top:map p1-left p1-top))
	       (x2 y2 (top:map p2-left p2-top))
	       (_ (truly (DUMP x1 y1)))
	       (c0 ::Cursor (cursor-under x0 y0 (head document)))
	       (c1 ::Cursor (cursor-under x1 y1 (head document)))
	       (c2 ::Cursor (cursor-under x2 y2 (head document)))
	       ((eq? (cdr c0) (cdr c2)))
	       (n ::integer (length c0))
	       (m ::integer (length c1))
	       ((is m >= n))
	       (cursor (drop (- m n) c1))
	       ((pair? cursor)))
      (cons (cdr cursor) document)))
  action:
  (lambda (self::Recognizer points::(sequence-of Position) result)
    (and-let* ((`(,cursor . ,document) result))
      (evaluate-expression! at: cursor in: document)))))

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
