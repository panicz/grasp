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
(import (language attributes))
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
(import (editor input splits))
(import (editor input document-editor))
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
(import (editor types extensions canvas))
(import (editor types extensions physics))
(import (extra collisions))

(import (editor document history-tracking))
(import (editor types spaces))
(import (editor document copy-paste))
(import (editor types extensions combinators))

(import (utils reflection))

;;(import (extra tile-board))

(define-syntax $lookup$
  (lambda (stx)
    (syntax-case stx ()
      (($lookup$ object name)
       (let ((target (syntax->datum #'object))
	     (field (syntax->datum #'name)))
	 (and (defined? target)
	      (is (eval field) field-of?
		  (eval target))))
       #'(slot-ref object name))
      (($lookup$ object name)
       #'(if (is name field-of? object)
	     (slot-ref object name)
	     (lambda args
	       (apply invoke object name args)))))))

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

(set-key! '(ctrl comma)
	  (lambda ()
	   (wrap-expression-with! (Atom "unquote"))))

(set-key! '(ctrl backquote)
	  (lambda ()
	   (wrap-expression-with! (Atom "quasiquote"))))

(set-key! '(ctrl quote)
	  (lambda ()
	   (wrap-expression-with! (Atom "quote"))))

(set-key! '(shift left) expand-selection-left!)
(set-key! '(shift right) expand-selection-right!)

(set-key! '(ctrl z) undo!)
(set-key! '(ctrl y) redo!)
(set-key! '(ctrl x) cut-selection!)
(set-key! '(ctrl c) copy-selection!)
(set-key! '(ctrl v) paste-selection!)
(set-key! '(ctrl f) open-search-window)
(set-key! '(ctrl q) exit)

(set-key! 'page-up
	  (lambda ()
	    (let* ((pivot ::Position (last-known-pointer-position 0))
		   (left ::real (slot-ref pivot 'left))
		   (top ::real (slot-ref pivot 'top)))
	      (screen:scroll-up! left top))))

(set-key! 'page-down
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:scroll-down! pivot:left pivot:top))))

(set-key! '(ctrl page-up)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:zoom-in! pivot:left pivot:top))))

(set-key! '(ctrl page-down)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	    (screen:zoom-out! pivot:left pivot:top))))

(set-key! '(shift page-up)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:scroll-left! pivot:left pivot:top))))

(set-key! '(shift page-down)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:scroll-right! pivot:left pivot:top))))

(set-key! '(alt page-up)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:rotate-left! pivot:left pivot:top))))

(set-key! '(alt page-down)
	  (lambda ()
	    (let ((pivot ::Position (last-known-pointer-position 0)))
	      (screen:rotate-right! pivot:left pivot:top))))

(set-key! '(ctrl e) evaluate-expression!)

(set-key! '(ctrl enter) evaluate-expression!)

(set-key! '(alt enter) maximize/unmaximize!)

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

(set-key! '(ctrl h) halve-beside!)
	  
(set-key! '(ctrl shift h) halve-below!)

(set-key! '(ctrl alt h) (lambda ()
			  (WARN "join halves")))

(set-key! '(alt h) (lambda ()
		     (WARN "switch halves")))

(set-key! '(alt shift h) (lambda ()
			   (WARN "switch halves back")))

(set-key! '(ctrl s)
	  (lambda ()
	    (((save-file) 0 (the-editor)))))

(set-key! '(ctrl o)
	  (lambda ()
	    (((open-file) 0 (the-editor)))))

(set-key! 'F1 (lambda ()
		(WARN "cursor: "(the-cursor)
		      ", expression: "(cursor-ref))))

(set-key! 'F2 (lambda ()
		(DUMP screen:top)))

(the-recognizers:add
 split-pane-by-horizontal-line)

(the-recognizers:add
 split-pane-by-vertical-line)

(the-recognizers:add
 evaluate-expression-by-wedge)

(before-possible-exit
 (lambda ()
   (display "exitting?\n")
   (flush-output-port)))

(screen:after-tap
 (lambda _
   (show-keyboard!)))

(set! (stack-trace-action)
      (lambda (trace::string)
	::void
	(copy-to-clipboard! trace)
	(log-stack-trace! trace)
	(WARN "(copied to clipboard)")))

(if (null? input-files)
    (screen:set-content!
      (DocumentEditor
       document:
       (load-document-from-port
	(open-asset "intro.scm")
	(java.io.File "intro.scm"))))
    
    (screen:set-content!
     (open-beside (map (lambda (file-name ::java.lang.String)
			 ::java.io.File
			 (java.io.File file-name))
		       input-files))))

(WARN "loaded init.scm")

;; (#\] 3 2 1)
