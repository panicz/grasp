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
(import (editor types extensions testing))
(import (editor document history-tracking))
(import (editor types spaces))
(import (editor document copy-paste))
(import (editor types extensions combinators))

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
 (DocumentEditor
  document: (Document (car (with-input-from-string"
     (Welcome to GRASP)

#|GRASP is an extensible structutal editor
for Scheme and other s-expression based
programming languages.

MIND THAT GRASP IS A WORK IN PROGRESS
AND HASN'T YET BEEN OFFICIALLY RELEASED.

This file is stored as plain text and can be
opened in any text editor. However, GRASP
treats its components in a special way.

For example, this text is placed in a section
of a text file that the Scheme parser treats
as a block comment.|#

;; In Scheme, a block comment is any text
;; that is contained between the #| and |#
;; markers.

;; The text in here is stored in a section
;; of the text file known as line comments,
;; which span from the ; (semicolon) character
;; until the end of line.

\"But there's so much more to Scheme syntax
than comments. For example, this text in
here is a string, which - unlike comments
- can be passed to functions and bound to
variables.\"

#|You can navigate around the code by using keyboard
arrows and page-up/page-down keys. The whole
document is editable in a straightforward manner.
In particular, you can use ctrl+x for cutting
the expression to a clipboard, ctrl+v for pasting it,
ctrl+z for undo etc.

Note that all keyboard shortcuts are customizable,
and they are set up in the assets/init.scm file
that you can find in the project's repository, or
inside of the .jar or .apk file (they're both
secretly just .zip files).|#

;; And of course, if you know anything about Lisp,
;; you're certainly going to expect s-expressions
;; (like the following):

(define (! n)
  (if (<= n 1)
      1
      (* n (! (- n 1)))))

;; the above expression is the famous definition
;; of the factorial function. If you place the
;; cursor right behind it, or at the closing
;; parenthesis, and press ctrl+e, it will be
;; evaluated (in the future, I'd like evaluated
;; definitions to change color or a font face,
;; which should be rather easy to do, but I
;; currently have a lot of other things to do).

#|Once the underlying Scheme interpreter knows
about the factorial function, you can evaluate
the following expression:|#

(! 5)

#|But that's not all! I mentioned that GRASP
is an extensible editor, and this means that
it can have extensions. They are meant to be
user-defined, but there's a few of them that
are built-in.

At the moment, the most spectacular one is
the visual stepper. To activate it, position
the cursor on opening or closing parenthesis
of the expression below, and press the tab key.|#

(Stepper (! 5))

;; you should get something that looks roughly
;; like this:

\"
╔═══════════════════════════════════╗
║╭     ╮                            ║
║│ ! 5 │                            ║
║╰     ╯                            ║
║╭─────╮╭─────╮╭─────╮╭─────╮╭─────╮║
║│ ▮◀◀ ││ ▮◀  ││  ▶  ││  ▶▮ ││ ▶▶▮ │║
║╰─────╯╰─────╯╰─────╯╰─────╯╰─────╯║
╚═══════════════════════════════════╝
\"

;; go ahead and play with it!

;; Here are some other extensions that
;; are currently available:

(Button label: \"Press me!\"
        action: (lambda ()
(WARN
\"button pressed!\")))

(Movement
 from:
 (Position left: 20
       top: 20)
 to:
 (Position left: 120
       top: 170)
 via: '())

;; one particular form of extensions that you
;; might already be familiar with, are quotations:

`',x

`(,(a) ,b ,@c ,(d))

;; you should notice, that the concept of extensions
;; in GRASP is somewhat similar to the concept of
;; macros from Lisp.

         #|LIMITATIONS OF GRASP|#

#|On the surface, GRASP may look like a decent tool,
but there are reasons why it hasn't been released.

You will discover many of those reasons if you try
to actually use it (so I will not spend too much
time trying to enumerate them).

However, if you want to help out, you're most
welcome to do so! For more details, visit|#

https://github.com/panicz/grasp

" parse-document))
			     #!null)))

(WARN "loaded init.scm")

;; (#\] 3 2 1)
