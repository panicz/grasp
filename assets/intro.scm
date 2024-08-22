
(Welcome to GRASP) ;; to exit, press F12

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

"But there's so much more to Scheme syntax
than comments. For example, this text in
here is a string, which - unlike comments
- can be passed to functions and bound to
variables."

#|You can navigate around the code by using
keyboard arrows and page-up/page-down keys.
The whole document is editable
in a straightforward manner. In particular,
you can use ctrl+x for cutting the expression
to a clipboard, ctrl+v for pasting it,
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

"
╔═══════════════════════════════════╗
║╭     ╮                            ║
║│ ! 5 │                            ║
║╰     ╯                            ║
║╭─────╮╭─────╮╭─────╮╭─────╮╭─────╮║
║│ ▮◀◀ ││ ▮◀  ││  ▶  ││  ▶▮ ││ ▶▶▮ │║
║╰─────╯╰─────╯╰─────╯╰─────╯╰─────╯║
╚═══════════════════════════════════╝
"

;; go ahead and play with it!

;; Here are some other extensions that
;; are currently available:

(Button label: "Press me!"
        action: (lambda ()
(WARN
"button pressed!")))

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

