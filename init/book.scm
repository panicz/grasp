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
(import (language mapping))

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
(import (editor document universe))
(import (editor document cursor))
(import (editor types extensions widgets))
(import (editor input gestures))
(import (editor types primitive))
(import (editor input evaluation))
(import (editor types extensions visual-stepper))
(import (editor types extensions testing))
(import (editor types extensions canvas))
(import (editor types extensions physics))
(import (editor types extensions module-viewer))
(import (editor types extensions gutenbergson))

(import (extra collisions))

(import (editor document history-tracking))
(import (editor types spaces))
(import (editor document copy-paste))
(import (editor types extensions combinators))
(import (editor input transforms))

(import (utils graph))
(import (utils server))
(import (utils reflection))

(import (utils file))


(define settings-file
  (join-path
   (application-directory)
   "tmipew-settings.scm"))

(define the-book
  (let ((book (open-asset "book.org")))
    (parse-book book)))

(define the-reader
  (InteractiveBookReader the-book))

(define reader-settings
  (object-mapping the-reader 'current-chapter 'scroll 'scale))

(when (file-exists? settings-file)
  (load-mapping-from settings-file
		     #;into reader-settings))

(slot-set! screen 'top the-reader)

(before-possible-exit
 (lambda ()
   (save-mapping reader-settings settings-file)))

(set-window-title! (slot-ref the-book 'title))

(invoke (slot-ref screen 'top) 'set-size!
	(slot-ref (slot-ref screen 'size) 'width)
	(slot-ref (slot-ref screen 'size) 'height)
	(invoke screen 'resize-anchor
		(slot-ref (slot-ref screen 'size) 'height)))
