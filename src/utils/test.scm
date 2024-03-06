(module-name (utils test))

(import (language define-syntax-rule))
(import (language define-parameter))
(import (language examples))
(import (language assert))
(import (language infix))
(import (language match))
(import (language while))

(import (utils hash-table))
(import (editor document documents))
(import (editor document document-operations))
(import (editor document editor-operations))
(import (editor document history-tracking))
(import (editor interfaces painting))
(import (editor interfaces elements))
(import (editor text-painter))

(import (editor interfaces painting))

(define-parameter (test-failed) ::procedure error)

(define (snapshot)::String
  (with ((painter (TextPainter)))
    (reset! extent-cached?)
    (draw-document! (the-document))
    (let ((result ::String (painter:toString)))
      (unless (in-example-context?)
	(display result))
      ;;(display (history (the-document)))
      result)))

(define-syntax-rule (with-undo-redo operation)
  (let ((initial (snapshot)))
    operation
    (let ((final (snapshot)))
      (undo!)
      (let ((reverted (snapshot)))
	(unless (equal? initial reverted)
	  ((test-failed) "Unexpected output after undoing "'operation
	   "\nexpected:\n"
	   initial
	   "\ngot:\n"
	   reverted)))
      (redo!)
      (let ((redone (snapshot)))
	(unless (equal? final redone)
	  ((test-failed) "Unexpected output after redoing "'operation
	   "\nexpected:\n"
	   final
	   "\ngot:\n"
	   redone)))
      final)))
