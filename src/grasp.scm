(import (only (srfi :1) partition span break first second))

(import (define-syntax-rule))
(import (assert))
(import (define-interface))
(import (define-type))
(import (define-object))
(import (define-property))
(import (define-cache))
(import (define-parameter))
(import (default-value))
(import (mapping))
(import (fundamental))
(import (infix))
(import (match))

(import (painter))

;(import (functions))
;(import (for))
;(import (while))
#|
(import (pane))
(import (indexable))

(import (print))
(import (primitive))
(import (cursor))
(import (input))
(import (extent))
(import (conversions))
(import (parse))
(import (editor-operations))
|#

(import (grasp-desktop))
(import (grasp-terminal))

(define-alias System java.lang.System)
(define-alias GraphicsEnvironment java.awt.GraphicsEnvironment)

(define files '())
(define options '())

#|
(match (command-line)
  (`(,command . ,args)
   (let-values (((command-line-options given-files)
		 (partition (is _ matching "^--") args)))
     (set! options command-line-options)
     (set! files given-files))))
|#

(define (process-command-line-arguments)
  (define (process arguments)
    (match arguments
      (`("--terminal" . ,rest)
       (System:setProperty "java.awt.headless", "true")
       (process rest))
      (`("--" . ,rest)
       (set! files rest))
      ))
  (match (command-line)
    (`(,program . ,arguments)
     (process arguments))))

(process-command-line-arguments)

(if (GraphicsEnvironment:isHeadless)
    (run-in-AWT-window)
    (run-in-terminal))
