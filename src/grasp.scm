(import (only (srfi :1) partition span break first second))

(import (language define-syntax-rule))
(import (language assert))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-property))
(import (language define-cache))
(import (language define-parameter))
(import (language mapping))
(import (language fundamental))
(import (language infix))
(import (language match))

(import (editor interfaces painting))

;(import (utils functions))
;(import (language for))
;(import (language while))
#|
(import (editor input pane))
(import (editor interfaces elements))

(import (utils print))
(import (editor types primitive))
(import (editor document cursor))
(import (editor input input))

(import (utils conversions))
(import (editor document parse))
(import (editor document editor-operations))
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
