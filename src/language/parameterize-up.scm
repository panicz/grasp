(module-name (language parameterize-up))

(import (language define-syntax-rule))

;; like `parameterize`, but updates parameter sources
;; after the execution of the inner block

(define-syntax-rule (parameterize/update-sources ((param source) ...)
		      body + ...)
  (parameterize ((param source) ...)
    (try-finally
     (begin body + ...)
     (begin (set! source (param)) ...))))


