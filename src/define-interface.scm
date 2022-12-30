(import (define-syntax-rule))

(define-syntax-rule (define-interface name supers prototypes ...)
  (interface-definition name supers (prototypes ...) ()))

(define-syntax interface-definition
  (syntax-rules ()
    ((_ name supers () methods)
     (define-simple-class name supers interface: #t . methods))
    
    ((_ name supers (method :: result . rest) (methods ...))
     (interface-definition
      name supers rest
      (methods ... (method :: result #!abstract))))
    ))

