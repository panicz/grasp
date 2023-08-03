(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))
(import (define-property))
(import (keyword-arguments))
(import (define-object))
(import (match))

(define-synonym cache property+)

(define-syntax curried
  (lambda (stx)
    (syntax-case stx (:: :=)

      ((_ kw (key arg :: type := value . args) body)
       (keyword? (syntax->datum #'key))
       #'(kw (arg::type) (curried kw args body)))


      ((_ kw (key arg :: type . args) body)
       (keyword? (syntax->datum #'key))
       #'(kw (arg::type) (curried kw args body)))

      ((_ kw (key arg := value . args) body)
       (keyword? (syntax->datum #'key))
       #'(kw (arg) (curried kw args body)))
      
      ((_ kw (key arg . args) body)
       (keyword? (syntax->datum #'key))
       #'(kw (arg) (curried kw args body)))

      ((_ kw (arg :: type := value . args) body)
       #'(kw (arg::type) (curried kw args body)))

      ((_ kw (arg :: type . args) body)
       #'(kw (arg::type) (curried kw args body)))

      ((_ kw (arg := value . args) body)
       #'(kw (arg) (curried kw args body)))
      
      ((_ kw (arg . args) body)
       #'(kw (arg) (curried kw args body)))
      
      ((_ kw () body)
       #'body)
      )))

(define-syntax curried-application
  (lambda (stx)
    (syntax-case stx (:: :=)
      ((_ procedure)
       #'procedure)

      ((_ procedure key arg :: type := value args ...)
       (keyword? (syntax->datum #'key))      
       #'(curried-application (procedure arg) args ...))

      ((_ procedure key arg :: type args ...)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))

      ((_ procedure key arg := value args ...)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))

      ((_ procedure key arg args ...)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))
      
      ((_ procedure arg :: type := value args ...)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure arg :: type args ...)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure arg := value args ...)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure arg args ...)
       #'(curried-application (procedure arg) args ...))
      )))

(define-syntax define-cache
  (syntax-rules (::)
    ((define-cache (name . args)::type body)
     (define-early-constant name
       (let* ((cached (curried cache args body))
	      (invoker (lambda/kw args::type
			 (curried-application cached . args))))
	 (set-procedure-property! invoker 'cache cached)
	 invoker)))

    ((define-cache (name . args) body)
     (define-early-constant name
       (let* ((cached (curried cache args body))
	      (invoker (lambda/kw args
			 (curried-application cached . args))))
	 (set-procedure-property! invoker 'cache cached)
	 invoker)))
  ))

(define (invalidate! cache . point)
  (let ((table ::Map (procedure-property cache 'table)))
    (match point
      ('() (table:clear))
      (`(,point) (table:remove point))
      (`(,head . ,tail)
       (apply invalidate! (cache head) tail)))))

(define (invalidate-cache! invoker . point)
  (apply invalidate! (procedure-property invoker 'cache) point))

;; the `hash-cons` definition presented here is only used
;; for bootstrapping. Prefer the analogous definition of
;; `recons` that is defined in the `(primitive)` module.

(define-object (immutable-pair car cdr)

  (define (setCar value)
    (error "The pair is immutable: " (this)))

  (define (setCdr value)
    (error "The pair is immutable: "(this)))
  
  (pair car cdr))

(define-cache (hash-cons head tail)
  (immutable-pair head tail))

(define-syntax hash-cons*
  (syntax-rules ()
    ((_ a b)
     (hash-cons a b))
    
    ((_ a b c ...)
     (hash-cons a (hash-cons* b c ...)))))
