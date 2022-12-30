(import (srfi :17))
(import (define-syntax-rule))
(import (hash-table))
(import (define-property))
(import (keyword-arguments))
(import (define-object))

(define-synonym cache property+)

(define-syntax curried
  (lambda (stx)
    (syntax-case stx (:: :=)

      ((_ kw (key arg :: type := value . args) body)
       (keyword? (syntax->datum #'key))
       #'(curried kw args (kw (arg::type) body)))

      ((_ kw (key arg :: type . args) body)
       (keyword? (syntax->datum #'key))
       #'(curried kw args (kw (arg::type) body)))

      ((_ kw (key arg := value . args) body)
       (keyword? (syntax->datum #'key))
       #'(curried kw args (kw (arg) body)))
      
      ((_ kw (key arg . args) body)
       (keyword? (syntax->datum #'key))
       #'(curried kw args (kw (arg) body)))

      ((_ kw (arg :: type := value . args) body)
       #'(curried kw args (kw (arg::type) body)))

      ((_ kw (arg :: type . args) body)
       #'(curried kw args (kw (arg::type) body)))

      ((_ kw (arg := value . args) body)
       #'(curried kw args (kw (arg) body)))
      
      ((_ kw (arg . args) body)
       #'(curried kw args (kw (arg) body)))
      
      ((_ kw () body)
       #'body)
      )))

(define-syntax curried-application
  (lambda (stx)
    (syntax-case stx (:: :=)
      ((_ procedure)
       #'procedure)

      ((_ procedure args ... key arg :: type := value) 
       (keyword? (syntax->datum #'key))      
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... key arg :: type)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... key arg := value)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... key arg)
       (keyword? (syntax->datum #'key))
       #'(curried-application (procedure arg) args ...))
      
      ((_ procedure args ... arg :: type := value)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... arg :: type)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... arg := value)
       #'(curried-application (procedure arg) args ...))

      ((_ procedure args ... arg)
       #'(curried-application (procedure arg) args ...))
      )))

(define-syntax define-cache
  (syntax-rules (::)
    ((define-cache (name . args)::type body)
     (define-early-constant name
       (let ((cached (curried cache args body)))
	 (lambda/kw args::type (curried-application cached . args)))))

    ((define-cache (name . args) body)
     (define-early-constant name
       (let ((cached (curried cache args body)))
	 (lambda/kw args (curried-application cached . args)))))
  ))

;; the `hash-cons` definition presented here is only used
;; for bootstrapping. Prefer the analogous definition of
;; `recons` that is defined in the `(primitive)` module.

(define-object (immutable-pair a d)

  (define (setCar value)
    (error "The pair is immutable: " (this)))

  (define (setCdr value)
    (error "The pair is immutable: "(this)))
  
  (pair a d))

(define-cache (hash-cons head tail)
  (immutable-pair head tail))

(define-syntax hash-cons*
  (syntax-rules ()
    ((_ a b)
     (hash-cons a b))
    
    ((_ a b c ...)
     (hash-cons a (hash-cons* b c ...)))))
