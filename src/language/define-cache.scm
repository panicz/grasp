(module-name (language define-cache))

(import (srfi :17))
(import (language define-syntax-rule))
(import (utils hash-table))
(import (language define-property))
(import (language keyword-arguments))
(import (language define-object))
(import (language match))
(import (language curry))

(define-synonym cache property+)

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
