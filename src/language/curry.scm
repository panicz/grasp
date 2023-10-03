(module-name (language curry))

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
