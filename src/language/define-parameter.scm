(module-name (language define-parameter))

(define-syntax define-parameter
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define-early-constant parameter-name :: parameter[type]
       (make-parameter initial-value)))

    ((_ (parameter-name) :: type)
     (define-early-constant parameter-name :: parameter[type]
       (make-parameter #!null)))
    
    ((_ (parameter-name) initial-value)
     (define-early-constant parameter-name :: parameter
       (make-parameter initial-value)))

    ((_ (parameter-name))
     (define-early-constant parameter-name :: parameter
       (make-parameter #!null)))
    ))

(define-syntax define-parameter+
  (syntax-rules (::)
    
    ((_ (parameter-name) :: type initial-value)
     (define parameter-name :: parameter[type]
       (make-parameter initial-value)))

    ((_ (parameter-name) :: type)
     (define parameter-name :: parameter[type]
       (make-parameter #!null)))
    
    ((_ (parameter-name) initial-value)
     (define parameter-name :: parameter
       (make-parameter initial-value)))

    ((_ (parameter-name))
     (define parameter-name :: parameter
       (make-parameter #!null)))
    ))
    

