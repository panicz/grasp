(module-name (language keyword-arguments))

(import (language match))
(import (utils conversions))

(define-syntax lambda/kw
  (lambda (stx)
    (syntax-case stx ()
      ((_ args . body)
       (identifier? #'args)
       #'(lambda args . body))

      ((_ args . body)
       #'(%lambda/kw args #;req () #;opt () #;kw ()
		     #;destruct () body)))))

(define-syntax %lambda/kw
  (lambda (stx)
    (syntax-case stx (:= ::)
            
      ((_ () (req ...) (opt ...) (kw ...) (pat ...) (:: type . body))
       #'(lambda (req ... #!optional opt ... #!key kw ...) :: type
            (match-let* (pat ...) . body)))
      
      ((_ tail (req ...) (opt ...) (kw ...) (pat ...) (:: type . body))
       (identifier? #'tail)
       #'(lambda (req ... #!optional opt ... #!key kw ... #!rest tail) :: type
            (match-let* (pat ...) . body)))

      ((_ () (req ...) (opt ...) (kw ...) (pat ...) body)
       #'(lambda (req ... #!optional opt ... #!key kw ...)
           (match-let* (pat ...) . body)))
      
      ((_ tail (req ...) (opt ...) (kw ...) (pat ...) body)
       (identifier? #'tail)
       #'(lambda (req ... #!optional opt ... #!key kw ... #!rest tail)
           (match-let* (pat ...) . body)))
      
      ;; keyword arguments:
      
      ((_ (key pattern :: type := init . rest) req opt (kw ...) (pat ...) body)
       (keyword? (syntax->datum #'key))

       (with-syntax ((sym (datum->syntax stx
                            (keyword->symbol
                             (syntax->datum #'key)))))
         #'(%lambda/kw rest req opt (kw ... (sym :: type init))
                       (pat ... (pattern sym)) body)))

      ((_ (key pattern :: type . rest) req opt (kw ...) (pat ...) body)
       (keyword? (syntax->datum #'key))
       (with-syntax ((sym (datum->syntax stx
                            (keyword->symbol
                             (syntax->datum #'key)))))
         #'(%lambda/kw rest req opt (kw ... (sym :: type))
                       (pat ... (pattern sym)) body)))
      
      ((_ (key pattern := init . rest) req opt (kw ...) (pat ...) body)
       (keyword? (syntax->datum #'key))
       (with-syntax ((sym (datum->syntax stx
                            (keyword->symbol
                             (syntax->datum #'key)))))
         #'(%lambda/kw rest req opt (kw ... (sym init))
                       (pat ... (pattern sym)) body)))

      ((_ (key pattern . rest) req opt (kw ...) (pat ...) body)
       (keyword? (syntax->datum #'key))
       (with-syntax ((sym (datum->syntax stx
                            (keyword->symbol
                             (syntax->datum #'key)))))
         #'(%lambda/kw rest req opt (kw ... (sym))
                       (pat ... (pattern sym)) body)))

      ;; optional arguments:
      
      ((_ (var :: type := init . rest) req (opt ...) kw pat body)
       (identifier? #'var)
       #'(%lambda/kw rest req (opt ... (var :: type init)) kw pat body))

      ((_ (pattern :: type := init . rest) req (opt ...) kw (pat ...) body)
       #'(%lambda/kw rest req (opt ... (var :: type init)) kw
                     (pat ... (pattern var)) body))

      ((_ (var := init . rest) req (opt ...) kw pat body)
       (identifier? #'var)
       #'(%lambda/kw rest req (opt ... (var init)) kw pat body))

      ((_ (pattern := init . rest) req (opt ...) kw (pat ...) body)
       #'(%lambda/kw rest req (opt ... (var init)) kw
                     (pat ... (pattern var)) body))

      ;; required arguments:
      
      ((_ (var :: type . rest) (req ...) opt kw pat body)
       (identifier? #'var)
       #'(%lambda/kw rest (req ... var :: type) opt kw pat body))

      ((_ (var . rest) (req ...) opt kw pat body)
       (identifier? #'var)
       #'(%lambda/kw rest (req ... var) opt kw pat body))

      ((_ (pattern . rest) (req ...) opt kw (pat ...) body)
       #'(%lambda/kw rest (req ... var) opt kw
                     (pat ... (pattern var)) body))
      
      )))


(define-syntax define/kw
  (syntax-rules (is ::)
    ((_ (is arg special?) . body)
     (define/kw (special? arg) . body))

    ((_ (is arg-1 related-to? arg-2) . body)
     (define/kw (related-to? arg-1 arg-2) . body))

    ((_ ((head . tail) . args) . body)
     (define/kw (head . tail) (lambda/kw args . body)))

    ((_ (name . args) . body)
     (define name (lambda/kw args . body)))
    
    ))
