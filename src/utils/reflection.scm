(module-name (utils reflection))

(import (language define-type))
(import (language define-object))

(import (language assert))
(import (language match))
(import (language examples))
(import (language infix))
(import (language while))
(import (language for))

(import (utils functions))

(define (direct-methods object)
  (map (lambda (method)
	 (list
	  (cons
	   (string->symbol (method:getName))
	   (map (chain _ 'getName ,string->symbol)
		(method:getParameterTypes)))
	  ':: (chain method 'getReturnType 'getName ,string->symbol)))
       (chain object 'getClass 'getDeclaredMethods)))

(define (direct-fields object)
  (map (lambda (field)
	 (list
	  (string->symbol (field:getName))
	  ':: (string->symbol (chain field 'getType 'getName))
	  (field:get object)))
       (chain object 'getClass 'getDeclaredFields)))

(define (field-of? name::symbol object)
  ::boolean
  (let ((name (symbol->string name)))
    (any (is (_:getName) string=? name)
	 (chain object 'getClass 'getFields))))

(define (defined? s)::boolean
  (try-catch
   (begin
     (eval s)
     #t)
   (ex java.lang.Throwable
       #f)))
