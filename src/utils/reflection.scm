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

(define (direct-methods object)::list
  (let* ((class ::java.lang.Class (object:getClass))
	 (methods (class:getMethods)))
    (map (lambda (method::java.lang.reflect.Method)
	   `((,(string->symbol (method:getName))
	      . ,(map (lambda (param::java.lang.Class)
			(string->symbol
			 (param:getName)))
		      (method:getParameterTypes)))
	     ::,(let ((return (method:getReturnType)))
		  (string->symbol
		   (return:getName)))))
	 methods)))

(define (direct-fields object)::list
  (let* ((class ::java.lang.Class (object:getClass))
	 (fields (class:getDeclaredFields)))
    (map (lambda (field::java.lang.reflect.Field)
	   (let ((type (field:getType)))
	     `(,(string->symbol (field:getName))
	       ::,(string->symbol (field:getName))
	       ,(field:get object))))
	 fields)))
