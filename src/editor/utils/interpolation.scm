(module-name (editor utils interpolation))

(import (language infix))
(import (language define-type))

(define-alias Tween (maps (initial::real
			   final::real
			   progress::real)
			  to: real))

(define (sine-interpolation initial-value::real
			    final-value::real
			    progress::real)
  ::real
  (cond
   ((is progress <= 0) initial-value)
   ((is progress >= 1) final-value)
   (else
    (+ initial-value
       (* (- final-value initial-value)
	  (sin (* progress pi/2)))))))
