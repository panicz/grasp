(module-name (editor input touch-event-processor))

(import (language define-interface))
(import (language define-object))
(import (language match))
(import (language infix))
(import (language assert))
(import (editor interfaces elements))
(import (editor input input))
(import (utils functions))
(import (editor interfaces delayed))
(import (editor interfaces painting))
(import (utils print))

(define-object (TouchEventProcessor finger::byte
				    target::Screen
				    run::Postponed)
  
  (define (distance x1::real y1::real x2::real y2::real)::real
    (hypotenuse (- x2 x1) (- y2 y1)))

  (define last-known-position ::Position
    (last-known-pointer-position finger))
  
  (define x0 ::real +nan.0)
  (define y0 ::real +nan.0)

  (define vx ::float 0.0)
  (define vy ::float 0.0)

  (define dx ::real +nan.0)
  (define dy ::real +nan.0)
  
  (define vicinity ::real 1.0)
  
  (define suppressed-presses ::byte 0)
  
  (define press-time-ms ::real -inf.0)
  
  (define release-time-ms ::real -inf.0)

  (define move-time-ms ::real -inf.0)

  (define long-press-time-ms ::real 700)

  (define double-tap-timeout-ms ::real 350)
  
  (define timeout ::Cancellable cancellable-nothing)

  (define (move! x::real y::real time-ms::real)::boolean
    (cond
     ((zero? suppressed-presses)
      (set! timeout (timeout:cancel))
      (let ((delta-ms ::real (- time-ms move-time-ms)))
	(set! dx (- x last-known-position:left))
	(set! dy (- y last-known-position:top))
	(set! vx (/ dx delta-ms))
	(set! vy (/ dy delta-ms))
	(set! last-known-position:left x)
	(set! last-known-position:top y)
	(set! move-time-ms time-ms)
	(target:move! finger x y dx dy)))
     
     ((is (distance x0 y0 x y) > vicinity)
      (set! timeout (timeout:cancel))
      (let ((suppressed ::int suppressed-presses))
	(set! suppressed-presses 0)
	(begin/or
	 (if (is suppressed >= 2)
	     (target:second-press! finger x0 y0)
	     (target:press! finger x0 y0))
	 (target:move! finger x0 y0 0 0)
	 (target:move! finger x y (- x x0) (- y y0)))))
     
     (else
      #f)))

  (define (press! x::real y::real time-ms::real)::boolean
    (set! timeout (timeout:cancel))
    (set! last-known-position:left x)
    (set! last-known-position:top y)
    (set! vx 0)
    (set! vy 0)
    (set! press-time-ms time-ms)
    (set! move-time-ms time-ms)

    (match suppressed-presses
      (0 (set! suppressed-presses 1)
	 (set! x0 x)
	 (set! y0 y)
	 (set! timeout
	       (run:after
		long-press-time-ms
		(lambda ()
		  (set! suppressed-presses 0)
		  ;; for some reason, the diagnostic message
		  ;; here seems to solve the problem :/
		  ;; (to be investigated)
		  ;;(WARN "invoking long press")
		  (target:long-press! finger x0 y0))))
	 #f)
      (1 (cond
	  ((is (distance x0 y0 x y) <= vicinity)
	   (set! suppressed-presses 2)
	   #f)
	  (else
	   (let ((x0- x0)
		 (y0- y0))
	     (set! x0 x)
	     (set! y0 y)
	     (target:tap! finger x0- y0-)))))))

  (define (release! x::real y::real time-ms::real)::boolean
    (set! timeout (timeout:cancel))
    ;;(WARN "vx: "vx", vy: "vy)
    (match suppressed-presses
      (0 (target:release! finger x y vx vy))
      (1 (set! timeout
	       (run:after
		double-tap-timeout-ms
		(lambda ()
		  (set! suppressed-presses 0)
		  (target:tap! finger x0 y0))))
	 #f)
      (2 (set! suppressed-presses 0)
	 (target:double-tap! finger x y))))

  #;(assert (is finger < (length last-known-pointer-position)))
  )
