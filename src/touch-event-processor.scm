(import (define-interface))
(import (define-object))
(import (match))
(import (infix))
(import (pane))
(import (functions))
(import (postponed))
(import (print))

(define-interface VelocityTracker ()
  (update! position::float time-step/ms::float)::void
  (current-velocity)::float
  (reset!)::void
  )

(define-object (TouchEventProcessor finger::byte
				    target::Screen
				    run::Postponed)
  
  (define (distance x1::real y1::real x2::real y2::real)::real
    (hypotenuse (- x2 x1) (- y2 y1)))
  
  (define x0 ::real +nan.0)
  (define y0 ::real +nan.0)

  (define x- ::real +nan.0)
  (define y- ::real +nan.0)

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
	(set! dx (- x x-))
	(set! dy (- y y-))
	(set! vx (/ dx delta-ms))
	(set! vy (/ dy delta-ms))
	(set! x- x)
	(set! y- y)
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
    (set! x- x)
    (set! y- y)
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
		  (WARN "invoking long press")
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
  )
