(define-syntax for
  (syntax-rules (in from to below by ::)

    ((_ var :: type in collection . actions)
     (for-each (lambda (var :: type) . actions) collection))

    ((_ var in collection . actions)
     (for-each (lambda (var) . actions) collection))

    ((_ var from start to end by increment actions ...)
     (let loop ((var start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))

    ((_ var from start below end by increment actions ...)
     (let loop ((var start))
       (if (< var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))

    ((_ var from start to end actions ...)
     (let loop ((var start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var 1))))))

    ((_ var from start below end actions ...)
     (let loop ((var start))
       (if (< var end)
	   (begin
	     actions ...
	     (loop (+ var 1))))))
    ))
