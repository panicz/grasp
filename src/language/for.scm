(module-name (language for))
(import (language while))

(define (par-for-each function collection)
  (let ((futures ::java.util.List
		 (java.util.ArrayList)))
    (for-each (lambda (x)
		(futures:add (future (function x))))
	      collection)
    (for-each (lambda (f)
		(force f))
	      futures)
    (futures:clear)))

(define-syntax for
  (syntax-rules (in from to below by
		    in-reverse
		    in-parallel ::)

    ((_ var :: type in-reverse collection . actions)
     (let ((it ::java.util.ListIterator (collection:listIterator
					 (length collection))))
       (while (it:hasPrevious)
	 (let ((var ::type (it:previous)))
	   . actions))))

    ((_ var in-reverse collection . actions)
     (let ((it ::java.util.ListIterator (collection:listIterator
					 (length collection))))
       (while (it:hasPrevious)
	 (let ((var (it:previous)))
	   . actions))))

    ((_ var :: type in-parallel collection . actions)
     (par-for-each (lambda (var :: type) . actions) collection))

    ((_ var in-parallel collection . actions)
     (par-for-each (lambda (var) . actions) collection))
    
    ((_ var :: type in collection . actions)
     (for-each (lambda (var :: type) . actions) collection))

    ((_ var in collection . actions)
     (for-each (lambda (var) . actions) collection))

    ((_ var::type from start to end by increment actions ...)
     (let loop ((var::type start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))
    
    ((_ var from start to end by increment actions ...)
     (let loop ((var start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))

    ((_ var::type from start below end by increment actions ...)
     (let loop ((var start))
       (if (< var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))
    
    ((_ var from start below end by increment actions ...)
     (let loop ((var start))
       (if (< var end)
	   (begin
	     actions ...
	     (loop (+ var increment))))))

    ((_ var::type from start to end actions ...)
     (let loop ((var start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var 1))))))
    
    ((_ var from start to end actions ...)
     (let loop ((var start))
       (if (<= var end)
	   (begin
	     actions ...
	     (loop (+ var 1))))))

    ((_ var::type from start below end actions ...)
     (let loop ((var start))
       (if (< var end)
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
