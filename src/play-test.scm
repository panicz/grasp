(import (kawa regex))
(import (only (srfi :1) partition span break first second))
(import (srfi :11))
(import (io))
(import (functions))
(import (infix))
(import (match))
(import (hash-table))
(import (mapping))
(import (for))
;;(import (term))


(define (matching string pattern)
  (regex-match pattern string))

(define files '())
(define options '())

(define step-time/seconds 1)
(define interactive? #f)

(match (command-line)
  (`(,command . ,args)
   (let-values (((command-line-options given-files)
		 (partition (is _ matching "^--") args)))
     (set! options command-line-options)
     (set! files given-files))))

(and-let* ((`(,_ ,time)
	    (any (is _ matching "^--step-time=\([0-9]+[.]?[0-9]*\)$")
		 options)))
  (set! step-time/seconds (string->number time)))

(when (any (is _ equal? "--interactive") options)
  (set! interactive? #t))
  
(define contents
  (map (lambda (file)
	 (let ((expressions (with-input-from-file file read-all)))
	   (let-values (((_prefix contents)
			 (break (lambda (e)
				  (and-let* ((`(snapshot ,screen) e))))
				expressions)))
	     `(,file . ,contents))))
       files))


(for entry in contents
  (display (car entry))
  (newline)
  (let loop ((expressions (cdr entry)))
    (when (isnt expressions null?)
      (match expressions
	(`((snapshot ,screen) . ,rest)
	 (display screen)
	 (newline)
	 (sleep step-time/seconds)
	 (let-values (((operations next)
		       (break (lambda (e)
				(and-let* ((`(snapshot ,screen)
					    e))))
			      rest)))
	   #;(for operation in operations
	     (write operation))
	   ;(newline)
	   ;;(sleep step-time/seconds)
	   (loop next)))))))
