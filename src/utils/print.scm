(module-name (utils print))

(import (kawa regex))
(import (language define-syntax-rule))
(import (language define-interface))
(import (language define-type))
(import (language define-object))
(import (language define-parameter))
(import (language for))
(import (language infix))
(import (language match))
(import (utils conversions))
(import (utils functions))
(import (language fundamental))

(define-parameter (current-display-procedure)::procedure
  display)

(define-syntax-rule (print elements ...)
  ((current-display-procedure) elements)
  ...
  ((current-display-procedure) #\newline)
  (force-output))

(define-interface MessageHandler ()
  (clear-messages!)::void
  (add-message message::list)::void
  (display-messages output::Object)::void)

(define-object (logger size::int)::MessageHandler
  (define messages ::list '())

  (define historyLength ::int)

  (define (clear-messages!)::void
    (set! messages '()))
  
  (define (add-message message::list)::void
    (let ((message-string (with-output-to-string
			    (lambda ()
			      (display "\n")
			      (for word in message
				(display word))
			      ))))
      (set! messages `(,message-string . ,messages))
      (drop-after! historyLength messages)))
  
  (define (display-messages output::Object)::void
    #!abstract)

  (set! historyLength size))

(define-object (direct-message-handler)::MessageHandler
  (define (add-message message::list)::void
    (for word in message
      (display word))
    (newline)
    (flush-output-port))
  
  (define last-message::string "")

  (define (clear-messages!)::void
    (values))

  (define (display-messages output::Object)::void
    (values)))

(define-object (ignoring-message-handler)::MessageHandler
  (define (add-message message::list)::void
    (values))
  
  (define last-message::string "")

  (define (clear-messages!)::void
    (values))

  (define (display-messages output::Object)::void
    (values)))

(define-object (recording-message-handler)::MessageHandler
  (define last-message::string)

  (define (clear-messages!)::void
    (set! last-message #!null))
  
  (define (add-message message::list)::void
    (set! last-message
	  (with-output-to-string
	    (lambda ()
	      (for word in message
		(display word))
	      (newline)))))
  
  (define (display-messages output::Object)::void
    (display last-message)
    (set! last-message #!null)))

(define-parameter (current-message-handler)::MessageHandler
  (direct-message-handler))

(define (WARN . args)
  (invoke (current-message-handler) 'add-message args))

(define-syntax dumping-variables
  (syntax-rules ()
    ((_ finally () output)
     (finally . output))

    ((_ finally (var . vars) (output ...))
     (dumping-variables finally vars (output ... 'var ": "var)))))

(define-syntax-rule (DUMP expr ...)
  (dumping-variables WARN (expr ...) ()))

(define-syntax-rule (truly actions ...)
  (begin actions ... #t))

(define-syntax-rule (falsely actions ...)
  (begin actions ... #f))

(define-syntax-rule (nully actions ...)
  (begin actions ... #!null))

(define (stack-trace ex::java.lang.Throwable)
  (let* ((sw ::java.io.StringWriter (java.io.StringWriter))
	 (pw ::java.io.PrintWriter (java.io.PrintWriter sw)))
    (ex:printStackTrace pw)
    (sw:toString)))

(define-syntax-rule (current-stack-trace)
  (try-catch
   (throw (java.lang.Exception))
   (ex java.lang.Throwable
       (stack-trace ex))))

(define-parameter (stack-dump-length)::integer 5)

(define-parameter (show-in-stack-dump?)::(maps (string)
					       to: boolean)
  (lambda (line)
    (and ;;(isnt ".java:[0-9]+" regex-match line)
	 (isnt "at gnu" regex-match line)
	 (isnt "at android.view.ViewRootImpl" regex-match line)
	 (isnt "Native Method" regex-match line))))

(define (log-stack-trace! trace::string)::void
  (for line in (take (stack-dump-length)
		     (only (show-in-stack-dump?)
			   (string-split trace
					 "\n")))
    (WARN line)))

(define-parameter (stack-trace-action)
  ::(maps (string) to: void)
  log-stack-trace!)

(define-syntax-rule (safely actions ...)
  (try-catch
   (begin actions ...)
   (ex java.lang.Throwable
       ((stack-trace-action) (stack-trace ex))
       #!null)))

(define (try action ::(maps () to: ,result))
  ::(either ,result java.lang.Throwable)
  (try-catch (action) (ex java.lang.Throwable ex)))

(define-parameter (debugging?) #f)

(define (as-string object)::String
  (if (eq? object #!null)
      "#!null"
      (object:toString)))

(define (DEBUG . msg)
  (when (debugging?)
    (apply WARN msg)))

(define-syntax-rule (DEBUG-DUMP expr ...)
  (when (debugging?)
    (DUMP expr ...)))
