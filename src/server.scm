(import (language while))
(import (language for))

(define (tcp-output-server port::short)::gnu.kawa.io.OutPort
  (let* ((server ::java.net.ServerSocket
		 (java.net.ServerSocket port))
	 (connections ::list '())
	 (thread (future
		  (while #t
		    (let ((socket ::java.net.Socket (server:accept)))
		      (set! connections
			    (cons (socket:getOutputStream)
				  connections)))))))
    (gnu.kawa.io.OutPort
     (object (java.io.OutputStream)
       ((write byte::int)::void
	(for connection::java.io.OutputStream in connections
	  (connection:write byte)
	  (when (= byte (char->integer #\newline))
	    (connection:flush))))))))
