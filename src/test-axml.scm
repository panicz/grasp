#!/bin/sh 
#|
exec java -jar ../libs/kawa.jar -f "$0" $*
|#

(import (kawa pprint))
(import (language assert))
(import (language define-syntax-rule))
(import (language define-parameter))
(import (language define-interface))
(import (language for))
(import (language fundamental))
(import (language define-object))
(import (language define-type))
(import (language infix))
(import (language match))
(import (language while))
(import (language mapping))
(import (utils print))
(import (utils conversions))
(import (utils functions))
(import (utils build))

(define (parse-axml file-name)::AndroidXML
  (fluid-let ((port-char-encoding #f))
    (let ((port (open-input-file file-name)))
      (parameterize ((current-input-port port))
	(let ((document ::AndroidXML (AndroidXML)))
	  (document:parse port)
	  (close-port port)
	  document)))))


(match (command-line)
  (`(,_ ,file-name)
   (with ((*print-base* 16))
     (fluid-let ((port-char-encoding #f))
       (let* ((parsed ::AndroidXML (parse-axml file-name))
	      (serialized (parsed:serialize))
	      (manifest (AndroidManifest))
	      (port (open-input-file file-name))
	      (original (read-bytevector (length serialized)
					 port)))
	 (close-port port)
	 (print (equal? serialized (u8vector->list original))
		(equal? serialized (manifest:serialize)))
	 
	 #;(print parsed )))))

  (`(,this . ,_)
   (print "Usage: "this" <axml-file>")))


