(module-name (editor document universe))

(import (language define-type))
(import (language define-object))
(import (language define-parameter))
(import (language attributes))
(import (language infix))
(import (language match))
(import (language mapping))
(import (srfi :11))
(import (utils functions))
(import (language fundamental))
(import (editor interfaces elements))
(import (editor types spaces))
(import (editor types primitive))
(import (language define-cache))
(import (utils conversions))
(import (editor document parse))
(import (utils print))
(import (editor document history-tracking))
(import (editor input screen))

(define open-documents ::(list-of Document)
  '())

(define (load-document-from-port port::gnu.kawa.io.InPort source)
  ::Document
  (or (find (lambda (document::Document)
	      ::boolean
	      (eq? document:source source))
	    open-documents)
      (let ((document (Document (parse port) source)))
	(set! open-documents 
	      (cons document open-documents))
	document)))

(define (open-document-file source::java.io.File)::Document
  (or (find (lambda (document::Document)
	      ::boolean
	      (eq? document:source source))
	    open-documents) 
      (call-with-input-file (source:getAbsolutePath)
	(lambda (port)
	  (let ((document (Document (parse port) source)))
	    (set! open-documents 
		  (cons document open-documents))         
	    document)))))

(define (new-document)::Document
  (call-with-input-string ""
    (lambda (port)
      (let ((document (Document (parse port)
				(java.time.LocalDateTime:now))))
	(set! open-documents 
	      (cons document open-documents))         
	document))))

(define-attribute (last-save-point document)::list
  '())

(define (save-document! document::Document
			file::java.io.File)
  ::void
  (and-let* ((source ::java.io.File document:source)
	     ((string=? (source:getCanonicalPath)
			(file:getCanonicalPath)))
	     (document-history ::History (history document))
	     (`(,front . ,_) document-history:fronts))
    (set! (last-save-point document) front))
  (call-with-output-file (file:getAbsolutePath)
    (lambda (port)
      (parameterize ((current-output-port port))
	(show-document document))))
  (set! document:source file))

(define (document-saved? document::Document)::boolean
  (let ((document-history ::History (history document)))
    (or (null? document-history:fronts)
	(and-let* ((`(,front . ,_) document-history:fronts))
	  (eq? front (last-save-point document))))))

(define (shut-document! document::Document)
  ::void
  (screen:close-document! document)
  (set! open-documents
    (only (isnt _ eq? document) open-documents)))
