(define (list->symbol l)
  (string->symbol (list->string l)))

(define (with-output-to-string proc)
  (call-with-output-string
    (lambda (port)
      (parameterize ((current-output-port port))
	(proc)))))

(define (with-input-from-string s proc)
  (call-with-input-string s
    (lambda (port)
      (parameterize ((current-input-port port))
	(proc)))))

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define (digit->char digit)
  (integer->char (+ digit (char->integer #\0))))
