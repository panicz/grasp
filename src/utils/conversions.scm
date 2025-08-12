(module-name (utils conversions))

(define (list->symbol l)
  (string->symbol (list->string l)))

(define (with-output-to-string proc)
  (call-with-output-string
    (lambda (port)
      (parameterize ((current-output-port port))
	(proc)))))

(define (with-output-to-port port proc)
  (parameterize ((current-output-port port))
    (proc)))

(define (with-input-from-string s proc)
  (call-with-input-string s
    (lambda (port)
      (parameterize ((current-input-port port))
	(proc)))))

(define (with-input-from-port port proc)
  (parameterize ((current-input-port port))
    (proc)))

(define (keyword->symbol kw)
  (string->symbol (keyword->string kw)))

(define (symbol->keyword s)
  (string->keyword (symbol->string s)))

(define (digit->char digit)
  (integer->char (+ digit (char->integer #\0))))

(define (inverse function)
  (procedure-property function 'inverse))

(define (listify item)
  (if (list? item)
      item
      (list item)))

(set! (setter inverse)
      (lambda (function value)
	(set! (procedure-property function 'inverse) value)))

(define (read-u32le #!optional (port (current-input-port)))::uint
  (let* ((b0 ::ubyte (read-u8))
	 (b1 ::ubyte (read-u8))
	 (b2 ::ubyte (read-u8))
	 (b3 ::ubyte (read-u8)))
    (as uint (bitwise-ior
		b0
		(arithmetic-shift b1 8)
		(arithmetic-shift b2 16)
		(arithmetic-shift b3 24)))))

(define (read-u16le #!optional (port (current-input-port)))::ushort
  (let* ((b0 ::ubyte (read-u8))
	 (b1 ::ubyte (read-u8)))
    (as uint (bitwise-ior b0 (arithmetic-shift b1 8)))))

(define (bytevector-u32le-ref b::bytevector i::integer)::uint
  (as uint (bitwise-ior
	    (b i)
	    (arithmetic-shift (b (+ i 1)) 8)
	    (arithmetic-shift (b (+ i 2)) 16)
	    (arithmetic-shift (b (+ i 3)) 24))))

(define (bytevector-u32le-set! b::bytevector i::integer v::uint)::void
  (set! (b i) (as byte (bitwise-and #xff v)))
  (set! (b (+ i 1)) (as byte (bitwise-and #xff (arithmetic-shift v -8))))
  (set! (b (+ i 2)) (as byte (bitwise-and #xff (arithmetic-shift v -16))))
  (set! (b (+ i 3)) (as byte (bitwise-and #xff (arithmetic-shift v -24)))))

(define (little-endian-bytes-u32 number::uint)::list ;of ubyte
  `(,(as ubyte (bitwise-and number #xFF))
    ,(as ubyte (bitwise-and (arithmetic-shift number -8) #xFF))
    ,(as ubyte (bitwise-and (arithmetic-shift number -16) #xFF))
    ,(as ubyte (bitwise-and (arithmetic-shift number -24) #xFF))))

(define (little-endian-bytes-u16 number::uint)::list ;of ubyte
  `(,(as ubyte (bitwise-and number #xFF))
    ,(as ubyte (bitwise-and (arithmetic-shift number -8) #xFF))))
