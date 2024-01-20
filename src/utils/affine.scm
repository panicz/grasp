(module-name (utils affine))

(import (srfi :1))
(import (srfi :11))
(import (utils fixnum))

(define-syntax assertion-violation
  (syntax-rules ()
    ((_ . content)
     (error . content))))

(define-syntax endianness
  (syntax-rules (little big)
    ((_ little) 'little-endian)
    ((_ big) 'big-endian)))

(define-syntax xbyte-ref
  (lambda (x)
    (syntax-case x ()
      ((_ b n)
       (with-syntax ((shift (* (syntax->datum #'n) 8)))
         (if (zero? (syntax->datum #'n))
             #'(bitwise-and #xff b)
             #'(bitwise-and #xff (bitwise-arithmetic-shift-right b shift))))))))

(define (bytevector-u32-set! bv index value endian)
  (let ((b0 (xbyte-ref value 0))
        (b1 (xbyte-ref value 1))
        (b2 (xbyte-ref value 2))
        (b3 (xbyte-ref value 3)))
    (cond
     ((eq? endian 'big-endian)
      (bytevector-u8-set! bv index b3)
      (bytevector-u8-set! bv (+ index 1) b2)
      (bytevector-u8-set! bv (+ index 2) b1)
      (bytevector-u8-set! bv (+ index 3) b0))
     (else
      (bytevector-u8-set! bv index b0)
      (bytevector-u8-set! bv (+ index 1) b1)
      (bytevector-u8-set! bv (+ index 2) b2)
      (bytevector-u8-set! bv (+ index 3) b3)))))


(define (bytevector-u32-ref bv index endian)
  (let ((b0 (bytevector-u8-ref bv index))
        (b1 (bytevector-u8-ref bv (+ index 1)))
        (b2 (bytevector-u8-ref bv (+ index 2)))
        (b3 (bytevector-u8-ref bv (+ index 3))))
    (if (eq? endian 'big-endian)
        (bitwise-ior (bitwise-arithmetic-shift-left b0 24)
                     (bitwise-arithmetic-shift-left b1 16)
                     (bitwise-arithmetic-shift-left b2 8)
                     b3)
        (bitwise-ior (bitwise-arithmetic-shift-left b3 24)
                     (bitwise-arithmetic-shift-left b2 16)
                     (bitwise-arithmetic-shift-left b1 8)
                     b0))))

(define alog
  (do ((alog (make-bytevector 256))
       (p 1 (let ((p (fxxor p (fxarithmetic-shift-left p 1))))
              (if (fxbit-set? p 8)
                  (fxxor p #b100011011) ;subtract X⁸+X⁴+X³+X+1
                  p)))
       (i 0 (+ i 1)))
      ((= i 256)
       (lambda (i) (bytevector-u8-ref alog i)))
    (bytevector-u8-set! alog i p)))

(define ilog                          ;called `log' in [Win96afast]
  (do ((ilog (make-bytevector 256))
       (i 0 (+ i 1)))
      ((= i 256)
       (lambda (i) (bytevector-u8-ref ilog i)))
    (bytevector-u8-set! ilog (alog i) i)))

(define (GF* a b)
  (if (or (zero? a) (zero? b))
      0
      (alog (mod (+ (ilog a) (ilog b)) 255))))

(define (GFexpt a n)
  (if (zero? n) 1
      (GF* a (GFexpt a (- n 1)))))

(define (GFinv a)
  (if (zero? a)
      0
      (alog (mod (- (ilog a)) 255))))

;; What follows is from Rijndael

(define (affine-transform b)
  (define (bit x i)
    (fxbit-field x i (+ i 1)))
  (do ((c #b01100011)
       (i 0 (+ i 1))
       (tmp 0 (fxior (fxarithmetic-shift-left
                      (fxxor (bit b i)
                             (bit b (mod (+ i 4) 8))
                             (bit b (mod (+ i 5) 8))
                             (bit b (mod (+ i 6) 8))
                             (bit b (mod (+ i 7) 8))
                             (bit c i))
                      i)
                     tmp)))
      ((= i 8) tmp)))

(define S-box                         ;for SubBytes
  (let ((S (make-bytevector 256)))
    (do ((i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref S i)))
      (bytevector-u8-set! S i (affine-transform (GFinv i))))
    S))

(define inv-S-box                     ;for InvSubBytes
  (let ((invS (make-bytevector 256)))
    (do ((i 0 (+ i 1)))
        ((= i 256)
         (lambda (i) (bytevector-u8-ref invS i)))
      (bytevector-u8-set! invS (affine-transform (GFinv i)) i))
    invS))

(define-syntax copy-byte
  (lambda (x)
    (syntax-case x ()
      ((_ b n)
       (with-syntax ((mask (bitwise-arithmetic-shift-left #xff (* (syntax->datum #'n) 8))))
         #'(bitwise-and mask b))))))

(define (uncat l n)
  (if (null? l)
      l
      (let-values (((this next) (split-at l n)))
        (cons this (uncat next n)))))

(define (rnrs-bytevector-copy! source source-start target target-start k)
  (bytevector-copy! target target-start source source-start (+ source-start k)))

;; The math for these tables is in the private library.
(define-syntax rcon-table
  (lambda (x)
    (syntax-case x ()
      ((_ n)
       (with-syntax ((tab (list->vector
                           (map (lambda (i)
                                  (bitwise-arithmetic-shift-left
                                   (GFexpt 2 i) 24))
                                (iota (syntax->datum #'n))))))
         #''tab)))))

(define-syntax aes-table
  (lambda (x)
    (define (sbox-table t e0 e1 e2 e3)
      (define (table-entry i)
        (bitwise-ior (bitwise-arithmetic-shift-left
                      (GF* (syntax->datum e0) (t i)) 24)
                     (bitwise-arithmetic-shift-left
                      (GF* (syntax->datum e1) (t i)) 16)
                     (bitwise-arithmetic-shift-left
                      (GF* (syntax->datum e2) (t i)) 8)
                     (GF* (syntax->datum e3) (t i))))
      (list->vector (map table-entry (iota 256))))
    (syntax-case x (S invS)
      ((_ S e0 e1 e2 e3)
       (with-syntax ((tab (sbox-table S-box #'e0 #'e1 #'e2 #'e3)))
         #''tab))
      ((_ invS e0 e1 e2 e3)
       (with-syntax ((tab (sbox-table inv-S-box #'e0 #'e1 #'e2 #'e3)))
         #''tab)))))
