(module-name (utils binary))

(import (language define-type))
(import (language examples))
(import (utils functions))

(define-alias Byte integer)

(define (digits/base base::integer n::integer)::(list-of integer)
  (map (lambda (n)
	 (modulo n base))
       (unfold-right-until zero? (lambda (n)
				   (quotient n base))
			   n)))

(e.g.
 (digits/base 2 4) ===> (1 0 0))

(e.g.
 (digits/base 10 140) ===> (1 4 0))

(define (number/base base::integer digits::(list-of integer))::integer
  (fold-left (lambda (result factor power)
	       (+ result (* factor (expt base power))))
	     0
	     digits
	     (numbers from: (- (length digits) 1) to: 0 by: -1)))

(e.g.
 (number/base 2 '(1 0 0)) ===> 4)

(e.g.
 (number/base 10 '(1 0 0)) ===> 100)

(define (number/base* base::integer . digits)::integer
  (fold-left (lambda (result factor power)
	       (+ result (* factor (expt base power))))
	     0
	     digits
	     (numbers from: (- (length digits) 1) to: 0 by: -1)))

(e.g.
 (number/base* 10 1 2 3) ===> 123)

(define (number->hex number::int)::string
  (format #f "~2,'0x" number))

(define (hex->number s::string)::int
  (string->number s 16))

(define (list->hex l::list)::string
  (string-join (map number->hex l) " "))

(define (hex->list s::string)::list
  (map hex->number (string-split s " ")))

(define (unsigned-little-endian bytes value)
  (let ((cells (reverse (digits/base 256 value))))
    (extend-right cells #;to bytes #;with 0)))

(define (unsigned-little-endian-16 value)
  (unsigned-little-endian 2 value))

(define (unsigned-little-endian-32 value)
  (unsigned-little-endian 4 value))

(define (unsigned-little-endian-64 value)
  (unsigned-little-endian 8 value))

(define (little-endian bytes value)
  (if (negative? value)
      (unsigned-little-endian bytes (+ value (expt 2 (* 8 bytes))))
      (unsigned-little-endian bytes value)))

(define (little-endian-16 value)
  (little-endian 2 value))

(define (little-endian-32 value)
  (little-endian 4 value))

(define (little-endian-64 value)
  (little-endian 8 value))

(define (unsigned-big-endian bytes value)
  (extend-left (digits/base 256 value) #;to bytes #;with 0))

(define (unsigned-big-endian-16 value)
  (unsigned-big-endian 2 value))

(define (unsigned-big-endian-32 value)
  (unsigned-big-endian 4 value))

(define (unsigned-big-endian-64 value)
  (unsigned-big-endian 8 value))

(define (big-endian bytes value)
  (if (negative? value)
      (unsigned-big-endian bytes (+ value (expt 2 (* 8 bytes))))
      (unsigned-big-endian bytes value)))

(define (big-endian-16 value)
  (big-endian 2 value))

(define (big-endian-32 value)
  (big-endian 4 value))

(define (big-endian-64 value)
  (big-endian 8 value))

(e.g. (little-endian 2 119) ===> (119 0))

(e.g. (little-endian 2 -119) ===> (137 255))

(e.g. (little-endian-64 (+ 256 119)) ===> (119 1 0 0 0 0 0 0))

(e.g. (little-endian-32 (+ 256 119)) ===> (119 1 0 0))

(e.g. (big-endian-32 (+ 256 119)) ===> (0 0 1 119))

(define (8-bits number)
  (extend-left (digits/base 2 number) #;to 8 #;with 0))

(define (16-bits number)
  (concatenate (map 8-bits (big-endian 2 number))))

(define (32-bits number)
  (concatenate (map 8-bits (big-endian 4 number))))

(define (64-bits number)
  (concatenate (map 8-bits (big-endian 8 number))))

(e.g. (8-bits 119) ===> (0 1 1 1 0 1 1 1))

(e.g. (16-bits (* 119 256)) ===> (0 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0))


(e.g. (map 8-bits (little-endian 2 311))
      ===> ((0 0 1 1 0 1 1 1) (0 0 0 0 0 0 0 1)))

(e.g. (map 8-bits (little-endian 2 -312))
      ===> ((1 1 0 0 1 0 0 0) (1 1 1 1 1 1 1 0)))

(e.g. (map 8-bits (unsigned-little-endian 2 -312))
      ===> ((1 1 0 0 1 0 0 0) (1 1 1 1 1 1 1 1)))

(e.g. (map 8-bits (big-endian 2 311))
      ===> ((0 0 0 0 0 0 0 1) (0 0 1 1 0 1 1 1)))

(e.g. (map 8-bits (big-endian 2 -312))
      ===> ((1 1 1 1 1 1 1 0) (1 1 0 0 1 0 0 0)))

(e.g. (map 8-bits (unsigned-big-endian 2 -312))
      ===> ((1 1 1 1 1 1 1 1) (1 1 0 0 1 0 0 0)))
