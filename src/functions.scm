(import (assert))
(import (match))
(import (examples))
(import (infix))

(define (times n::int action . args)
  (when (is n > 0)
    (apply action args)
    (apply times (- n 1) action args)))

(define-early-constant head car)

(define-early-constant tail cdr)

(define (drop k::integer #;elements-from s::list)::list
  (if (and (pair? s)
	   (> k 0))
      (let loop ((result (cdr s))
		 (k (- k 1)))
	(if (or (<= k 0) (null? result))
	    result
	    (loop (cdr result) (- k 1))))
      s))

(e.g.
 (drop 2 (list 1 2 3))
 ===> (3))

(define (drop-after! k::integer #;elements-in s::list)::list
  (define (lastmost-tail n::integer l::list)
    (if (or (is n <= 1) (isnt l pair?))
	l
	(lastmost-tail (- n 1) (cdr l))))
  (let ((trail (lastmost-tail k s)))
    (when (pair? trail)
      (set! (cdr trail) '())))
  s)


(e.g.
 ;; if the input is a pair, then the output must also be a pair
 ;; (we cannot return an empty list)
 (let ((items (list 'a 'b 'c)))
   (drop-after! 0 items))
 ===> (a))

(e.g.
 ;; but if the input is not a pair, we get it back no problem
 (drop-after! 100 '()) ===> ())

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 1 items))
 ===> (a))

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 2 items))
 ===> (a b))

(e.g.
 (let ((items (list 'a 'b 'c)))
   (drop-after! 5 items))
 ===> (a b c))

(define (take k::integer #;elements-from s::list)::list
  (if (and (pair? s)
	   (> k 0))
      (let ((result (cons (car s) '())))
	(let loop ((input (cdr s))
		   (tip result)
		   (k (- k 1)))
	  (if (or (<= k 0) (null? input))
	      result
	      (begin
		(set! (cdr tip) (cons (car input) (cdr tip)))
		(loop (cdr input) (cdr tip) (- k 1))))))
      s))

(e.g.
 (take 3 #;elements-from '(1 2 3 4 5))
 ===> (1 2 3))

(define (suffix? ending::list stem::list)::boolean
  (let ((m ::integer (length ending))
        (n ::integer (length stem)))
    (and (is m <= n)
         (let ((r ::integer (- n m)))
           (equal? (drop r stem) ending)))))

(e.g.
 (is '(4 5) suffix? '(1 2 3 4 5)))

(define predicate procedure)

(define (any satisfying? elements)
  (and-let* ((`(,first . ,rest) elements))
    (or (satisfying? first)
	(any satisfying? rest))))

(e.g.
 (any even? '(1 2 3)))

(define (every satisfying? elements)
  (or (null? elements)
      (and-let* ((`(,first . ,rest) elements))
	(and (satisfying? first)
	  (every satisfying? rest)))))

(e.g.
 (every even? '(2 4 6)))

(define (fold-left f x0 . xs*)
  (define (fold-left1 f x0 xs)
    (if (null? xs)
	x0
	(fold-left1 f (f x0 (car xs)) (cdr xs))))

  (define (fold-left2 f x0 xs xs2)
    (if (or (null? xs) (null? xs2))
	x0
	(fold-left2 f (f x0 (car xs) (car xs2)) (cdr xs) (cdr xs2))))

  (define (fold-left3 f x0 xs xs2 xs3)
    (if (or (null? xs) (null? xs2) (null? xs3))
	x0
	(fold-left3 f (f x0 (car xs) (car xs2) (car xs3)) (cdr xs) (cdr xs2) (cdr xs3))))

  (define (fold-left* f x0 . xs*)
    (if (any null? xs*)
	x0
	(apply fold-left* f (apply f x0 (map car xs*)) (map cdr xs*))))
  (cond
   ((null? xs*) x0)
   ((null? (cdr xs*)) (fold-left1 f x0 (car xs*)))
   ((null? (cddr xs*)) (fold-left2 f x0 (car xs*) (cadr xs*)))
   ((null? (cdddr xs*)) (fold-left3 f x0 (car xs*) (cadr xs*) (caddr xs*)))
   (else (apply fold-left* f x0 xs*))))

(define (fold-right f x0 . xs*)
  (define (fold-right1 f x0 xs)
    (if (null? xs)
	x0
	(f (car xs) (fold-right1 f x0 (cdr xs)))))

  (define (fold-right2 f x0 xs xs2)
    (if (or (null? xs) (null? xs2))
	x0
	(f (car xs) (car xs2) (fold-right2 f x0 (cdr xs) (cdr xs2)))))

  (define (fold-right3 f x0 xs xs2 xs3)
    (if (or (null? xs) (null? xs2) (null? xs3))
	x0
	(f (car xs) (car xs2) (car xs3)
	   (fold-right3 f x0 (cdr xs) (cdr xs2) (cdr xs3)))))

  (define (fold-right* f x0 . xs*)
    (if (any null? xs*)
	x0
	(apply f (fold-right1
		  (lambda (x y)
		    (cons (car x) y))
		  (list (apply fold-right* f x0 (map cdr xs*)))
		  xs*))))
  (cond
   ((null? xs*) x0)
   ((null? (cdr xs*)) (fold-right1 f x0 (car xs*)))
   ((null? (cddr xs*)) (fold-right2 f x0 (car xs*) (cadr xs*)))
   ((null? (cdddr xs*)) (fold-right3 f x0 (car xs*) (cadr xs*) (caddr xs*)))
   (else (apply fold-right* f x0 xs*))))

(e.g.
 (fold-right (lambda (a b) `(,a + ,b)) 'e '(a b c d))
 ===> (a + (b + (c + (d + e)))))

(define (only cool? stuff)
  (match stuff
    ('()
     '())
    (`(,first . ,rest)
     (if (cool? first)
	 `(,first . ,(only cool? rest))
	 (only cool? rest)))))

(e.g.
 (only even? '(1 2 3 4 5 6))
 ===> (2 4 6))

(define (in element list)
  (any (is _ equal? element) list))

(define (union set . sets)
  (define (union a b)
    (fold-left (lambda (set element)
		 (if (is element in set)
		     set
		     `(,element . ,set)))
	       a b))
  (fold-left union set sets))

(e.g.
 (union '(a b c) '(b c d e))
 ===> (e d a b c))

(define (intersection set . sets)
  (define (intersection a b)
    (only (is _ in b) a))
  (fold-left intersection set sets))

(e.g.
 (intersection '(a b c) '(b c d) '(c d e))
 ===> (c))

(define (difference set . sets)
  (define (difference a b)
    (fold-left (lambda (set element)
		 (if (is element in set)
		     (only (isnt _ equal? element) set)
		     set))
	       a b))
  (fold-left difference set sets))

(e.g.
 (difference '(a b c) '(b c d))
 ===> (a))

(define (subset? a b)
  (every (is _ in b) a))

(e.g.
 (is '(a b) subset? '(b a c)))

(define (same-sets? a b)
  (and (is a subset? b)
       (is b subset? a)))

(e.g.
 (same-sets? '(a b c) '(b a c)))

(define (concatenate list)
  (apply append list))

(e.g.
 (concatenate '((a b) (c) (d e f)))
 ===> (a b c d e f))

(define (pass x . functions)
  (fold-left (lambda (x f) (f x)) x functions))

(define (last list::pair)
  (if (pair? (cdr list))
      (last (cdr list))
      (car list)))

(e.g.
 (last '(1 2 3)) ===> 3)

(define (last-pair list::pair)::pair
  (if (pair? (cdr list))
      (last-pair (cdr list))
      list))

(e.g.
 (last-pair '(1 2 3)) ===> (3 . ()))

(define-early-constant last-tail
  (let ()
    (define (set-last-tail! p::pair value)
      (if (pair? (cdr p))
	  (set-last-tail! (cdr p) value)
	  (set! (cdr p) value)))
    (define (last-tail p::pair)
      (if (pair? (cdr p))
	  (last-tail (cdr p))
	  (cdr p)))
    (set! (setter last-tail) set-last-tail!)
    last-tail))

(define (read-all #!optional (port (current-input-port)))
  (let ((first-expression (read port)))
    (if (eof-object? first-expression)
	'()
	(let ((result `(,first-expression)))
	  (define (read-into tail)
	    (let ((next-expression (read port)))
	      (cond ((eof-object? next-expression)
		     result)
		    (else
		     (set-cdr! tail `(,next-expression))
		     (read-into (cdr tail))))))
	  (read-into result)))))

(define (char-digit? c::char)::boolean
  (<= (char->integer #\0)
      (char->integer c)
      (char->integer #\9)))


(define (char-hex-digit? c::char)::boolean
  (or (char-digit? c)
      (<= (char->integer #\a)
	  (char->integer c)
	  (char->integer #\f))
      (<= (char->integer #\A)
	  (char->integer c)
	  (char->integer #\F))))

(define (char-hex-value c::char)::int
  (let ((code (char->integer c)))
    (cond ((char-digit? c)
	   (- code (char->integer #\0)))
	  ((<= (char->integer #\a) code (char->integer #\f))
	   (+ 10 (- code (char->integer #\a))))
	  (else
	   (assert (<= (char->integer #\A) code (char->integer #\F)))
	   (+ 10 (- code (char->integer #\A)))))))

(e.g.
 (char-hex-value #\f) ===> 15)

(define (nothing . _)::void (values))

(define (never . _)::boolean #f)

(define (always . _) ::boolean #t)

(define (negation proc)
  (lambda args
    (not (apply proc args))))

(define-alias hypotenuse java.lang.Math:hypot)

(define-constant pi/4 ::real (atan 1))

(define-constant pi/2 ::real (* 2 pi/4))

(define-constant pi ::real (* 2 pi/2))

(define-constant -pi/4 ::real (- pi/4))

(define-constant -pi/2 ::real (- pi/2))

(define-constant -pi ::real (- pi))

(define (square x::number)::number
  (* x x))

(define (fraction number::real)
  (- number (floor number)))

(define (min+max first . args)
  #;(assert (and (number? first)
	       (every number? args)))
  (let loop ((min first)
	     (max first)
	     (remaining args))
    (match remaining
      ('()
       (values min max))
      (`(,current . ,remaining)
       (cond ((is current < min)
	      (loop current max remaining))
	     ((is current > max)
	      (loop min current remaining))
	     (else
	      (loop min max remaining)))))))

(e.g.
 (min+max 5 4 6 3 7 2 8 1)
 ===> 1 8)

(define (argmin+argmax property element . elements)
  (let ((quality (property element)))
    (let next-trial ((winner element)
		     (looser element)
		     (mastery quality)
		     (failure quality)
		     (opponents elements))
      (if (null? opponents)
	  (values looser winner)
	  (let* ((rival (head opponents))
		 (quality (property rival)))
	    (cond ((is quality < failure)
		   (next-trial winner rival mastery quality 
			       (tail opponents)))
		  ((is quality > mastery)
		   (next-trial rival looser quality failure 
			       (tail opponents)))
		  (else
		   (next-trial winner looser mastery failure 
			       (tail opponents)))))))))

(e.g.
 (argmin+argmax length '(1 2) '(3) '(4 5 6))
 ===> (3) (4 5 6))

(define (numbers #!key
		 (from::real 0)
		 (to::real 0)
		 (by::real (if (> from to) -1 1)))  
  (if (or (and (> from to) (>= by 0))
	  (and (< from to) (<= by 0)))
      '()
      (let ((result (cons from '())))
	(let loop ((tip result)
		   (from (+ from by)))
	  (if (or (and (> from to) (>= by 0))
		  (and (< from to) (<= by 0)))
	      result
	      (begin
		(set! (cdr tip) (cons from (cdr tip)))
		(loop (cdr tip) (+ from by))))))))

(define (concatenate! list-of-lists)
  (if (null? list-of-lists)
      '()
      (if (null? (car list-of-lists))
	  (concatenate! (cdr list-of-lists))
	  (let* ((result (car list-of-lists)))
	    (let loop ((last-segment result)
		       (rest (cdr list-of-lists)))
	      (cond ((null? rest)
		     result)
		    ((null? (car rest))
		     (loop last-segment (cdr rest)))
		    (else
		     (set! (cdr (last-pair last-segment)) (car rest))
		     (loop (car rest) (cdr rest)))))))))

(define (append! . lists)
  (concatenate! lists))

(define (split! list #!key (at::int 1))
  (let loop ((input list)
	     (pivot at))
    (if (<= pivot 0)
	'()
	(if (= pivot 1)
	    (let ((suffix (cdr input)))
	      (set! (cdr input) '())
	      suffix)
	    (loop (cdr input) (- pivot 1))))))

(e.g.
 (let* ((l (list 1 2 3 4 5))
	(s (split! l at: 3)))
   (and (equal? l '(1 2 3))
	(equal? s '(4 5)))))

(define (paste! sublist::list
		#!key
		(into::list '())
		(at::int 0))
  ::list
  (let ((suffix (split! into at: at)))
    (append! into sublist suffix)))

(e.g.
 (paste! (list 'a 'b 'c) into: (list 1 2 3 4) at: 2)
 ===> (1 2 a b c 3 4))


(define (sublist satisfying?::predicate elements::list)
  (and (not (null? elements))
       (if (satisfying? elements)
	   elements
	   (sublist satisfying? (cdr elements)))))

(e.g.
 (sublist (lambda (cell)
	    (= (car cell) 3))
	  '(1 2 3 4 5))
 ===> (3 4 5))


(define (sublist+index satisfying?::predicate elements::list
		       #!key (start-index 0))
  (if (null? elements)
      (values #f start-index)
      (if (satisfying? elements start-index)
	  (values elements start-index)
	  (sublist+index satisfying? (cdr elements)
			 start-index: (+ start-index 1)))))

(e.g.
 (sublist+index (lambda (cell index)
		  (= (car cell) index))
	  '(4 3 2 1 0))
 ===> (2 1 0) 2)

(define (for-each-pair action sequence::list)::void
  (when (pair? sequence)
    (action sequence)
    (for-each-pair action (cdr sequence))))

(define (count-sublists satisfying?::predicate sequence::list)::int
  (let ((result ::int 0))
    (for-each-pair (lambda (cell::pair)
		     (when (satisfying? cell)
		       (set! result (+ result 1))))
		   sequence)
    result))

(define (last-pair-before index::int pairs::list)::list
  (let try ((n ::int 0)
	    (items ::list pairs))
    (if (and (is items pair?)
	     (is n < index)
	     (is (cdr items) pair?))
	(try (+ n 1) (cdr items))
	pairs)))

(define (map! f inout . in*)
  (cond
   ((null? in*)
    (let loop ((tip inout))
      (if (pair? tip)
	  (begin
	    (set! (car tip) (f (car tip)))
	    (loop (cdr tip)))
	  inout)))
   ((null? (cdr in*))
    (let loop ((tip1 inout)
	       (tip2 (car in*)))
      (if (and (pair? tip1) (pair? tip2))
	  (begin
	    (set! (car tip1) (f (car tip1) (car tip2)))
	    (loop (cdr tip1) (cdr tip2)))
	  inout)))
   ((null? (cddr in*))
    (let loop ((tip1 inout)
		  (tip2 (car in*))
		  (tip3 (cadr in*)))
	 (if (and (pair? tip1) (pair? tip2) (pair? tip3))
	     (begin
	       (set! (car tip1) (f (car tip1) (car tip2) (car tip3)))
	       (loop (cdr tip1) (cdr tip2) (cdr tip3))
	       inout))))
   (else
    (let loop ((tip inout)
		  (tips in*))
	 (if (and (pair? tip) (every pair? tips))
	     (begin
	       (set! (car tip) (apply f (car tip) (map car tips)))
	       (loop (cdr tip) (map! cdr tips)))
	     inout)))))

(define (remove! satisfying?::predicate elements::list)
  ::list
  (define (remove-prefix! prefix::list)
    (match prefix
      (`(,head . ,tail)
       (if (satisfying? head)
	   (cond ((pair? tail)
		  (set-car! prefix (car tail))
		  (set-cdr! prefix (cdr tail))
		  (remove-prefix! prefix))
		 (else
		  '()))
	   (let remove-tail! ((cell prefix))
	     (match (cdr cell)
	       (`(,head . ,tail)
		(cond ((satisfying? head)
		       (set-cdr! cell tail)
		       (remove-tail! cell))
		      (else
		       (remove-tail! (cdr cell)))))
	       (_ prefix)))))
      (_ prefix)))
  (remove-prefix! elements))

(e.g.
 (let ((l (list 2 3 4 5 6 7)))
   (remove! even? l)
   l) ===> (3 5 7))

(e.g.
 (let ((l (list 1 2 3 4 5 6 7 8)))
   (remove! even? l)
   l) ===> (1 3 5 7))

(define (count-while satisfying?::predicate s::sequence)::int
  (let ((l ::int (length s)))
    (let loop ((n ::int 0))
      (if (or (is n >= l)
	      (isnt (s n) satisfying?))
	  n
	  (loop (+ n 1))))))

(e.g.
 (count-while even? '(2 4 6 7 8)) ===> 3)

(e.g.
 (count-while char-upper-case? "ABcDEf") ===> 2)

(define (byte-ref value::integer index::ubyte)::ubyte
  (as ubyte
      (bitwise-and #xFF (bitwise-arithmetic-shift
			 value (- (* 8 index))))))

(e.g.
 (is (byte-ref #xAABBCCDD 0) = #xDD))

(e.g.
 (is (byte-ref #xAABBCCDD 1) = #xCC))

(e.g.
 (is (byte-ref #xAABBCCDD 2) = #xBB))

(e.g.
 (is (byte-ref #xAABBCCDD 3) = #xAA))
