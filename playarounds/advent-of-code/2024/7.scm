(define sample-input
"190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(define (combinations base n)
(cond
((<= n 0) '())
((= n 1)
(map list base))
(else
(append-map
 (lambda (c)
(map (lambda (x)
(cons x c))
base))
(combinations base (- n 1))))))  (e.g.
(combinations '(a b) 3)
===> ((a a a)
(b a a)
(a b a)
(b b a)
(a a b)
(b a b)
(a b b)
(b b b))) 
 
 
 
(define (applied operators operands)
(fold-left (lambda (result operand operator) 
(operator result operand))
(car operands) (cdr operands) operators)) 

(e.g. (applied (list + *) (list 2 3 4)) 
;; 2 + 3 * 4 (left-to-right evaluation, so it's 5 * 4)
===> 20) 
 

(define (aoc7) 
(let loop ((result 0) 
(line (read-line)))
(if (eof-object? line)
  result
(let* ((parts (string-split line ": "))
(expected-result (string->number (car parts)))
(operands (map string->number
(string-split (cadr parts) " ")))
(n (length operands))                         ) 
(loop (+ result
(if (any (is expected-result
 = (applied _ operands))
(combinations (list + *) (- n 1)))
 expected-result
0                              )) 
(read-line))))))

(e.g.
(with-input-from-string sample-input aoc7)
===> 3749)





