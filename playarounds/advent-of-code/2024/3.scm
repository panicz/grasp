(import (kawa regex)) 

(define (aoc3 input )
(apply +
(map (lambda (m)
(* (string->number (m 1))
   (string->number (m 2)))                   )
(regex-matches "mul[(][0-9]+,[0-9]+[)]" input)))) 


(aoc3 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")