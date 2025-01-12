

 
(define (read-lists input-port) 
(let ((first (read input-port)))
(if (eof-object? first)
  '()
 (let ((result `(,first)))
(let loop ((cone result))
(let ((input (read input-port)))
(cond
((eof-object? input)
result              )
(else
(set-cdr! cone (pair input (cdr cone)))
(loop (cdr cone))               )  )) ))))) 
   

(e.g. (call-with-input-string "3   4
4   3
2   5
1   3
3   9
3   3" read-lists)
===> (3 4 4 3 2 5 1 3 3 9 3 3))


(define (unweave items #;by n)
(if (is n <= 1)
  items 
(let*  ((result (map! list (take n items)))
(input (drop n  items)) 
(front (map (lambda (x) x) result)))
(escape-with break
(while (isnt input empty?)
(map! (lambda (x)
(when (empty? input)
  (break)) 

(set-cdr! x (cons (car input)
(cdr x)))
(set! input (cdr input))    
(cdr x))  front)))
result                                                   )))
(e.g.        
(unweave '(3 4 4 3 2 5 1 3 3 9 3 3) 2)
===> ((3
4
2
1
3
3) (4
3
5
3
9
3))
)
(and-let* ((x y (values 2 3)))
(+ x y)) 

(call-with-input-string "3   4
4   3
2   5
1   3
3   9
3   3"
(lambda (input-port)
(and-let* ((input (read-lists input-port))
((quasiquote ((unquote left) (unquote right))) (unweave input 2)))
(apply + 
(map (lambda (a b)
(abs (- a b)))
(sort left) 
(sort right)))))) 
 












