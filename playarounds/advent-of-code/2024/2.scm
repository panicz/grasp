    

 

(define (read-lines port)
(unfold (lambda () (read-line port)) #;until eof-object?) )


(define (safe-report? list-of-levels)
(or (and (apply < list-of-levels)
  (every (is 1 <= _ <= 3)  
(map - (cdr list-of-levels) list-of-levels)))
(and (apply > list-of-levels)
  (every (is 1 <= _ <= 3)  
(map - list-of-levels (cdr list-of-levels))))))


(define (count-safe-reports input-port)
(let* ((lines (read-lines input-port))
(reports (map (lambda (line)
(map string->number
(string-split line " ")))
lines                         )))
(count safe-report? reports)))


(call-with-input-string "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9" 
count-safe-reports               )



 