(define (count-horizontal s board)
(let ((count 0)
(n (string-length s)))
(for row in board
(for i from 0 to (- (string-length row) n)
(when (string=? s (substring row 
i (+ i n)))
(set! count (+ count 1)))))
count))    (e.g.
(count-horizontal "XMAS" '("XMAS....XMAS"
"....XMAS...."))
===> 3) 
 
(define (vertical= s board row column diagonal)
(assert (is diagonal in (set -1 0 1))) 
(escape-with return
(for i from 0 below (string-length s)
(when (isnt (s i) eq? ((board (+ row i))
(+ column
 (* i diagonal))))
(return #false)))
(return #true)                                      )) (e.g.
(let ((board (string-split "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" "
")))
(and (vertical= "XMAS"board 3 9 0)
(vertical= "XMAS"board 0 4 1)
(vertical= "XMAS"board 3 9 -1)))) 

(define (count-vertical s board)
(let ((count 0)
(n (string-length s)) 
(height (length board))
(width (string-length (board 0))))
(for row from 0 to (- height n)
(for column from 0 below width
(when (vertical= s board row column 0)
(set! count (+ count 1))               ))) 
count))   (e.g.
(let ((board (string-split "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" "
")))
(count-vertical "XMAS" board)    )
===> 1) 

(define (count-diagonal-right s board)
(let ((count 0)
(n (string-length s)) 
(height (length board))
(width (string-length (board 0))))
(for row from 0 to (- height n)
(for column from 0 to (- width n) 
(when (vertical= s board row column 1)
(set! count (+ count 1))               ))) 
count))   (e.g.
(let ((board (string-split "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" "
")))
(count-diagonal-right "XMAS" board))
===> 1) 

(define (count-diagonal-left s board)
(let ((count 0)
(n (string-length s)) 
(height (length board))
(width (string-length (board 0))))
(for row from 0 to (- height n)
(for column from (- n 1) below width 
(when (vertical= s board row column -1)
(set! count (+ count 1))               ))) 
count)) (e.g.
(let ((board (string-split "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" "
")))
(count-diagonal-left "XMAS" board))
===> 1)

(define (read-lines input-port)
(unfold (lambda () (read-line input-port))
 #;until eof-object?)) 


(define (aoc4 input-port)
(let* ((board (read-lines input-port))
(s "XMAS")
(r "SAMX"))
(+ (count-horizontal s board)
(count-vertical s board)
(count-diagonal-right s board)
(count-diagonal-left s board)
(count-horizontal r board)
(count-vertical r board)
(count-diagonal-right r board)
(count-diagonal-left r board))   ))           (e.g.
(call-with-input-string "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX" 
aoc4)
===> 18)

