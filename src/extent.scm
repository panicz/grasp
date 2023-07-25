(import (define-type))
(import (examples))
(import (for))

(define-type (Point x: real := 0 y: real := 0))

(define-type (Extent width: real := 0
                     height: real := 0))

(define (string-extent s::java.lang.CharSequence)::Extent
  (let ((line-length 0)
        (max-length 0)
        (total-lines 1))
    (for c in s
         (cond ((eq? c #\newline)
                (set! max-length (max max-length
				      line-length))
                (set! total-lines (+ total-lines 1))
                (set! line-length 0))
               (else
                (set! line-length (+ line-length 1)))))
    (Extent width: (max max-length line-length)
            height: total-lines)))

(e.g.
 (string-extent "\
abc
def") ===> [Extent width: 3 height: 2])

(define-type (Position left: real := 0
		       top: real := 0))
