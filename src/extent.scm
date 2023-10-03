(import (language define-type))
(import (language examples))
(import (language for))

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

(define-type (Area left: real top: real
                   right: real bottom: real))

(define (area points::(sequence-of Position))::Area
  (let* ((result ::Area (Area left: +inf.0 top: +inf.0
                              right: -inf.0 bottom: -inf.0)))
    (for p::Position in points
      (set! result:left (min result:left p:left))
      (set! result:top (min result:top p:top))
      (set! result:right (max result:right p:left))
      (set! result:bottom (max result:bottom p:top)))
    result))
