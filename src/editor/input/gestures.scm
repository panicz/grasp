(module-name (editor input gestures))

(import (language define-type))
(import (language define-parameter))
(import (language define-property))
(import (language define-cache))
(import (language fundamental))
(import (editor interfaces painting))
(import (language examples))
(import (language for))
(import (language infix))
(import (language match))
(import (utils functions))
(import (utils print))
(import (editor document documents))
(import (editor document cursor))
(import (editor input pane))
(import (editor interfaces elements))
(import (editor types primitive))
(import (editor types extensions extensions))
(import (editor types extensions interactions))
(import (editor input evaluation))
(import (editor types extensions visual-stepper))

(define-type (Recognizer name: string
			 recognizes: (maps ((sequence-of Position))
					   to: Object)
			 action: (maps (Recognizer (sequence-of
						    Position) Object)
				       to: void)))

(define-parameter (the-recognizers)
  ::($bracket-apply$ java.util.List Recognizer)
  (java.util.ArrayList))

(define (distance-to-line-through p1::Position p2::Position)
  ::(maps (Position) to: real)
  (let* ((dx ::real (- p2:left p1:left))
         (dy ::real (- p2:top p1:top))
	 (1/d ::real (/ (hypotenuse dx dy)))
	 (cross ::real (- (* p1:left p2:top)
	                  (* p2:left p1:top))))
    (lambda (p::Position)
      ::real
      (abs (* (- (* dy p:left) (* dx p:top)
		 cross)
	      1/d)))))

(define (simplify points::($bracket-apply$ java.util.List
					   Position)
			epsilon::real)
  ::($bracket-apply$ java.util.List Position)
  (let* ((n ::int (length points))
         (n-1 ::int (- n 1)))
    (if (is n <= 2)
	(let ((result ::($bracket-apply$ java.util.List Position)
		      (java.util.ArrayList)))
	  (result:addAll points)
	  result)
	(let* ((first ::Position (points 0))
	       (last ::Position (points n-1))
	       (interior (points:subList 1 n-1))
	       (distance-to ::(maps (Position) to: real)
	                    (distance-to-line-through first last))
	       (furthest-distance ::real (distance-to (interior 0)))
	       (index ::int 0))
	  (for i::int from 1 below (length interior)
	       (let* ((element ::Position (interior i))
	              (distance ::real (distance-to element)))
		 (when (is distance > furthest-distance)
	           (set! index i)
		   (set! furthest-distance distance))))
	  (if (is furthest-distance > epsilon)
	      (let* ((left (points:subList 0 (+ index 2)))
	             (right (points:subList (+ index 1) n))
		     (left* ::java.util.List
			    (simplify left epsilon))
		     (right* ::java.util.List
			     (simplify right epsilon)))
                (left*:addAll (right*:subList 1 (length right*)))
		left*)
	  (java.util.ArrayList first last))))))

(e.g.
 (equal?
  (simplify
   (java.util.ArrayList
    (Position left: 0 top: 0)
    (Position left: 1 top: 0.1)
    (Position left: 2 top: -0.1)
    (Position left: 3 top: 5)
    (Position left: 4 top: 6)
    (Position left: 5 top: 7)
    (Position left: 6 top: 8.1)
    (Position left: 7 top: 9)
    (Position left: 8 top: 9)
    (Position left: 9 top: 9))
   1.0)
  (java.util.ArrayList
   (Position left: 0 top: 0)
   (Position left: 2 top: -0.1)
   (Position left: 3 top: 5)
   (Position left: 7 top: 9)
   (Position left: 9 top: 9))))

(define-early-constant split-pane-by-horizontal-line ::Recognizer
  (Recognizer
   name: "horizontal-line"
   recognizes:
   (lambda (points::(sequence-of Position))
     (let* ((vicinity ::real
		      (painter:line-simplification-resolution))
	    (simplified ::java.util.List
			(simplify points vicinity)))
       (and-let* (((is (length simplified) = 2))
		  (p0 ::Position (simplified 0))
		  (p1 ::Position (simplified 1))
		  ((is (abs (- (slot-ref p0 'top)
			       (slot-ref p1 'top)))
		       <= (* vicinity 2)))
		  (line ::Area (area simplified)))
	 (and (screen:can-split-below? line)
	      line))))
   action:
   (lambda (own::Recognizer
	    points::(sequence-of Position)
	    line::Area)
     (screen:split-below! line))))

(define-early-constant split-pane-by-vertical-line
  (Recognizer
   name: "vertical-line"
   recognizes:
   (lambda (points::(sequence-of Position))
     (let* ((vicinity ::real
		      (painter:line-simplification-resolution))
	    (simplified ::java.util.List
			(simplify points vicinity)))
       (and-let* (((is (length simplified) = 2))
		  (p0 ::Position (simplified 0))
		  (p1 ::Position (simplified 1))
		  ((is (abs (- (slot-ref p0 'left)
			       (slot-ref p1 'left)))
		       <= (* vicinity 2)))
		  (line ::Area (area simplified)))
	 (and (screen:can-split-beside? line)
	      line))))
   action:
   (lambda (own::Recognizer
	    points::(sequence-of Position)
	    line::Area)
     (screen:split-beside! line))))

(define-early-constant evaluate-expression-by-wedge ::Recognizer
  (Recognizer
   name: "eval"
   recognizes:
   (lambda (points ::(sequence-of Position))
     (and-let* ((vicinity ::real
			  (painter:line-simplification-resolution))
		(simplified ::java.util.List
			    (simplify points vicinity))
		((is (length simplified) = 3))
		(top ::Embeddable (slot-ref screen 'top))
		((Position left: p0-left top: p0-top) (simplified 0))
		((Position left: p1-left top: p1-top) (simplified 1))
		((Position left: p2-left top: p2-top) (simplified 2))
		((is p0-left < p1-left < p2-left))
		((is p1-top < p0-top))
		((is p1-top < p2-top))
		(editor0 ::Editor (top:pane-under p0-left p0-top))
		(editor1 ::Editor (top:pane-under p1-left p1-top))
		(editor2 ::Editor (top:pane-under p2-left p2-top))
		((eq? editor0 editor1))
		((eq? editor1 editor2))
		(document ::Document (slot-ref editor1 'document))
		(x0 y0 (top:outside-in p0-left p0-top))
		(x1 y1 (top:outside-in p1-left p1-top))
		(x2 y2 (top:outside-in p2-left p2-top))
		(c0 ::Cursor (cursor-under x0 y0 (head document)))
		(c1 ::Cursor (cursor-under x1 y1 (head document)))
		(c2 ::Cursor (cursor-under x2 y2 (head document)))
		((eq? (cdr c0) (cdr c2)))
		(n ::integer (as integer (length c0)))
		(m ::integer (as integer (length c1)))
		((is m >= n))
		(cursor (drop (- m n) c1))
		((pair? cursor)))
       (values (cdr cursor) document)))
   action:
   (lambda (self::Recognizer
	    points::(sequence-of Position)
	    cursor
	    document)
     (evaluate-expression! at: cursor in: document))))
