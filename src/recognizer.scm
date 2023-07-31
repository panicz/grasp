(import (define-type))
(import (define-parameter))
(import (extent))

(define-type (Recognizer name: string
			 recognizes: (maps ((sequence-of Position))
					   to: boolean)
			 action: (maps () to: void)))

(define-parameter (the-recognizers)
  ::($bracket-apply$ java.util.List Recognizer)
  (java.util.ArrayList))
