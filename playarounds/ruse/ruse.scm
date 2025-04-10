

(define-syntax define-interface
  (syntax-rules (::)
    ((define-interface interface-name (super-interfaces ...)
       methods ...)
     ???)))

(define-syntax define-object
  (syntax-rules (::)
    ((define-object (class-name . constructor-args) :: interface
       definitions ...)
     ???)))


(define (counter-from n)
  (lambda ()
    (let ((previous n))
      (set! n (+ n 1))
      previous)))


(define-class (Counter current-value :: int)

  (define (next) :: int
    (let ((previous-value current-value))
      (set! current-value (+ current-value 1))
      previous-value)))

(define class-name
  (let* ((class (make-vector (next-index))))
    (lambda constructor-args
      (let ((instance (make-vector (+ (length fields) 1))))
	(set! (instance 0) table)
	(for field-name in fields
	  (set! (instance (field-index class field-name))
		((initializer field-name) instance)))
	(perform-initialization)
	instance))))

;; no to tak:
;; - w jaki sposob bedziemy reprezentowac klasy?
;;   - lista interfejsow
;;   - wskaznik bazowy
;;   - metoda inicjalizujaca

;; - w jaki sposob bedziemy reprezentowac obiekty?
;;   - 

;;
;; 

(define (field-ref object field-name)
  (vector-ref object (field-index (class object) field-name)))

(define (invoke object method-name . args)
  (apply (field-ref object method-name) args))



