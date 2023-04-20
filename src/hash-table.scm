(import (define-syntax-rule))
(define-alias make-weak-key-hash-table java.util.WeakHashMap)
(define-alias make-hash-table java.util.HashMap)
(define-alias Map java.util.Map)
(define-alias Set java.util.Set)

(define (hash-set! table::Map key value)::void
  (table:put key value))

(define (hash-ref table::Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (pair? default)
	  ((car default)))))

(define (hash-remove! table::Map key)::void
  (table:remove key))
  
;; hash-ref+ is like hash-ref, but it stores
;; the result of evaluating the "default" thunk
;; in the hash table
(define (hash-ref+ table::Map key . default)
  (if (table:contains-key key)
      (table:get key)
      (if (pair? default)
	  (let ((value ((car default))))
	    (table:put key value)
	    value))))

(define-syntax-rule (unset! (mapping object))
  (let ((table ::Map (procedure-property mapping 'table)))
    (hash-remove! table object)))

(define-syntax-rule (update! (mapping object) expression)
  (let ((value expression))
    (unless (equal? (mapping object) value)
      (set! (mapping object) value))))

(define (reset! mapping)::void
  (let ((table ::Map (procedure-property mapping 'table)))
    (table:clear)))

(define (clean? mapping)::boolean
  (let ((table ::Map (procedure-property mapping 'table)))
    (table:isEmpty)))
  
(define (overrides mapping)::Set
  (let ((table ::Map (procedure-property mapping 'table)))
    (table:keySet)))

(define (overridden-at? mapping key)::boolean
    (let ((table ::Map (procedure-property mapping 'table)))
      (table:contains-key key)))
