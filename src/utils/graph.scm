(module-name (utils graph))

(import (language infix))
(import (utils functions))
(import (utils print))

(define (reach #;of graph #;from node)
  (define (walk #;from novel #;into visited)
    (let* ((vicinity (fold-left union '() (map graph novel)))
	   (visited (union visited novel))
	   (novel (difference vicinity visited)))
      (if (null? novel)
	  visited
	  (walk #;from novel #;into visited))))
  (walk #;from (graph node) #;into '()))

(define (graph-layers graph nodes)
  (let loop ((modules nodes)
	     (layers '())
	     (allowed-dependencies (set)))
    (let-values (((layer modules)
		  (partition (is (graph _) subset?
				 allowed-dependencies)
			     modules)))
      (cond
       ((null? modules)
	`(,layer . ,layers))
       ((null? layer)
	;;(print"empty layer with remaining modules "modules)
	`(,modules . ,layers))
       (else
	(loop modules
	      `(,layer . ,layers)
	      (union! allowed-dependencies layer)))))))
