(import (define-syntax-rule))

(define-syntax infix/postfix
  (syntax-rules ()
    
    ((infix/postfix x somewhat?)
     (somewhat? x))

    ((infix/postfix left related-to? right)
     (related-to? left right))

    ((infix/postfix left related-to? right . likewise)
     (let ((right* right))
       (and (infix/postfix left related-to? right*)
	    (infix/postfix right* . likewise))))))

(define-syntax extract-_
  (syntax-rules (_ is isnt quote
 		   quasiquote unquote
		   unquote-splicing)
    ;; ok, it's a bit rough, so it requires an explanation.
    ;; the macro operates on sequences of triples
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr>) +
    ;;
    ;; where <remaining-expr> is being systematically
    ;; rewritten to <processed-expr>. When the _ symbol
    ;; is encountered, it is replaced with a fresh "arg"
    ;; symbol, which is appended to both <arg-list>
    ;; and <processed-expr>.
    ;;
    ;; The goal is to create a lambda where each
    ;; consecutive _ is treated as a new argument
    ;; -- unless there are no _s: then we do not
    ;; create a lambda, but a plain expression.
    ;;
    ;; The nested "is" and "isnt" operators are treated
    ;; specially, in that the _s within those operators are
    ;; not extracted.
    ;;
    ;; Similarly, the _ isn't extracted from quoted forms,
    ;; and is only extracted from quasi-quoted forms if
    ;; it appears on unquoted positions.

    ;; The support for quasiquote modifies the tuples
    ;; to have the form
    ;;
    ;;   (<remaining-expr> <arg-list> <processed-expr> . qq*) +
    ;;
    ;; where qq* is a sequence of objects that expresses
    ;; the nesting level of the 'quasiquote' operator
    ;; (i.e. quasiquote inside quasiquote etc.)

    ;; The macro consists of the following cases:
    
    ;; fin case with no _s
    ((extract-_ fin (() () body))
     (fin (infix/postfix . body)))

    ;; fin case with some _s -- generate a lambda
    ((extract-_ fin (() args body))
     (lambda args (fin (infix/postfix . body))))

    ;; treat 'is' and 'isnt' operators specially and
    ;; don't touch their _s
    ((extract-_ fin (((is . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (is . t))) . *))

    ((extract-_ fin (((isnt . t) . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... (isnt . t))) . *))

    ;; same with 'quote'
    ((extract-_ fin (('literal . rest) args (body ...)) . *)
     (extract-_ fin (rest args (body ... 'literal)) . *))

    ;; when 'quasiquote' is encountered, we increase the
    ;; level of quasiquotation (the length of the qq* sequence)
    ((extract-_ fin
		(((quasiquote x) . rest) args body . qq*) . *)
     (extract-_ fin
		((x) () (quasiquote) qq . qq*)
		(rest args body) . *))

    ;; on the other hand, for 'unquote' and
    ;; 'unquote-splicing', we decrease the nesting level
    ;; (i.e. we consume one element from the qq* sequence)
    ((extract-_ fin
		(((unquote x) . rest) args body qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote) . qq*)
		(rest args body qq . qq*) . *))

    ((extract-_ fin
		(((unquote-splicing x) . rest) args body
		 qq . qq*) . *)
     (extract-_ fin
		((x) () (unquote-splicing) . qq*)
		(rest args body qq . qq*) . *))

    ;; push/unnest nested expression for processing
    ((extract-_ fin (((h . t) . rest) args body . qq) . *)
     (extract-_ fin ((h . t) () () . qq)
		(rest args body . qq) . *))

    ;; unquote in the tail position
    ((extract-_ fin
		((unquote x) args (body ...) qq . qq*) . *)
     (extract-_ fin
		((x) args (body ... unquote) . qq*) . *))
    
    ;; generate a new arg for the _ in the head position
    ((extract-_ fin ((_ . rest) (args ...) (body ...)) . *)
     (extract-_ fin (rest (args ... arg) (body ... arg)) . *))

    ;; rewrite the term in the head position to the back
    ;; of the processed terms
    ((extract-_ fin ((term . rest) args (body ...) . qq) . *)
     (extract-_ fin (rest args (body ... term) . qq) . *))

    ;; _ in the tail position
    ((extract-_ fin
		(_ (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin
		(rest (args+ ... args ... arg)
		      (body+ ... (body ... . arg)) . qq+) . *))

    ;; pop/nest back processed expression
    ;; ('last' is an atom; most likely (), but can also
    ;; be some value, e.g. in the case of assoc list literals)
    ((extract-_ fin
		(last (args ...) (body ...) . qq)
		(rest (args+ ...) (body+ ...) . qq+) . *)
     (extract-_ fin (rest (args+ ... args ...)
			  (body+ ... (body ... . last))
			  . qq+) . *))
    ))

(define-syntax-rule (identity-syntax form)
  form)

(define-syntax-rule (is . something)
  (extract-_ identity-syntax (something () ())))

(define-syntax-rule (isnt . something)
  (extract-_ not (something () ())))
