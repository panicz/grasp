(module-name (editor input input))

(import (language define-syntax-rule))
(import (language assert))
(import (language define-cache))
(import (language define-type))
(import (language mapping))
(import (language define-parameter))
(import (language match))
(import (language infix))
(import (language for))
(import (utils functions))
(import (utils conversions))
(import (utils hash-table))
(import (editor document editor-operations))
(import (editor interfaces painting))
(import (utils print))

;(define-early-constant META_MASK      ::long #x8000000000000000)
(define-early-constant CTRL_MASK      ::long #x4000000000000000)
(define-early-constant ALT_MASK       ::long #x2000000000000000)
(define-early-constant SHIFT_MASK     ::long #x1000000000000000)
(define-early-constant MODIFIERS_MASK ::long #x7000000000000000)

(define-parameter (unicode-input)::gnu.text.Char #\null)

(define (insert-character-input!)::boolean
  (let ((c (unicode-input)))
    (and (isnt c eqv? #\null)
	 #;(begin
	   (write c)
	   (newline)
	   #t)
	 (insert-character! c))))

(define-early-constant keymap ::java.util.HashMap
  (java.util.HashMap))

(define-early-constant last-known-pointer-position
  ;; here we initialize 10 values, because Android can support
  ;; up to 10 pointers. That they'll be unused with other clients?
  ;; We don't care! 
  ((array-of Position)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)
   (Position left: 0 top: 0)))

(define (char-code c::char)::long
  (as long (char->integer c)))

(define-early-constant key-code-name
  (bimapping (key-code::long)
    ;; this table should be populated by particular clients
    'unknown-key))

(define (keychord-code combination)::long
  (match combination
    (`(shift . ,rest)
      (bitwise-ior SHIFT_MASK (keychord-code rest)))
    (`(ctrl . ,rest)
      (bitwise-ior CTRL_MASK (keychord-code rest)))
    (`(alt . ,rest)
      (bitwise-ior ALT_MASK (keychord-code rest)))
    (`(,key)
     ((inverse key-code-name) key))
    (,@(isnt _ pair?)
     ((inverse key-code-name) combination))))
  
(define (set-key! combination action::(maps () to: boolean))
  (hash-set! keymap (keychord-code combination) action))
