(import (define-syntax-rule))
(import (assert))
(import (define-cache))
(import (define-type))
(import (mapping))
(import (define-parameter))
(import (match))
(import (infix))
(import (functions))
(import (conversions))

;(define-constant META_MASK ::long #x8000000000000000)
(define-early-constant CTRL_MASK      ::long #x4000000000000000)
(define-early-constant ALT_MASK       ::long #x2000000000000000)
(define-early-constant SHIFT_MASK     ::long #x1000000000000000)
(define-early-constant MODIFIERS_MASK ::long #x7000000000000000)

(define-parameter (unicode-input)::gnu.text.Char #!null)

(define-early-constant key-code-name
  (bimapping (key-code::long)
    ;; this table should be populated by particular clients
    'unknown-key))

(define (char-code c::char)::long
  (as long (char->integer c)))
     
(define (key-code combination)::long
  (match combination
    (`(shift . ,rest)
      (bitwise-ior SHIFT_MASK (key-code rest)))
    (`(ctrl . ,rest)
      (bitwise-ior CTRL_MASK (key-code rest)))
    (`(alt . ,rest)
      (bitwise-ior ALT_MASK (key-code rest)))
    (`(,key)
     ((inverse key-code-name) key))
    (,@(isnt _ pair?)
     ((inverse key-code-name) combination))))

(define-early-constant keymap
  (mapping (key-code::long)::(maps () to: boolean)
    never))
  
(define (set-key! combination action::(maps () to: boolean))
  (set! (keymap (key-code combination)) action))
