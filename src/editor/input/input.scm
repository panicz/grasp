(module-name (editor input input))

(import (language define-syntax-rule))
(import (language assert))
(import (language define-cache))
(import (language define-type))
(import (language mapping))
(import (language define-parameter))
(import (language match))
(import (language infix))
(import (utils functions))
(import (utils conversions))
(import (editor document editor-operations))

;(define-early-constant META_MASK      ::long #x8000000000000000)
(define-early-constant CTRL_MASK      ::long #x4000000000000000)
(define-early-constant ALT_MASK       ::long #x2000000000000000)
(define-early-constant SHIFT_MASK     ::long #x1000000000000000)
(define-early-constant MODIFIERS_MASK ::long #x7000000000000000)

(define-parameter (unicode-input)::gnu.text.Char #\null)

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

(define (insert-character-input!)::boolean
  (let ((c (unicode-input)))
    (and (isnt c eqv? #\null)
	 #;(begin
	   (write c)
	   (newline)
	   #t)
	 (insert-character! c))))

(define-early-constant keymap
  (mapping (key-code::long)::(maps () to: boolean)
	   insert-character-input!))
  
(define (set-key! combination action::(maps () to: boolean))
  (set! (keymap (key-code combination)) action))
