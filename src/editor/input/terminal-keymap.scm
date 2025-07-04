(module-name (editor input terminal-keymap))

(import (editor input input))
(import (editor term))
(import (language match))
(import (language infix))

(import (utils print))

(define (special-key-code key::KeyType)::long
  (bitwise-arithmetic-shift (as long (key:ordinal))
			    java.lang.Integer:SIZE))

(define (regular-key-code c::char)::long
  (as long (char->integer c)))

(define (scancode input::KeyStroke)::long
  (define (with-modifiers code::long)::long
    (bitwise-ior
     (if (input:ctrl-down?) CTRL_MASK 0)
     (if (input:alt-down?) ALT_MASK 0)
     (if (or (input:shift-down?)
	     (and (eq? (input:getKeyType) KeyType:Character)
		  (or (input:ctrl-down?)
		      (input:alt-down?))
		  (let ((c (input:getCharacter)))
		    (char-upper-case? (integer->char (c:charValue))))))
	 SHIFT_MASK 0)
     code))
  
  (match (input:getKeyType)
    (,KeyType:Character
     (let ((c ::java.lang.Character (input:getCharacter)))
       (with-modifiers
	(char->integer
	 (char-downcase
	  (integer->char (c:charValue)))))))
    (,KeyType:ReverseTab
     (bitwise-ior SHIFT_MASK (special-key-code KeyType:Tab)))

    (type
     (with-modifiers (special-key-code type)))))

(define (input-character input::KeyStroke)::char
  (let* ((c ::java.lang.Character (input:getCharacter)))
    (or (and c
	     (not (input:ctrl-down?))
	     (not (input:alt-down?))
	     (integer->char (c:charValue)))
	#\null)))

(define (initialize-keymap)  
  (set! (key-code-name (special-key-code KeyType:Escape)) 'esc)
  (set! (key-code-name (special-key-code KeyType:Escape)) 'escape)
  (set! (key-code-name (special-key-code KeyType:Backspace))
	#\backspace)
  (set! (key-code-name (special-key-code KeyType:Backspace))
	'backspace)
  (set! (key-code-name (special-key-code KeyType:ArrowLeft)) 'left)
  (set! (key-code-name (special-key-code KeyType:ArrowRight)) 'right)
  (set! (key-code-name (special-key-code KeyType:ArrowUp)) 'up)
  (set! (key-code-name (special-key-code KeyType:ArrowDown)) 'down)
  (set! (key-code-name (special-key-code KeyType:Insert)) 'ins)
  (set! (key-code-name (special-key-code KeyType:Insert)) 'insert)
  (set! (key-code-name (special-key-code KeyType:Delete)) 'del)
  (set! (key-code-name (special-key-code KeyType:Delete)) 'delete)
  (set! (key-code-name (special-key-code KeyType:Home)) 'home)
  (set! (key-code-name (special-key-code KeyType:End)) 'end)
  (set! (key-code-name (special-key-code KeyType:PageUp)) 'page-up)
  (set! (key-code-name (special-key-code KeyType:PageDown)) 'page-down)
  (set! (key-code-name (special-key-code KeyType:Tab)) #\tab)
  (set! (key-code-name (special-key-code KeyType:Tab)) 'tab)
  (set! (key-code-name (special-key-code KeyType:Enter)) #\newline)
  (set! (key-code-name (special-key-code KeyType:Enter)) 'enter)

  (set! (key-code-name (special-key-code KeyType:F1)) 'f1)
  (set! (key-code-name (special-key-code KeyType:F2)) 'f2)
  (set! (key-code-name (special-key-code KeyType:F3)) 'f3)
  (set! (key-code-name (special-key-code KeyType:F4)) 'f4)
  (set! (key-code-name (special-key-code KeyType:F5)) 'f5)
  (set! (key-code-name (special-key-code KeyType:F6)) 'f6)
  (set! (key-code-name (special-key-code KeyType:F7)) 'f7)
  (set! (key-code-name (special-key-code KeyType:F8)) 'f8)
  (set! (key-code-name (special-key-code KeyType:F9)) 'f9)
  (set! (key-code-name (special-key-code KeyType:F10)) 'f10)
  (set! (key-code-name (special-key-code KeyType:F11)) 'f11)
  (set! (key-code-name (special-key-code KeyType:F12)) 'f12)
  (set! (key-code-name (special-key-code KeyType:F13)) 'f13)
  (set! (key-code-name (special-key-code KeyType:F14)) 'f14)
  (set! (key-code-name (special-key-code KeyType:F15)) 'f15)
  (set! (key-code-name (special-key-code KeyType:F16)) 'f16)
  (set! (key-code-name (special-key-code KeyType:F17)) 'f17)
  (set! (key-code-name (special-key-code KeyType:F18)) 'f18)
  (set! (key-code-name (special-key-code KeyType:F19)) 'f19)
  
  (set! (key-code-name (special-key-code KeyType:F1)) 'F1)
  (set! (key-code-name (special-key-code KeyType:F2)) 'F2)
  (set! (key-code-name (special-key-code KeyType:F3)) 'F3)
  (set! (key-code-name (special-key-code KeyType:F4)) 'F4)
  (set! (key-code-name (special-key-code KeyType:F5)) 'F5)
  (set! (key-code-name (special-key-code KeyType:F6)) 'F6)
  (set! (key-code-name (special-key-code KeyType:F7)) 'F7)
  (set! (key-code-name (special-key-code KeyType:F8)) 'F8)
  (set! (key-code-name (special-key-code KeyType:F9)) 'F9)
  (set! (key-code-name (special-key-code KeyType:F10)) 'F10)
  (set! (key-code-name (special-key-code KeyType:F11)) 'F11)
  (set! (key-code-name (special-key-code KeyType:F12)) 'F12)
  (set! (key-code-name (special-key-code KeyType:F13)) 'F13)
  (set! (key-code-name (special-key-code KeyType:F14)) 'F14)
  (set! (key-code-name (special-key-code KeyType:F15)) 'F15)
  (set! (key-code-name (special-key-code KeyType:F16)) 'F16)
  (set! (key-code-name (special-key-code KeyType:F17)) 'F17)
  (set! (key-code-name (special-key-code KeyType:F18)) 'F18)
  (set! (key-code-name (special-key-code KeyType:F19)) 'F19)

  (set! (key-code-name (regular-key-code #\space)) #\space)
  (set! (key-code-name (regular-key-code #\space)) 'space)
#|
  (set! (key-code-name (regular-key-code #\newline)) #\newline)
  (set! (key-code-name (regular-key-code #\newline)) 'enter)
  |#
  (set! (key-code-name (regular-key-code #\0)) #\0)
  (set! (key-code-name (regular-key-code #\1)) #\1)
  (set! (key-code-name (regular-key-code #\2)) #\2)
  (set! (key-code-name (regular-key-code #\3)) #\3)
  (set! (key-code-name (regular-key-code #\4)) #\4)
  (set! (key-code-name (regular-key-code #\5)) #\5)
  (set! (key-code-name (regular-key-code #\6)) #\6)
  (set! (key-code-name (regular-key-code #\7)) #\7)
  (set! (key-code-name (regular-key-code #\8)) #\8)
  (set! (key-code-name (regular-key-code #\9)) #\9)
  
  (set! (key-code-name (regular-key-code #\0)) 0)
  (set! (key-code-name (regular-key-code #\1)) 1)
  (set! (key-code-name (regular-key-code #\2)) 2)
  (set! (key-code-name (regular-key-code #\3)) 3)
  (set! (key-code-name (regular-key-code #\4)) 4)
  (set! (key-code-name (regular-key-code #\5)) 5)
  (set! (key-code-name (regular-key-code #\6)) 6)
  (set! (key-code-name (regular-key-code #\7)) 7)
  (set! (key-code-name (regular-key-code #\8)) 8)
  (set! (key-code-name (regular-key-code #\9)) 9)

  (set! (key-code-name (regular-key-code #\a)) #\a)
  (set! (key-code-name (regular-key-code #\b)) #\b)
  (set! (key-code-name (regular-key-code #\c)) #\c)
  (set! (key-code-name (regular-key-code #\d)) #\d)
  (set! (key-code-name (regular-key-code #\e)) #\e)
  (set! (key-code-name (regular-key-code #\f)) #\f)
  (set! (key-code-name (regular-key-code #\g)) #\g)
  (set! (key-code-name (regular-key-code #\h)) #\h)
  (set! (key-code-name (regular-key-code #\i)) #\i)
  (set! (key-code-name (regular-key-code #\j)) #\j)
  (set! (key-code-name (regular-key-code #\k)) #\k)
  (set! (key-code-name (regular-key-code #\l)) #\l)
  (set! (key-code-name (regular-key-code #\m)) #\m)
  (set! (key-code-name (regular-key-code #\n)) #\n)
  (set! (key-code-name (regular-key-code #\o)) #\o)
  (set! (key-code-name (regular-key-code #\p)) #\p)
  (set! (key-code-name (regular-key-code #\q)) #\q)
  (set! (key-code-name (regular-key-code #\r)) #\r)
  (set! (key-code-name (regular-key-code #\s)) #\s)
  (set! (key-code-name (regular-key-code #\t)) #\t)
  (set! (key-code-name (regular-key-code #\u)) #\u)
  (set! (key-code-name (regular-key-code #\v)) #\v)
  (set! (key-code-name (regular-key-code #\w)) #\w)
  (set! (key-code-name (regular-key-code #\x)) #\x)
  (set! (key-code-name (regular-key-code #\y)) #\y)
  (set! (key-code-name (regular-key-code #\z)) #\z)

  (set! (key-code-name (regular-key-code #\a)) 'a)
  (set! (key-code-name (regular-key-code #\b)) 'b)
  (set! (key-code-name (regular-key-code #\c)) 'c)
  (set! (key-code-name (regular-key-code #\d)) 'd)
  (set! (key-code-name (regular-key-code #\e)) 'e)
  (set! (key-code-name (regular-key-code #\f)) 'f)
  (set! (key-code-name (regular-key-code #\g)) 'g)
  (set! (key-code-name (regular-key-code #\h)) 'h)
  (set! (key-code-name (regular-key-code #\i)) 'i)
  (set! (key-code-name (regular-key-code #\j)) 'j)
  (set! (key-code-name (regular-key-code #\k)) 'k)
  (set! (key-code-name (regular-key-code #\l)) 'l)
  (set! (key-code-name (regular-key-code #\m)) 'm)
  (set! (key-code-name (regular-key-code #\n)) 'n)
  (set! (key-code-name (regular-key-code #\o)) 'o)
  (set! (key-code-name (regular-key-code #\p)) 'p)
  (set! (key-code-name (regular-key-code #\q)) 'q)
  (set! (key-code-name (regular-key-code #\r)) 'r)
  (set! (key-code-name (regular-key-code #\s)) 's)
  (set! (key-code-name (regular-key-code #\t)) 't)
  (set! (key-code-name (regular-key-code #\u)) 'u)
  (set! (key-code-name (regular-key-code #\v)) 'v)
  (set! (key-code-name (regular-key-code #\w)) 'w)
  (set! (key-code-name (regular-key-code #\x)) 'x)
  (set! (key-code-name (regular-key-code #\y)) 'y)
  (set! (key-code-name (regular-key-code #\z)) 'z)

  (set! (key-code-name (regular-key-code #\&)) #\&)
  (set! (key-code-name (regular-key-code #\&)) '&)

  (set! (key-code-name (regular-key-code #\*)) #\*)
  (set! (key-code-name (regular-key-code #\*)) 'asterisk)
  (set! (key-code-name (regular-key-code #\*)) '*)

  (set! (key-code-name (regular-key-code #\@)) #\@)
  (set! (key-code-name (regular-key-code #\@)) 'at)

  (set! (key-code-name (regular-key-code #\@)) #\`)
  (set! (key-code-name (regular-key-code #\@)) 'backquote)
  
  (set! (key-code-name (regular-key-code #\\)) #\\)
  (set! (key-code-name (regular-key-code #\\)) '\\)
  (set! (key-code-name (regular-key-code #\\)) 'backslash)

  (set! (key-code-name (regular-key-code #\^)) #\^)
  (set! (key-code-name (regular-key-code #\^)) '^)

  (set! (key-code-name (regular-key-code #\:)) #\:)
  (set! (key-code-name (regular-key-code #\:)) ':)
  (set! (key-code-name (regular-key-code #\:)) 'colon)

  (set! (key-code-name (regular-key-code #\,)) #\,)
  (set! (key-code-name (regular-key-code #\,)) 'comma)

  (set! (key-code-name (regular-key-code #\$)) #\$)
  (set! (key-code-name (regular-key-code #\$)) '$)
  (set! (key-code-name (regular-key-code #\$)) 'dollar)

  (set! (key-code-name (regular-key-code #\=)) #\=)
  (set! (key-code-name (regular-key-code #\=)) '=)
  (set! (key-code-name (regular-key-code #\=)) 'equals)

  (set! (key-code-name (regular-key-code #\!)) #\!)
  (set! (key-code-name (regular-key-code #\!)) 'exclamation-mark)
  (set! (key-code-name (regular-key-code #\!)) '!)

  (set! (key-code-name (regular-key-code #\>)) #\>)
  (set! (key-code-name (regular-key-code #\>)) 'greater)
  (set! (key-code-name (regular-key-code #\>)) '>)

  (set! (key-code-name (regular-key-code #\<)) #\<)
  (set! (key-code-name (regular-key-code #\<)) 'less)
  (set! (key-code-name (regular-key-code #\<)) '<)

  (set! (key-code-name (regular-key-code #\-)) #\-)
  (set! (key-code-name (regular-key-code #\-)) 'minus)
  (set! (key-code-name (regular-key-code #\-)) '-)

  (set! (key-code-name (regular-key-code #\#)) #\#)
  (set! (key-code-name (regular-key-code #\#)) 'hash)

  (set! (key-code-name (regular-key-code #\.)) 'period)
  (set! (key-code-name (regular-key-code #\.)) 'dot)
  (set! (key-code-name (regular-key-code #\.)) #\.)

  (set! (key-code-name (regular-key-code #\+)) 'plus)
  (set! (key-code-name (regular-key-code #\+)) #\+)
  (set! (key-code-name (regular-key-code #\+)) '+)

  (set! (key-code-name (regular-key-code #\+)) 'quote)
  (set! (key-code-name (regular-key-code #\')) #\')

  (set! (key-code-name (regular-key-code #\")) 'double-quote)
  (set! (key-code-name (regular-key-code #\")) #\")

  (set! (key-code-name (regular-key-code #\;)) 'semicolon)
  (set! (key-code-name (regular-key-code #\;)) #\;)

  (set! (key-code-name (regular-key-code #\/)) 'slash)
  (set! (key-code-name (regular-key-code #\/)) #\/)
  (set! (key-code-name (regular-key-code #\/)) '/)

  (set! (key-code-name (regular-key-code #\_)) 'underscore)
  (set! (key-code-name (regular-key-code #\_)) #\_)
  (set! (key-code-name (regular-key-code #\_)) '_)

  (set! (key-code-name (regular-key-code #\())
	'opening-parenthesis)
  (set! (key-code-name (regular-key-code #\()) 'opening-paren)
  (set! (key-code-name (regular-key-code #\()) 'open-parenthesis)
  (set! (key-code-name (regular-key-code #\()) 'open-paren)
  (set! (key-code-name (regular-key-code #\()) 'left-parenthesis)
  (set! (key-code-name (regular-key-code #\()) 'left-paren)
  (set! (key-code-name (regular-key-code #\()) #\()
  
  (set! (key-code-name (regular-key-code #\)))
	'closing-parenthesis)
  (set! (key-code-name (regular-key-code #\)))
	'closing-paren)
  (set! (key-code-name (regular-key-code #\)))
	'close-parenthesis)
  (set! (key-code-name (regular-key-code #\))) 'close-paren)
  (set! (key-code-name (regular-key-code #\)))
	'right-parenthesis)
  (set! (key-code-name (regular-key-code #\))) 'right-paren)
  (set! (key-code-name (regular-key-code #\))) #\))
  
  (set! (key-code-name (regular-key-code #\[)) 'opening-bracket)
  (set! (key-code-name (regular-key-code #\[)) 'open-bracket)
  (set! (key-code-name (regular-key-code #\[)) 'left-bracket)
  (set! (key-code-name (regular-key-code #\[)) #\[)
  
  (set! (key-code-name (regular-key-code #\])) 'closing-bracket)
  (set! (key-code-name (regular-key-code #\])) 'close-bracket)
  (set! (key-code-name (regular-key-code #\])) 'right-bracket)
  (set! (key-code-name (regular-key-code #\])) #\])
  
  (set! (key-code-name (regular-key-code #\{)) 'opening-brace)
  (set! (key-code-name (regular-key-code #\{)) 'open-brace)
  (set! (key-code-name (regular-key-code #\{)) 'left-brace)
  (set! (key-code-name (regular-key-code #\{)) #\{)
  
  (set! (key-code-name (regular-key-code #\})) 'closing-brace)
  (set! (key-code-name (regular-key-code #\})) 'close-brace)
  (set! (key-code-name (regular-key-code #\})) 'right-brace)
  (set! (key-code-name (regular-key-code #\})) #\})

  )
