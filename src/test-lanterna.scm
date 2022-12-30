(import (term))
(import (for))

(define io ::Terminal (make-terminal))
;;(define ui ::TerminalScreen (TerminalScreen io))

(io:setBackgroundColor Color:ANSI:RED)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:YELLOW)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:GREEN)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:CYAN)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:BLUE)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:MAGENTA)
(io:putCharacter #\space)


(io:setBackgroundColor Color:ANSI:RED_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:YELLOW_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:GREEN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:CYAN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:BLUE_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:MAGENTA_BRIGHT)
(io:putCharacter #\space)

(io:setBackgroundColor Color:ANSI:DEFAULT)
(io:putCharacter #\newline)


(io:setBackgroundColor Color:ANSI:RED)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:RED_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:YELLOW)
(io:putCharacter #\space)

(io:setBackgroundColor Color:ANSI:YELLOW_BRIGHT)
(io:putCharacter #\space)

;;(io:setBackgroundColor Color:ANSI:GREEN)
;;(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:GREEN_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:CYAN_BRIGHT)
(io:putCharacter #\space)

(io:setBackgroundColor Color:ANSI:CYAN)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:BLUE)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:BLUE_BRIGHT)
(io:putCharacter #\space)
(io:setBackgroundColor Color:ANSI:MAGENTA)
(io:putCharacter #\space)

;;(io:setBackgroundColor Color:ANSI:MAGENTA_BRIGHT)


(io:setBackgroundColor Color:ANSI:DEFAULT)
(io:putCharacter #\newline)




(for r from 0 to 255 by 51
     (for g from 0 to 255 by 51
	  (for b from 0 to 255 by 51
	       (io:setBackgroundColor (Color:RGB r g b))
	       (io:putCharacter #\space)))
	  (io:setBackgroundColor Color:ANSI:DEFAULT)
	  (io:putCharacter #\newline))
	  
