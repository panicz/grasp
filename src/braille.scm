(import (language for))


(for i from 0 to #xff
     (when (= (modulo i 16) 0)
       (newline))
     (display (integer->char (+ #x2800 i)))
     (display #\space))
