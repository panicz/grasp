#!/bin/sh
#|
exec java -jar "libs/kawa.jar" -Dkawa.import.path="|:.:src" \
  --no-warn-unreachable -f "$0"
|#

(import (kawa regex))
(import (only (srfi :1) filter-map))
(import (language define-syntax-rule))
(import (language define-interface))
(import (utils shell))
(import (utils functions))
(import (language infix))
(import (language match))
(import (utils hash-table))
(import (language mapping))
(import (language for))

(display "Hello from Kawa!\n")

