#!/bin/sh 
#|
mkdir -p build/cache
JARS=`ls libs/*.jar | tr '\n' ':' | sed 's/:$//'`
exec java -cp "$JARS:build/cache" kawa.repl \
  -Dkawa.import.path="|:src:build/cache:." \
  -f $0 $*
|#
(import (utils build))
(build-with (command-line))
(exit)
