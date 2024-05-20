#!/bin/sh

set -x

while [ "$#" -gt 0 ]; do
  case "$1" in
    -i) INIT="$2"; shift 2;;
    --init=*) INIT="${1#*=}"; shift 1;;
    *) break
  esac
done

mkdir -p build/terminal/assets
cd src
java -cp "../libs/lanterna-3.1.1.jar:../libs/kawa.jar" kawa.repl \
     --no-warn-unreachable -d ../build/terminal -C \
     `java -jar ../libs/kawa.jar --no-warn-unreachable \
           -f analdep.scm -- --list grasp-terminal.scm` $@ \
     grasp-terminal.scm || exit
cd ..
cp libs/lanterna-3.1.1.jar build/terminal
cp libs/kawa.jar build/terminal

if [ -z "$INIT" ]; then
    cp init/init.scm build/terminal/assets/
else
    cp $INIT build/terminal/assets/
fi

cd build/terminal
unzip lanterna-3.1.1.jar || exit
unzip -uo kawa.jar || exit
rm lanterna-3.1.1.jar
rm kawa.jar
jar --verbose --create --file ../grasp-terminal.jar \
    --main-class=grasp\$Mnterminal `find ./ -name '*.class'` \
    assets
cd ..
rm -r terminal
