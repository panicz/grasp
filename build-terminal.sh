#!/bin/sh

mkdir -p build/terminal
cd src
java -cp "../libs/lanterna-3.1.1.jar:../libs/kawa.jar" kawa.repl \
     --no-warn-unreachable -d ../build/terminal -C \
     `java -jar ../libs/kawa.jar --no-warn-unreachable \
           -f analdep.scm -- --list grasp-terminal.scm` \
     grasp-terminal.scm || exit
cd ..
cp libs/lanterna-3.1.1.jar build/terminal
cp libs/kawa.jar build/terminal
cd build/terminal
unzip lanterna-3.1.1.jar || exit
unzip -uo kawa.jar || exit
rm lanterna-3.1.1.jar
rm kawa.jar
jar --verbose --create --file ../grasp-terminal.jar \
    --main-class=grasp\$Mnterminal `find ./ -name '*.class'`
cd ..
rm -r terminal
