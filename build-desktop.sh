#!/bin/sh

set -x

mkdir -p build/desktop
cd src

KAWA_JAR="../libs/kawa.jar"
JSVG="../libs/jsvg-1.0.0.jar"
JSERIAL="../libs/jSerialComm-2.10.4.jar"

DEPS=`java -jar $KAWA_JAR --no-warn-unreachable \
 -f analdep.scm -- --list grasp-desktop.scm`


UPDATE=""
for file in $DEPS
do
    CLASS=`echo "../build/desktop/$file" \
| sed -e 's/-/$Mn/g' \
| sed -e 's/scm$/class/'`
    if [ ! -f $CLASS ] || [ $file -nt $CLASS ];
    then
	UPDATE="$UPDATE $file"
    fi
done

echo $UPDATE

set -x

java -cp "../build/desktop/:$KAWA_JAR:$JSVG:$JSERIAL" kawa.repl \
     --no-warn-unreachable -d ../build/desktop -C \
     $UPDATE grasp-desktop.scm || exit
set +x

cd ..

if [ ! -f build/desktop/assets ]; then
    cp -r assets build/desktop
fi

cd build/desktop

if [ ! -f com/github/weisj/jsvg ]; then
    cp ../../libs/jsvg-1.0.0.jar .
    unzip jsvg-1.0.0.jar || exit
    rm jsvg-1.0.0.jar module-info.class
    rm -rf META-INF
fi

if [ ! -f com/fazecast/jSerialComm ]; then
    cp ../../libs/jSerialComm-2.10.4.jar .
    unzip jSerialComm-2.10.4.jar || exit
    rm jSerialComm-2.10.4.jar module-info.class
    rm -rf META-INF
fi

if [ ! -f kawa ]; then
    cp ../../libs/kawa.jar .
    unzip -uo kawa.jar || exit
    rm kawa.jar
fi

jar --verbose --create --file ../grasp-desktop.jar \
    --main-class=grasp\$Mndesktop `find ./ -type f`

cd ..
rm -r desktop
