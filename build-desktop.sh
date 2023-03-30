#!/bin/sh

set -x

mkdir -p build/desktop
cd src

KAWA_JAR="../libs/kawa.jar"

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

java -cp "../build/desktop/:$KAWA_JAR" kawa.repl \
     --no-warn-unreachable -d ../build/desktop -C \
     $UPDATE grasp-desktop.scm || exit
set +x

cd ..

if [ ! -f build/desktop/assets ]; then
    cp -r assets build/desktop
fi

cd build/desktop

if [ ! -f build/desktop/kawa ]; then
    cp ../../libs/kawa.jar .
    unzip kawa.jar || exit
    rm kawa.jar
fi

jar --verbose --create --file ../grasp-desktop.jar \
    --main-class=grasp\$Mndesktop `find ./ -name '*.class'` \
    assets
#cd ..
#rm -r desktop
