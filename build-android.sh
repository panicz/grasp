#!/bin/sh

set -x

mkdir -p build/android/bin
mkdir -p build/android/gen
mkdir -p build/android/obj

PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"

JARS="../libs/kawa.jar:../libs/android.jar:../libs/androidsvg-1.4.jar"

aapt package -f -m \
     -M "AndroidManifest.xml" \
     -J "build/android/gen" \
     -S "res" || exit

# -P $PKGNAME. -T $PKGNAME.Grasp

cd src

java -cp $JARS kawa.repl \
     --no-warn-unreachable -d ../build/android/obj -C \
     `java -jar ../libs/kawa.jar --no-warn-unreachable \
      -f analdep.scm -- --list grasp-android.scm` || exit

java -cp "$JARS:../build/android/obj" \
     kawa.repl --no-warn-unreachable -d ../build/android/obj \
     -P $PKGNAME. -T $PKGNAME.GRASP \
     -C grasp-android.scm || exit
cd ..

d8 --min-api 23 --lib libs/android.jar \
   `find build/android/obj -name '*.class'` libs/kawa.dex \
    libs/androidsvg-1.4.dex || exit

mv classes.dex build/android/bin/

aapt package -f \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F build/android/bin/"$PKGNAME.apk" || exit

cd build/android/bin

aapt add -f "$PKGNAME.apk" classes.dex || exit

zipalign -p 4 "$PKGNAME.apk" "aligned-$PKGNAME.apk" || exit
mv "aligned-$PKGNAME.apk" "$PKGNAME.apk"

if [ ! -s ~/pland.keystore ]; then
    keytool -genkey -v -keystore ~/pland.keystore \
	    -alias pland -keyalg RSA -keysize 2048 -validity 10000
fi

jarsigner -storepass quack01 -verbose -sigalg SHA1withRSA \
	  -digestalg SHA1 -keystore ~/pland.keystore "$PKGNAME.apk" pland

mv "$PKGNAME.apk" ..

cd ..

if [ -d "$HOME/storage/downloads" ];
then
    echo "Copying $PKGNAME.apk to $HOME/storage/downloads/GRASP/"
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
fi
