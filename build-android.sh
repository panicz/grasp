#!/bin/sh

set -x

mkdir -p build/android/bin
mkdir -p build/android/gen
mkdir -p build/android/obj

PKGNAME="$(grep -o "package=.*" AndroidManifest.xml | cut -d\" -f2)"

JARS="../libs/kawa.jar:../libs/android.jar:../libs/androidsvg-1.4.jar"

while [ "$#" -gt 0 ]; do
  case "$1" in
    -i) INIT="$2"; shift 2;;
    --init=*) INIT="${1#*=}"; shift 1;;
    *) break
  esac
done

if [ -z "$INIT" ]; then
    INIT="init/init.scm"
fi

# aapt package -I libs/android.jar -f -m \
#      -M "AndroidManifest.xml" \
#      -J "build/android/gen" \
#      -S "res" || exit

# -P $PKGNAME. -T $PKGNAME.Grasp

cd src

java -cp $JARS kawa.repl \
     --no-warn-unreachable -d ../build/android/obj -C \
     `java -jar ../libs/kawa.jar --no-warn-unreachable \
      -f analdep.scm -- --list grasp-android.scm` $@ || exit

java -cp "$JARS:../build/android/obj" \
     kawa.repl --no-warn-unreachable -d ../build/android/obj \
     -P $PKGNAME. -T $PKGNAME.GRASP \
     -C grasp-android.scm || exit
cd ..

java -cp libs/d8.jar com.android.tools.r8.D8 \
     --min-api 23 --lib libs/android.jar \
   `find build/android/obj -name '*.class'` libs/kawa.dex \
    libs/androidsvg-1.4.dex || exit

mv classes.dex build/android/bin/

cp $INIT assets/init.scm

aapt package -I tools/android.jar -f \
       	-M AndroidManifest.xml \
       	-S res \
       	-A assets \
       	-F build/android/bin/"$PKGNAME.apk" || exit

rm assets/init.scm

cd build/android/bin

aapt add -f "$PKGNAME.apk" classes.dex || exit

java -jar ../../../libs/zipalign.jar "$PKGNAME.apk" "aligned-$PKGNAME.apk" || exit

#zipalign -p 4 "$PKGNAME.apk" "aligned-$PKGNAME.apk" || exit


if [ ! -s ~/pland.keystore ]; then
    keytool -genkey -v -keystore ~/pland.keystore \
	    -alias pland -keyalg RSA -keysize 2048 -validity 10000
fi

java -jar ../../../libs/apksigner.jar sign --ks ~/pland.keystore \
	  --ks-key-alias pland --ks-pass pass:quack01 \
     "aligned-$PKGNAME.apk"

mv "aligned-$PKGNAME.apk" "$PKGNAME.apk"

# jarsigner -storepass quack01 -verbose -sigalg SHA1withRSA \
# 	  -digestalg SHA1 -keystore ~/pland.keystore "$PKGNAME.apk" pland

mv "$PKGNAME.apk" ..

cd ..

if [ -d "$HOME/storage/downloads" ];
then
    echo "Copying $PKGNAME.apk to $HOME/storage/downloads/GRASP/"
    mkdir -p "$HOME/storage/downloads/GRASP"
    cp "$PKGNAME.apk" "$HOME/storage/downloads/GRASP/"
fi
