#!/bin/bash
set -x

if [ $# -eq 0  ]; then
    echo "Usage: $0 <file.apk/directory.dump>"
    exit
fi

case $1 in
    *.apk)
	if [ ! -f $1 ]; then
	    echo "The file $1 does not exist"
	    exit
	fi
	DIR_NAME=`dirname $1`
	BASE_NAME=`basename $1 .apk`
	DUMP_DIR=$DIR_NAME/$BASE_NAME.dump
	apktool d -m $1 -o $DUMP_DIR/apktool
	unzip $1 -d $DUMP_DIR/unzip
	;;
	
    *.dump)
	if [ ! -d $1 ]; then
	    echo "The directory $1 does not exist"
	    exit
	fi
	DIR_NAME=`dirname $1`
	BASE_NAME=`basename $1 .dump`
	DUMP_DIR=$1
	;;
    *)
	echo "The argument must either be an .apk file or a .dump directory"
	exit
	;;
esac

if [ ! -d $DUMP_DIR/apktool ]; then
    echo "The directory $DUMP_DIR/apktool does not exist"
    exit
fi

if [ ! -d $DUMP_DIR/unzip ]; then
    echo "The directory $DUMP_DIR/unzip does not exist"
    exit
fi

if [ -d $DUMP_DIR/compiled_resources ]; then
    rm -rf $DUMP_DIR/compiled_resources
fi

mkdir -p $DUMP_DIR/compiled_resources

aapt2 compile `find $DUMP_DIR/unzip/res -type f` -o $DUMP_DIR/compiled_resources
aapt2 link --proto-format \
      -o $DUMP_DIR/$BASE_NAME.proto.apk \
      -I tools/android.jar \
      --manifest $DUMP_DIR/apktool/AndroidManifest.xml \
      -R $DUMP_DIR/compiled_resources/*.flat \
      --auto-add-overlay

if [ -d $DUMP_DIR/proto ]; then
    rm -rf $DUMP_DIR/proto
fi

unzip $DUMP_DIR/$BASE_NAME.proto.apk -d $DUMP_DIR/proto

if [ -d $DUMP_DIR/base ]; then
    rm -rf $DUMP_DIR/base
fi

if [ -f $DUMP_DIR/base.zip ]; then
    rm $DUMP_DIR/base.zip
fi

mkdir -p $DUMP_DIR/base/manifest
mkdir -p $DUMP_DIR/base/dex

cp $DUMP_DIR/proto/AndroidManifest.xml $DUMP_DIR/base/manifest/
cp $DUMP_DIR/proto/resources.pb $DUMP_DIR/base/
cp $DUMP_DIR/unzip/classes.dex $DUMP_DIR/base/dex
cp -r $DUMP_DIR/proto/res $DUMP_DIR/base/
cp -r $DUMP_DIR/unzip/assets $DUMP_DIR/base/

WORKING_DIRECTORY=`pwd`
cd $DUMP_DIR/base
zip -r ../base.zip .
cd $WORKING_DIRECTORY


if [ -f build/$BASE_NAME.aab ]; then
    rm build/$BASE_NAME.aab
fi

java -jar tools/bundletool-all-1.18.1.jar build-bundle \
     --modules=$DUMP_DIR/base.zip \
     --output=build/$BASE_NAME.aab

jarsigner -keystore secret/keystore.jks build/$BASE_NAME.aab publisher

if [ -f build/$BASE_NAME.apks ]; then
    rm build/$BASE_NAME.apks
fi

java -jar tools/bundletool-all-1.18.1.jar build-apks \
     --bundle=build/$BASE_NAME.aab \
     --output=build/$BASE_NAME.apks \
     --mode=universal \
     --ks=secret/keystore.jks \
     --ks-key-alias=publisher
