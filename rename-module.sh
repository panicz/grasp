#!/bin/bash

SOURCE="src/$(echo $1|sed 's/ /\//g' |sed 's/[()]//g').scm"
TARGET="src/$(echo $2|sed 's/ /\//g' |sed 's/[()]//g').scm"

git mv $SOURCE $TARGET
find ./ -name '*.scm' -exec sed -i "s/$1/$2/" {} \;
