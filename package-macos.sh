#!/bin/bash    

set -x

WORK_DIR=`mktemp -d`

# check if tmp dir was created
if [[ ! "$WORK_DIR" || ! -d "$WORK_DIR" ]]; then
  echo "Could not create temp dir"
  exit 1
fi

# deletes the temp directory
function cleanup {      
  rm -rf "$WORK_DIR"
  echo "Deleted temp working directory $WORK_DIR"
}

# register the cleanup function to be called on the EXIT signal
trap cleanup EXIT


BUNDLE_DIR=`stack path --dist-dir`"/build/Macbeth.app"

stack build 

# copy resources (icons, pieces, sounds) from macbeth-lib
cp -r resources/* $BUNDLE_DIR/Contents/Resources/

# prepare dmg contents
cp -r $BUNDLE_DIR $WORK_DIR

# copy LICENSE
cp LICENSE $WORK_DIR
cp resources/README.txt $WORK_DIR

xattr -cr $WORK_DIR

# cleanup
find $WORK_DIR -iname .DS_Store -delete

# create dmg file
hdiutil create -volname Macbeth -srcfolder $WORK_DIR -ov -format UDZO Macbeth.dmg


