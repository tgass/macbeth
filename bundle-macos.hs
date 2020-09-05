#!/bin/bash    

set -x

BUNDLE_DIR="macbeth-macos"/`stack path --dist-dir`"/build/Macbeth.app"

stack build --stack-yaml=stack-bundle-macos.yaml

xattr -cr $BUNDLE_DIR
find $BUNDLE_DIR -iname .DS_Store -delete

# create dmg file
hdiutil create -volname Macbeth -srcfolder $BUNDLE_DIR -ov -format UDZO Macbeth.dmg

