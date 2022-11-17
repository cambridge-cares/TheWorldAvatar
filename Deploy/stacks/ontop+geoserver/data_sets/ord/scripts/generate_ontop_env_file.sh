#!/bin/bash

SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
ONTOP_DIR=$SPATH/../../../ontop
ONTOP_INPUT_DIR=$SPATH/../../../ontop/inputs
cp $SPATH/../tbox/*.owl $ONTOP_INPUT_DIR

for file in $ONTOP_INPUT_DIR/*.owl; do
    FILE_NAME=${file##*/}
    echo $FILE_NAME
    sed -i 's/#ONTOP/ONTOP/' $ONTOP_DIR/ontop.env
    sed -i "s/[a-z .]*.owl/$FILE_NAME/g" $ONTOP_DIR/ontop.env
done