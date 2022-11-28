#!/bin/bash

SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
pushd $SPATH
echo $(pwd) 
echo "SPATH: "$SPATH
ADDRESS=../../../ontop/
echo "ADDRESS: "$ADDRESS
${ADDRESS}transform_obda.sh $SPATH
popd