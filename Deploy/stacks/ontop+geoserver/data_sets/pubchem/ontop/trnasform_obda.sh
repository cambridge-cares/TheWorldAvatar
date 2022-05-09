#!/bin/bash
Address=../../ontop/
SPATH="$( cd  "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
echo $SPATH
${Address}transform_obda.sh $SPATH