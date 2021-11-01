#!/bin/sh

cp -rf /in/server /out/

find /out -type f -name 'config.ttl' | \
  xargs sed -i \
    -e "s/BLAZEGRAPH_HOST/${BLAZEGRAPH_HOST}/g" \
    -e "s/BLAZEGRAPH_PORT/${BLAZEGRAPH_PORT}/g" \
    -e "s/ONTOP_HOST/${ONTOP_HOST}/g" \
    -e "s/ONTOP_PORT/${ONTOP_PORT}/g"