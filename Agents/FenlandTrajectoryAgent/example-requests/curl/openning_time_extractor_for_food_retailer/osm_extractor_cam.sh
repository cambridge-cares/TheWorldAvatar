#!/bin/bash
curl 'https://qlever.cs.uni-freiburg.de/api/osm-planet' -Gs -H 'Accept: text/turtle' --data-urlencode "query=$(< opening_hours_cam.sparql)" > "results/opening_hours_cam.ttl"