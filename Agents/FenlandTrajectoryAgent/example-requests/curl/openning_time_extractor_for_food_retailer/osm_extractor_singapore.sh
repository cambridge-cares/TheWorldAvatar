#!/bin/bash
curl 'https://qlever.cs.uni-freiburg.de/api/osm-planet' -Gs -H 'Accept: text/turtle' --data-urlencode "query=$(< food_retailers_singapore.sparql)" > "results/food_retailers_singapore.ttl"