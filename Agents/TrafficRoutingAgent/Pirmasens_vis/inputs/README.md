# Pirmasens data source
## Elevation data
Can be retrieved from here
https://lvermgeo.rlp.de/de/geodaten-geoshop/opendata/

## Population data
Can be retrieved from Meta DataForGood
https://dataforgood.facebook.com/dfg/tools/high-resolution-population-density-maps

## Raw OSM data
1) [BBBike.org](https://extract.bbbike.org/) allows you to extract selected region. 
2) [Geofabrik](https://download.geofabrik.de/) allows you to download OSM data via region/country 

Note: 
Downloading cropped map from BBBike.org is currently the best option. If OSM map data is downloaded from Geofabrik and subsequently cropped by using tool such as osmium or osmium will result in leaving out certain nodes, subsequently when imported via osm2pgrouting will lead to invisible/non-existent road.