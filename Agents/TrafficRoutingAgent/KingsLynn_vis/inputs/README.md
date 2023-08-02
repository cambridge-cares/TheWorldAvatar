# Input data
Use the stack-data-uploader to upload the `.osm`, Raster data `flood`, `elevation`, `population`.

### Raw OSM data
1) [BBBike.org](https://extract.bbbike.org/) allows you to extract selected region. 
2) [Geofabrik](https://download.geofabrik.de/) allows you to download OSM data via region/country 

Note: 
Downloading cropped map from BBBike.org is currently the best option. If OSM map data is downloaded from Geofabrik and subsequently cropped by using tool such as osmium or osmium will result in leaving out certain nodes, subsequently when imported via osm2pgrouting will lead to invisible/non-existent road.

### Elevation 
Kindly retrieve from Shin Zert from CARES. 

### Population
Population Raster data is retrieved from [OpenPopGrid](http://openpopgrid.geodata.soton.ac.uk/)

Note: Currently, QGIS is used to consume the .asc file, crops desired location and exported in tif file. 

### Flood Data
Retrived from CMCLInnovations. 

