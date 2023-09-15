# Input data
Use the stack-data-uploader to upload the `.osm`, Raster data `flood`, `elevation`, `population`.

### Elevation 
Kindly retrieve from Shin Zert from CARES. `.tif` file.

### Grid Primary Site
Downloaded from [ukpowernetworks](https://ukpowernetworks.opendatasoft.com/explore/dataset/grid-and-primary-sites/information/?disjunctive.sitename&disjunctive.powertransformercount&disjunctive.local_authority&location=14,52.76712,0.42611&basemap=jawg.light). `.csv` file. 

### Flood Data
Retrived from CMCLInnovations. `.tif` file. 

_NB this data is proprietary and given to us under creative commons license. Need to reference this and be careful not to use in commercial public demonstration_

### Population
Population Raster data is retrieved from [OpenPopGrid](http://openpopgrid.geodata.soton.ac.uk/). `.tif` file. 

Note: Currently, QGIS is used to consume the .asc file, crops desired location and exported in tif file. 

### Raw OSM data
1) [BBBike.org](https://extract.bbbike.org/) allows you to extract selected region. A file of around 3.5mb in the area around King's Lynn was found optimal. 
2) [Geofabrik](https://download.geofabrik.de/) allows you to download OSM data via region/country 

`.osm` or `pbf` file, the latter preferred .

Note: 
Downloading cropped map from BBBike.org is currently the best option. If OSM map data is downloaded from Geofabrik and subsequently cropped by using tool such as osmium or osmium will result in leaving out certain nodes, subsequently when imported via osm2pgrouting will lead to invisible/non-existent road.