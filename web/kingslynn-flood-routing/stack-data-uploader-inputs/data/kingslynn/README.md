# Input data
Use the stack-data-uploader to upload the `.osm`, Raster data `flood`, `elevation`, `population`.

### Elevation 
Detailed elevation can be retrieved from CARES. `.tif` file. Otherwise, it can be downloaded from [USGS EROS Archive - Digital Elevation](https://www.usgs.gov/centers/eros/science/usgs-eros-archive-digital-elevation-shuttle-radar-topography-mission-srtm-1#overview).

### Grid Primary Site
Downloaded from [ukpowernetworks](https://ukpowernetworks.opendatasoft.com/explore/dataset/grid-and-primary-sites/information/?disjunctive.sitename&disjunctive.powertransformercount&disjunctive.local_authority&location=14,52.76712,0.42611&basemap=jawg.light). `.csv` file. 

### Flood Data
Retrived from CMCLInnovations. `.tif` file. 

_FLood data is proprietary and provided under creative commons license._

### Population
Population Raster data is retrieved from [OpenPopGrid](http://openpopgrid.geodata.soton.ac.uk/). `.tif` file. 

Note: Currently, QGIS is used to consume the .asc file, crops desired location and exported in tif file. 

### Routing
1) [BBBike.org](https://extract.bbbike.org/) allows you to extract selected region. A file of around 3.5mb in the area around King's Lynn was found optimal. 
2) [Geofabrik](https://download.geofabrik.de/) allows you to download OSM data via region/country 

`.osm` or `pbf` file, the latter preferred .

Note: 
Downloading cropped map from BBBike.org is currently the best option. If OSM map data is downloaded from Geofabrik and subsequently cropped by using tool such as osmium or osmium will result in leaving out certain nodes, subsequently when imported via osm2pgrouting will lead to invisible/non-existent road.