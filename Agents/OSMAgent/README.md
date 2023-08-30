# OSMAgent
## Description
The OSMAgent is intended to do
1) Identify and match corresponding OSM data with 3D Building Data's footprint.
2) Categorize OSM data as according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) concept. 
3) Populate the following columns - building_iri, ontobuilt, propertyusage_iri, usageshare for the OSM data.
4) For building_iri with untagged propertyusage, the OSMAgent will tag it with corresponding DLM (Digitales Landschaftsmodell) landuse.  

## Pre-requisite
### Configuration
`osm_tags.csv` contains the corresponding osm tags and the ontobuilt classification.  

`dlm_landuse.csv` contains the corresponding Digitales Landschaftsmodell tags and the ontobuilt classification.

The files can be found [here](/osmagent/src/main/resources/).

### Uploading Raw data
#### OSM Data
Upload raw OSM data using [stack-data-uploader] in the a set of points and polygons of `.gml` data. The data structure and config file to upload the raw OSM data in stack-data-uploader is located in [inputs] directory. 

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) or [GeoFabrik](https://download.geofabrik.de/).
2) The `.pbf` is then to be converted into `.osm` using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) `.osm` file is then imported into QGis's using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, the points and polygons layer are exported in `.gml` format.

#### DLM (Digitales Landschaftsmodell) landuse
If unavailable, DLM files can be uploaded via stack-data-uploader in Pirmasens Digital Twin (PSDT) repository. Private repository link [here](https://github.com/cambridge-cares/pirmasens/tree/main/psdt/stack-data-uploader-inputs/data/dlm), permission may be needed. 

## To deploy this agent with the stack locally
1) Prepare the stack-manager
The agent has been implemented to work with stack, which requires the stack to be [set up](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). 

2) Input the necessary credentials in the folders

You'll need to provide  your credentials in single-word text files located like this:
#### Under the main folder
```
./OSMAgent/
    credentials/
        repo_username.txt
        repo_password.txt
```
3) Place [osmagent.json](/stack-manager-input-config/osmagent.json) into [stack-manager config directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services).

### Starting the agent
Specify in [config.properties](osmagent\src\main\resources\config.properties),
1) Database name containing both 3D buildings and OSM data
2) OSM data's schema
3) DLM's table name  

Run in command prompt:
```
curl -X POST localhost:3838/osmagent/update
```

[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[inputs]: /inputs/