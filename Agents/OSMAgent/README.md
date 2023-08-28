# OSMAgent
## Description
The OSMAgent is intended to do
1) Building matching from building footprint.
2) Categorize OSM data as according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv). 
3) Populate the following columns - building_iri, ontobuilt, propertyusage_iri, usageshare for each of the OSM items.
4) For building_iri with untagged propertyusage, the OSMAgent will tag it with corresponding OSM landuse.  

## Pre-requisite
### Configuration
`osm_tags.csv` contains the corresponding osm tags and the ontobuilt classification.  

`osm_landuse.csv` contains the corresponding osm landuse tags and the ontobuilt classification.

The files can be found [here](/Agents/OSMAgent/osmagent/src/main/resources/).

### Uploading Raw data
Upload raw OSM data using [stack-data-uploader] in the a set of points and polygons of `.gml` data. The data structure and config file to upload the raw OSM data in stack-data-uploader is in [inputs]. 

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) or [GeoFabrik](https://download.geofabrik.de/).
2) The `.pbf` is then to be converted into `.osm` using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) `.osm` file is then imported into QGis's using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, the points and polygons layer are exported in `.gml` format.

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

### Starting the agent
Run in command prompt
```
curl -X POST localhost:10102/osmagent/update
```

[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[inputs]: /Agents/OSMAgent/inputs/