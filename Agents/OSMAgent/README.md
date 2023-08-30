# OSMAgent
## Description
The OSMAgent is an agent that works with OpenStreetMap (OSM) data to link them to existing building IRI and instantiate semantic representation of building usage information from the OSM data.
The workflow of the agent can be broadly outlined in the following steps:
1) Categorize usage OSM data according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) concept. 
2) Identify and match geometry in OSM data with 3D Building Data's footprint, in order to assign building IRI to OSM data points. 
3) Calculate building usage share for all OSM data points with tagged building IRI and non-null usage information.
4) For OSM data points with tagged building IRI but no usage information from OSM, the agent will attempt to tag it with the corresponding Digitales Landschaftsmodell (DLM) land use.  

## Prerequisite
### Configuration
In the [resource folder](osmagent/src/main/resources/), there are two CSV files that govern the classification of OSM usage and DLM land use to OntoBuiltEnvironment concepts:
- `osm_tags.csv` contains the OSM tags and the corresponding OntoBuiltEnvironment classification.  
- `dlm_landuse.csv` contains the DLM tags and the corresponding OntoBuiltEnvironment classification.

### Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager README](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md) to set up the stack.

### Uploading Raw Data
#### OSM Data
Upload raw OSM data using [stack-data-uploader] in a set of points and polygons of `.gml` data. The data structure and config file to upload the raw OSM data in stack-data-uploader is located in [inputs] directory. 

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) or [GeoFabrik](https://download.geofabrik.de/).
2) The `.pbf` is then to be converted into `.osm` using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) `.osm` file is then imported into QGis's using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, the points and polygons layer are exported in `.gml` format.

#### DLM Land Use Data
If unavailable, DLM files can be uploaded via the stack-data-uploader in Pirmasens Digital Twin (PSDT) repository. The link to the DLM file in PSDT is available [here](https://github.com/cambridge-cares/pirmasens/tree/main/psdt/stack-data-uploader-inputs/data/dlm). Please note that PSDT is a private repository, permission may be required. 

## Build
### GitHub Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### Properties File
In the [config.properties](osmagent/src/main/resources/config.properties) file, specify the following:
- Database name containing both 3D building and OSM data as `db.name`.
- Schema name containing OSM data as `osm.schema`.
- Table name (inclusive of schema) containing DLM land use data as `landuse.table`.

### Building Docker Image
In the same directory as this README, run `docker-compose build`. This will build the OSMAgent Docker Image

## Deployment
The agent has been implemented to work in the stack, which requires the OSMAgent Docker container to be deployed in the stack. To do so, place [osmagent.json](stack-manager-input-config/osmagent.json) in the [stack-manager config directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services). 
Then, run `./stack.sh start <STACK NAME>` in the [stack-manager main folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). This will spin up the agent in the stack.
Please ensure that the stack the agent is spun up in is the same stack where the OSM and DLM data were uploaded.

## Debugging
To debug the agent, place [osmagent-debug.json](stack-manager-input-config/osmagent-debug.json) instead of `osmagent.json` in the [stack-manager config directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services). 
Then, spin up with `./stack.sh start <STACK NAME>` in the [stack-manager main folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).
The debugger port will be available at 5005.

## Running the Agent
The agent is reachable at the `/update` endpoint. No request parameters is required to run the agent at the `/update endpoint`.

To run the agent, simply run the following cURL command:
```
curl -X POST localhost:3838/osmagent/update
```

[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[inputs]: inputs/