# OSMAgent
## 1. Description
The OSMAgent is an agent that works with OpenStreetMap (OSM) data to link them to existing building IRI and instantiate semantic representation of building usage information from the OSM data.
The workflow of the agent can be broadly outlined in the following steps:
1) Categorize usage OSM data according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) concept. 
2) Identify and match OSM data points with the 3D buildings instantiated by the DataIntegrationAgent, in order to assign building IRI to OSM data points. This is achieved through matching the geometry of the OSM data points to the 3D buildings' footprint. 
3) Calculate building usage share for all OSM data points with tagged building IRI and non-null usage information.
4) For OSM data points with tagged building IRI but no usage information from OSM, the agent will attempt to tag it with the corresponding Digitales Landschaftsmodell (DLM) land use.  

## 2. Prerequisites
### 2.1. OntoBuiltEnvironment Classification
In the [resource folder](osmagent/src/main/resources/), there are two CSV files that govern the classification of OSM usage and DLM land use to OntoBuiltEnvironment concepts:
- `osm_tags.csv` contains the OSM tags and the corresponding OntoBuiltEnvironment classification.  
- `dlm_landuse.csv` contains the DLM tags and the corresponding OntoBuiltEnvironment classification.

### 2.2. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager README](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager/README.md) to set up the stack.

### 2.3. DataIntegrationAgent
This agent is designed to work with the data instantiated by the DataIntegrationAgent (henceforth, referred to as 3D data), and assumes the presence of 3D data inside the stack PostgreSQL. 
**Please ensure that the DataIntegrationAgent is deployed in the stack and ran first before attempting to build and run this agent.**

### 2.4. Uploading Raw Data
#### 2.4.1. OSM Data
**Upload raw OSM data in the same stack PostgreSQL database as the 3D data using [stack-data-uploader] as a set of points and polygons of `.gml` data. The schema does not have to be the same but both datasets are assumed to be in the same database.**
The data structure and config file to upload the raw OSM data in stack-data-uploader is located in [inputs] directory. 

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) or [GeoFabrik](https://download.geofabrik.de/).
2) The `.pbf` is then to be converted into `.osm` using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) `.osm` file is then imported into QGis's using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, the points and polygons layer are exported in `.gml` format.

#### 2.4.2. DLM Land Use Data
If unavailable, DLM files can be uploaded via the stack-data-uploader in Pirmasens Digital Twin (PSDT) repository. 
The link to the DLM file in PSDT is available [here](https://github.com/cambridge-cares/pirmasens/tree/main/psdt/stack-data-uploader-inputs/data/dlm). 
Please note that PSDT is a private repository, permission may be required.
**Please ensure that the DLM land use data is uploaded in the same PostgreSQL database as the 3D data and OSM data. The schema does not have to be the same but all datasets are assumed to be in the same database.**

## 3. Build
### 3.1. GitHub Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 3.2. Agent Configuration
In the [config.properties](osmagent/src/main/resources/config.properties) file, specify the following:
- Database name containing both 3D building and OSM data as `db.name`.
- Schema name containing OSM data as `osm.schema`.
- Table name (inclusive of schema) containing DLM land use data as `landuse.table`.

### 3.3. Building Docker Image
In the same directory as this README, run `docker-compose build`. This will build the OSMAgent Docker Image

## 4. Deployment
The agent has been implemented to work in the stack, which requires the OSMAgent Docker container to be deployed in the stack. To do so, place [osmagent.json](stack-manager-input-config/osmagent.json) in the [stack-manager config directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services). 
Then, run `./stack.sh start <STACK NAME>` in the [stack-manager main folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager). This will spin up the agent in the stack.
Please ensure that the stack the agent is spun up in is the same stack where the OSM and DLM data were uploaded.

## 5. Debugging
To debug the agent, place [osmagent-debug.json](stack-manager-input-config/osmagent-debug.json) instead of `osmagent.json` in the [stack-manager config directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services). 
Then, spin up with `./stack.sh start <STACK NAME>` in the [stack-manager main folder](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).
The debugger port will be available at 5005.

## 6. Running the Agent
The agent is reachable at the `/update` endpoint. No request parameters is required to run the agent at the `/update endpoint`.

To run the agent, simply run the following cURL command:
```
curl -X POST localhost:3838/osmagent/update
```

[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[inputs]: inputs/