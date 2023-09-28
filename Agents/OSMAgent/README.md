# OSMAgent
## 1. Description
The OSMAgent is an agent that works with OpenStreetMap (OSM) data to link them to existing building IRI and instantiate the semantic representation of building usage information from OSM data.
The workflow of the agent can be broadly outlined in the following steps:
1) Categorize OSM tags according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) concept. 
2) Identify and match OSM data with the 3D buildings uploaded as CityGML data and LoD0 footprint as extracted by the [DataIntegrationAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-1524-integration-data-2/Agents/DataIntegrationAgent). To assign building IRI to OSM data, this is carried out through matching the geometry of the OSM data to the 3D buildings' footprint. 
3) Calculate building usage share for all OSM data with tagged building IRI and non-null usage information.
4) If land use data is available, for 3D buildings without tagged OSM usage, the agent will tag it with the corresponding land use.  

## 2. Prerequisites
### 2.1. OntoBuiltEnvironment Classification
In the [resource folder](osmagent/src/main/resources/), there are two CSV files that govern the classification of OSM usage and DLM land use to OntoBuiltEnvironment concepts:
- `osm_tags.csv` contains the OSM tags and the corresponding OntoBuiltEnvironment classification.  
- `dlm_landuse.csv` contains the DLM tags and the corresponding OntoBuiltEnvironment classification.

### 2.2. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

### 2.3. CityDb
The agent works with 3D buildings uploaded from CityGML data, follow the instructions in the [stack-data-uploader]'s README.

### 2.4. DataIntegrationAgent
This agent is designed to work with the LoD0 footprint extracted by the DataIntegrationAgent, and assumed the presence of the 3D buildings and LoD0 footprint inside the stack PostgreSQL. 
**Please ensure that the DataIntegrationAgent is deployed in the stack and ran first before building and running this agent.**

### 2.5. Uploading Raw Data
#### 2.5.1. OSM Data
Upload raw OSM data in the same stack PostgreSQL database as the 3D buildings using [stack-data-uploader] as a set of points and polygons of `.gml` data.
The data structure and config file to upload the raw OSM data in stack-data-uploader is located in [inputs] directory. 
Ensure that the database specified in [osm.json](inputs/config/osm.json) is the same database where the 3D buildings is located.

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) (check junk email) or [GeoFabrik](https://download.geofabrik.de/) in `.pbf` format.
2) Convert the `.pbf` file into `.osm` format using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) Import the `.osm` file  into QGIS using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, then export points and polygons layer as `points.gml` and `polygons.gml`.

#### 2.5.2. Digitales Landschaftsmodell (DLM) Land Use Data
If unavailable within the database, DLM files can be uploaded via the stack-data-uploader in Pirmasens Digital Twin (PSDT) repository. 
The link to the DLM file in PSDT is available [here](https://github.com/cambridge-cares/pirmasens/tree/main/psdt/stack-data-uploader-inputs/data/dlm). 
Please note that PSDT is a private repository, permission may be required.

**Please ensure that the DLM land use data is uploaded in the same PostgreSQL database as the 3D buildings and OSM data. 3D Buildings, OSM data and DLM data can have different schemas however all datasets are assumed to be in the same PostgresSQL database.**

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
- `db.name` - Database name containing both 3D building and OSM data. 
Default is set to the database value specified in [osm.json](inputs/config/osm.json). Change `db.name` if [osm.json](inputs/config/osm.json) database value is changed.
- `osm.schema` - Schema name containing OSM data. 
Default is set to the schema value specified in [osm.json](inputs/config/osm.json). Change `osm.schema` and [`building_usage.obda`](FeatureInfoAgent/building_usage.obda) if [osm.json](inputs/config/osm.json) schema value is changed.
- `landuse.table` -  Table name (inclusive of schema) containing DLM land use data as . 
Leave empty if there is no land use data available, no land use matching will be run.

### 3.3. Building Docker Image
In the same directory as this README, run `docker compose build`. This will build the OSMAgent Docker Image

## 4. Deployment
The agent has been implemented to work in the stack, which requires the OSMAgent Docker container to be deployed in the stack. To do so, place [osmagent.json](stack-manager-input-config/osmagent.json) in the [stack-manager config directory]. 
The agent container makes use of bind mounts to read in configuration details, please ensure that ```Source``` under ```Mounts``` in [osmagent.json](stack-manager-input-config/osmagent.json) is replaced with the absolute path to [resources folder](osmagent/src/main/resources).
Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.
Please ensure that the stack the agent is spun up in is the same stack where the OSM and DLM data were uploaded.

## 5. Debugging
To debug the agent, replace [`osmagent-debug.json`](stack-manager-input-config/osmagent-debug.json) instead of [`osmagent.json`](stack-manager-input-config/osmagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 6. Running the Agent
The agent is reachable at the `/update` endpoint. No request parameters is required to run the agent at the `/update` endpoint.

To run the agent, simply run the following cURL command:
```
curl -X POST localhost:3838/osmagent/update
```

## 7. TWA-VF Visualization
The result of OSMAgent - Building Usages is designed to be compatible with TWA-VF and queryable via FeatureInfoAgent. 

Steps: 
1) Upload [building_usage.obda](FeatureInfoAgent/building_usage.obda) via stack-data-uploader.
2) Run the SPARQL Update in [building_class.sparql](FeatureInfoAgent/building_class.sparql).
3) Place [building_usage.sparql](FeatureInfoAgent/queries/building_usage.sparql) and [fia-config.json](FeatureInfoAgent/queries/fia-config.json) into FeatureInfoAgent's [queries](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent/queries) subfolder.
4) Spin FeatureInfoAgent up along with the [stack-manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-the-feature-info-agent).

### To-Do in the future
Current approach of SPARQL query in [building_usage.sparql](FeatureInfoAgent/queries/building_usage.sparql) involves processing an IRI string for visualisation purposes. This is done here as a work-around for performance reason as querying the same information semantically from the KG is dramatically slower. Note that extracting information from IRI strings is generally unacceptable and should not be copied or imitated. When performance issue is resolved, the semantically correct SPARQL query can be found [here](FeatureInfoAgent/native/).


[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[inputs]: inputs/
