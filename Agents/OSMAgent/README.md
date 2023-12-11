# OSMAgent
## 1. Description
The OSMAgent is an agent that works with OpenStreetMap (OSM) data to link them to existing building IRI and instantiate the semantic representation of building usage information from OSM data.
The workflow of the agent can be broadly outlined in the following steps:
1) Categorize OSM tags according to [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) concept. 
2) Identify and match OSM data with the 3D buildings uploaded as CityGML data and LoD0 footprint. This is performed by assigning building IRI to OSM data through matching the geometry of the OSM data to the 3D buildings' footprint. 
3) Calculate building usage share for all OSM data with tagged building IRI and non-null usage information.
4) If land use data is available, for 3D buildings without tagged OSM usage, the agent will tag it with the corresponding land use.

### 1.1 Results
After running the OSMAgent, the results can be retrieved through: 
1) The materialized view `usage.buildingusage_osm` which stores the same results of `building_iri`, `propertyusage_iri`, `ontobuilt`, `usageshare` and `name`. Storing this cached copy of SQL view allows the flexibility of linking additional OSM meta-data via ontologies whenever required. An OBDA mapping file is applied onto this materialized view to expose the content of the relational database as knowledge graph. [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent) then can perfrom cross-domain query from this information with additional data from the knowledge graph.

- `building_iri` - Refers to the IRI of the building.
- `propertyusage_iri` - Refers to the IRI of the propertyusage.
- `ontobuilt` - Refers to the [OntoBuiltEnvironment](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobuiltenv) usage category of the building.
- `usageshare` - Refers to the usage proportion of each `ontobuilt` in a building. 
- `name` - Refers to the name of the building derived from OSM data.

2) A geoserver layer with the workspace name `twa` and layer name `building_usage`, this layer contains all the information to display the buildings with [TWA-VF](https://github.com/cambridge-cares/TheWorldAvatar/tree/1671-dev-update-osmagent-to-new-building-workflow/web/twa-vis-framework) using the [data.json](stack-manager-config/data/webspace/data.json).


## 2. Prerequisites
### 2.1. OntoBuiltEnvironment Classification
In the [resource folder](osmagent/src/main/resources/), there are two CSV files that govern the classification of OSM usage and DLM land use to OntoBuiltEnvironment concepts:
- `osm_tags.csv` contains the [OSM tags](https://wiki.openstreetmap.org/wiki/Map_features) and the corresponding OntoBuiltEnvironment classification.  
- `dlm_landuse.csv` contains the Digitales Landschaftsmodell (DLM) tags and the corresponding OntoBuiltEnvironment classification.

### 2.2. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

### 2.3. CityDb
The agent works with 3D buildings uploaded from CityGML data, follow the instructions in the [stack-data-uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#citydb-data)'s README.

### 2.4. Uploading Raw Data
#### 2.4.1. OSM Data
Upload raw OSM data using [stack-data-uploader] as a set of points and polygons of `.gml` data. The data structure and config file to upload the raw OSM data in stack-data-uploader is located in [stack-data-uploader-inputs] directory. 

To prepare OSM data in `.gml` format
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) (check junk email) or [GeoFabrik](https://download.geofabrik.de/) in `.pbf` format.
2) Convert the `.pbf` file into `.osm` format using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) Import the `.osm` file  into QGIS using [QuickOSM](https://plugins.qgis.org/plugins/QuickOSM/) plugin, then export points and polygons layer as `points.gml` and `polygons.gml`.

#### 2.4.2. Digitales Landschaftsmodell (DLM) Land Use Data
DLM files can be uploaded via the stack-data-uploader in Pirmasens Digital Twin (PSDT) repository. 
The link to the DLM file in PSDT is available [here](https://github.com/cambridge-cares/pirmasens/tree/main/psdt/stack-data-uploader-inputs/data/dlm). 
Please note that PSDT is a private repository, permission may be required.

#### 2.4.3. Other Land Use Data
If other land use data, that are not the DLM files, are uploaded, please create a CSV file under the [resources folder](osmagent/src/main/resources/). The CSV file is to contain a mapping between the uploaded land use data's land use tags and the OntoBuiltEnv concepts.
Please see [dlm_landuse.csv](osmagent/src/main/resources/dlm_landuse.csv) for format. In the CSV file, the `Key` column should contain the column name for land use, `Value` should be the land use value, and `OntoBuiltEnv` should be the corresponding OntoBuiltEnv concept that `Value` is mapped to; `Comment` column is optional.
Please also update `landuse.csv` in [config.properties](osmagent/src/main/resources/config.properties) to use the correct CSV file with the appropriate mapping for the land use data uploaded. 

#### Important note
The following datasets must exists in the same PostgreSQL database, they are allowed to have different schemas.  
1)  DLM land use data
2)  3D buildings 
3)  OSM data

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
- `db.name` - Database name containing both 3D building, OSM data and Landuse data (Optional). 
Default value - `postgres` is set to according to the database name specified in [osmagent_data.json](stack-data-uploader-inputs/config/osmagent_data.json). Change `db.name` if [osmagent_data.json](stack-data-uploader-inputs/config/osmagent_data.json) database value is changed.
- `osm.schema` - Schema name containing OSM data. 
Default value - `public` is set to the schema specified in [osmagent_data.json](stack-data-uploader-inputs/config/osmagent_data.json). Change `osm.schema` and [`building_usage.obda`](osmagent/src/main/resources/building_usage.obda) if [osmagent_data.json](stack-data-uploader-inputs/config/osmagent_data.json) schema is changed.
- `landuse.table` - Table name (inclusive of schema) containing land use data. Default value is set to `public.dlmsie02f` as per uploaded via psdt [here](https://github.com/cambridge-cares/pirmasens/blob/main/psdt/stack-data-uploader-inputs/config/dlm.json). Leave empty if there is no land use data available, no land use matching will be run.

## 4. Deployment
### 4.1 Retrieving OSMAgent's image
The OSMAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/osmagent) using `docker pull ghcr.io/cambridge-cares/osmagent:<LATEST-VERSION>`

### 4.2 Starting with the stack-manager
The agent has been implemented to work in the stack. To do so, place [osmagent.json](stack-manager-config/inputs/config/services/osmagent.json) in the [stack-manager config directory]. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 4.3 Running the Agent
The agent is reachable at the `/update` endpoint. No request parameters is needed.

To run the agent, run the following cURL command:
```
curl -X POST localhost:3838/osmagent/update
```

## 5. Debugging
### 5.1 Building Docker Image
In the same directory as this README, run `docker compose build`. This will build the OSMAgent local Docker Image.

### 5.2 Starting with the stack-manager
To debug the agent, replace [`osmagent-debug.json`](stack-manager-config/inputs/config/services/osmagent-debug.json) instead of [`osmagent.json`](stack-manager-config/inputs/config/services/osmagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.


## 6. TWA-VF Visualization
The result of OSMAgent - Building Usages is designed to be compatible with TWA-VF and queryable via FeatureInfoAgent. 

#### Setting up FIAgent
1) Place [`building_usage.sparql`](stack-manager-config/data/fia-queries/queries/building_usage.sparql) and [`fia-config.json`](stack-manager-config/data/fia-queries/queries/fia-config.json) inside [`stack-manager/inputs/data/queries`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data) as according the volume path specified in the stack-manager config's [`feature-info-agent.json`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/FeatureInfoAgent/sample/feature-info-agent.json).
2) Spin FeatureInfoAgent up along with the [stack-manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-the-feature-info-agent).

#### Setting up TWA-VF
1) Place [`data.json`](stack-manager-config/data/webspace/data.json) inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation) in the stack-manager.


#### To-Do in the future
Current approach of SPARQL query in [`building_usage.sparql`](stack-manager-config/data/fia-queries/queries/building_usage.sparql) involves processing an IRI string for visualisation purposes. This is done here as a work-around for performance reason as querying the same information semantically from the KG is dramatically slower. Note that extracting information from IRI strings is generally unacceptable and should not be copied or imitated. When performance issue is resolved, the semantically correct SPARQL query can be found [here](stack-manager-config/data/fia-queries/queries/native/).


[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[stack-data-uploader-inputs]: stack-data-uploader-inputs/
