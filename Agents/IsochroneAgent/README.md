# IsochroneAgent
## 1. Description
The IsochroneAgent is an agent that
1) Retrieves Points of Interest (POI) locations from the knowledge graph.
2) Generates isochrones from the locations with different mode of transport and or road conditions as according to OntoIsochrone. 
3) Performs 15 Minute Smart City (15MSC) and Urban Resillience (UR) planning. 
4) Instantiates the isochrones via OBDA mapping. 

## 2. Prerequisites
### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

### 2.2. Uploading OSM Data via stack-data-uploader
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) (check junk email) or [GeoFabrik](https://download.geofabrik.de/) in `.pbf` format.
2) `.pbf` uploaded via [stack-data-uploader] in [osm2pgrouting](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#osm-data) data type.

### 2.3. Uploading population data via stack-data-uploader
1) Download High Resolution Population Density Maps from [HDX - Meta Data For Good](https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates?).
2) Configuration to upload population data using [stack-data-uploader] in King's Lynn and Pirmasens can be found in [stack-data-uploader-inputs](stack-data-uploader-inputs/).

## 3. Agent Configuration 
### 3.1 Config Properties
Items to configure in [config.properties](inputs/config.properties): 
1) `timeThreshold` - The time cutoff of an isochrone in minutes. Default value is set at 15 minutes. 
2) `timeInterval` - The time increment value of each isochrone in minutes. Default value is set at 5 mintues time interval. 
3) `populationTables` - The exact table names of the population tables should follow the names of population table as per [uploaded via stack-data-uploader](#23-uploading-population-data-via-stack-data-uploader). 

### 3.2 SPARQL Queries
SPARQL queries are used to retrieve the locations of POI. 

The SPARQL queries follow the format which requires the returned variable to be in this format: 
1) `poi_iri` refers to the POI's iri. 
2) `poi_type` refers to the POI's iri type. 
3) `geometry` refers to the WKT literals of the POI location. 

SPARQL queries are created for [15MSC in Pirmasens](inputs/15MSC/POIqueries/) and [UR in King's Lynn](inputs/UR/POIqueries/) use cases.

To use the example created in [15MSC in Pirmasens](inputs/15MSC/POIqueries/), replace `[ONTOP]` with your Ontop endpoint as per the format below: 
```
<http://<STACK-NAME>-ontop:8080/sparql/>
```

### 3.3 EdgeTableSQL
[EdgeTable](https://docs.pgrouting.org/2.5/en/pgRouting-concepts.html#description-of-the-edges-sql-query-for-dijkstra-like-functions) describes the characteristic of the road networks. It is used to define the transport mode and road conditions during the calculation of isochrone. 

EdgeTableSQL follows the following format `TransportMode_RoadConditions.sql`.
1) `TransportMode` and `RoadConditions` refers to the ontology classes developed in [OntoIsochrone](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontoisochrone/OntoIsochrone.owl).
2) The SQL statement content refers to the cost table used for routing calculations. 

EdgeTableSQL are created for [15MSC in Pirmasens](inputs/15MSC/edgesSQLTable/) and [UR in King's Lynn](inputs/UR/edgesSQLTable/) use cases. 

## 4. Build
### 4.1. GitHub Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

## 5. Deployment

### 5.1 Building Docker Image
In the same directory as this README, run `docker compose build`. This will build the IsochroneAgent Docker Image. 

### 5.2 Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the IsochroneAgent Docker container to be deployed in the stack. To do so, place [isochroneagent.json](stack-manager-input-config/isochroneagent.json) in the [stack-manager config directory]. Replace `<REPLACE_WITH_YOUR_DIRECTORY>` of the bind mount with absolute path to the isochroneagent's inputs directory. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent
The agent is reachable at the `/update` endpoint. `function` is used to indicate the type. 

To run the agent, simply run the following cURL command:

#### 15 Minute Smart City (15MSC)
```
curl -X POST localhost:3838/isochroneagent/update?function=15MSC
```
#### Urban Resilience Planning (UR)
```
curl -X POST localhost:3838/isochroneagent/update?function=UR
```


## 6. Debugging
To debug the agent, replace [`isochroneagent-debug.json`](stack-manager-input-config/isochroneagent-debug.json) instead of [`isochroneagent.json`](stack-manager-input-config/isochroneagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. TWA-VF Visualisation
### 7.1 Feature Info Agent
The isochrones is designed to be compatible with TWA-VF and queryable via FeatureInfoAgent.

1) In the directory [twa-vf/15MSCPirmasens/webspace](twa-vf/15MSCPirmasens/webspace/), contains `data.json` prepared for TWA-VF that is meant to be placed inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation).
2) In the directory [twa-vf/FeatureInfoAgent](twa-vf/FeatureInfoAgent/queries/), contains `SPARQL queries` and `fia-config.json` to be used with the agent [FeatureInfoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent#configuration).  Place the `fia-config.json` and `isochrone.sparql` inside `stack-manager/inputs/data/queries` as according the volume path specified in the stack-manager config's [`feature-info-agent.json`](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/FeatureInfoAgent/sample/feature-info-agent.json).




[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[inputs]: stack-data-uploader-inputs/
