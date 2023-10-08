# IsochroneAgent
## 1. Description
The IsochroneAgent is an agent that 
1) Retrieves Points of Interest (POI) locations from the knowledge graph.
2) Generates isochrones from the locations with different mode of transport. 

## 2. Prerequisites
### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

### 2.2. Uploading OSM Data via stack-data-uploader
To prepare OSM data
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) (check junk email) or [GeoFabrik](https://download.geofabrik.de/) in `.pbf` format.
2) `.pbf` uploaded via stack-data-uploader in [osm2pgrouting](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader#osm-data)data type, following the instructions in the [stack-data-uploader]'s README.

### 2.3. Uploading population data via stack-data-uploader
To prepare population data
1) Download High Resolution Population Density Maps from [HDX - Meta Data For Good](https://data.humdata.org/dataset/germany-high-resolution-population-density-maps-demographic-estimates?).
2) Configuration to upload population data using [stack-data-uploader] in King's Lynn and Pirmasens can be found in [stack-data-uploader-inputs](stack-data-uploader-inputs/)

## 3. Agent Configuration 
### 3.1 Config Properties
Things to configure in table: 
1) `timeThreshold` the upper bounds of time. 
2) `timeInterval` in minutes. 
3) `populationTable` all the population table names as uploaded in .

### 3.2 SPARQL Queries
Modify SPARQL queries as according to the POI you wish to query.

### 3.3 
Modify the edgeTableSQL as according to the cost table per the transport mode.


## 4. Build
### 4.1. GitHub Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 4.2. Building Docker Image
In the same directory as this README, run `docker compose build`. This will build the IsochroneAgent Docker Image

## 5. Deployment
The agent has been implemented to work in the stack, which requires the IsochroneAgent Docker container to be deployed in the stack. To do so, place [isochroneagent.json](stack-manager-input-config/isochroneagent.json) in the [stack-manager config directory]. 

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.
Please ensure that the stack the agent is spun up in is the same stack where the OSM and DLM data were uploaded.

## 6. Debugging
To debug the agent, replace [`isochroneagent-debug.json`](stack-manager-input-config/isochroneagent-debug.json) instead of [`isochroneagent.json`](stack-manager-input-config/isochroneagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. Running the Agent
The agent is reachable at the `/update` endpoint. `function` is used to indicate the type 

To run the agent, simply run the following cURL command:
```
curl -X POST localhost:10105/isochroneagent/update?function=15MSC
```


[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[inputs]: stack-data-uploader-inputs/
