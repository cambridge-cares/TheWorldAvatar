# IsochroneAgent
## 1. Description
The IsochroneAgent is an agent that retrieves Points of Interest (POI) locations from the knowledge graph and generates isochrones from the locations.

## 2. Prerequisites
### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

### 2.2. Uploading OSM Data via stack-data-uploader
To prepare OSM data
1) Download desired bounding box from [BBBike.org](https://extract.bbbike.org/) (check junk email) or [GeoFabrik](https://download.geofabrik.de/) in `.pbf` format.
2) Convert the `.pbf` file into `.osm` format using [osmconvert](https://wiki.openstreetmap.org/wiki/Osmconvert). 
3) `.osm` uploaded via stack-data-uploader in `osm2pgrouting` data type, following the instructions in the [stack-data-uploader]'s README.

### 2.3. Uploading population data via stack-data-uploader

## 3. Agent Configuration 
Copy the content of the desired function - [15 Minute Smart City (15MSC)](inputs-example/15MSC/) or [Urban Resillience](inputs-example/UR/) into the [inputs](inputs/) folder. The files have been pre-configured for the agent to run.

### 3.1 Config Properties
Things to configure in table: 
1) `Upper time limit` in minutes. 
2) `Time interval` in minutes. 
3) `Population table` all the population table name. 

### 3.2 Modification to achieve other purposes
Modify SPARQL queries as according to the POI you wish to query.
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
The agent container makes use of bind mounts to read in configuration details, please ensure that ```Source``` under ```Mounts``` in [isochroneagent.json](stack-manager-input-config/isochroneagent.json) is replaced with the absolute path to [resources folder](isochroneagent/src/main/resources).
Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.
Please ensure that the stack the agent is spun up in is the same stack where the OSM and DLM data were uploaded.

## 6. Debugging
To debug the agent, replace [`isochroneagent-debug.json`](stack-manager-input-config/isochroneagent-debug.json) instead of [`isochroneagent.json`](stack-manager-input-config/isochroneagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. Running the Agent
The agent is reachable at the `/update` endpoint. No request parameters is required to run the agent at the `/update` endpoint.

To run the agent, simply run the following cURL command:
```
curl -X POST localhost:3838/isochroneagent/update
```


[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[inputs]: stack-data-uploader-inputs/
