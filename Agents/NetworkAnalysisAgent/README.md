# NetworkAnalysisAgent
## 1. Description
The NetworkAnalysisAgent is an agent that
1) Retrieves Points of Interest (POI) locations from the knowledge graph.
2) Calculate the trip centrality tables by applying Betweeness Centrality method.
3) Generate geoserver layer that indicates the usage of the road network.

## 2. Prerequisites
### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

## 3. Agent Configuration 
### 3.1 Config Properties
1) `kgEndpoint` - The blazegraph endpoint for retrieval of POI information. Blazegraph endpoint can be on the same stack or different stack. When left blank, it is assumed to be on the same stack.

### 3.2 SPARQL Queries
SPARQL queries are used to retrieve the locations of POI. 

The SPARQL queries follow the format which requires the returned variable to be in this format: 
1) `poi_iri` refers to the POI's iri. 
2) `poi_type` refers to the POI's iri type. 
3) `geometry` refers to the WKT literals of the POI location in EPSG 4326. 

SPARQL queries are created for [UR in King's Lynn](inputs/UR/POIqueries/) use cases.

### 3.3 EdgeTableSQL
[EdgeTableSQL](https://docs.pgrouting.org/2.5/en/pgRouting-concepts.html#description-of-the-edges-sql-query-for-dijkstra-like-functions) describes the characteristic of the road networks. It represents the cost table used for routing calculations. EdgeTableSQL have been generated for [Urban Resillience (UR)](inputs/UR/edgesSQLTable/) use case in King's Lynn.

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
### 5.1 Retrieving NetworkAnalysisAgent's image
The NetworkAnalysisAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/networkanalysisagent) using `docker pull ghcr.io/cambridge-cares/networkanalysisagent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the NetworkAnalysisAgent Docker container to be deployed in the stack. To do so, place [networkanalysisagent.json](stack-manager-config/inputs/config/services/networkanalysisagent.json) in the [stack-manager config directory]. Replace `<REPLACE_WITH_YOUR_DIRECTORY>` of the bind mount with absolute path to the networkanalysisagent's inputs directory.   

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent
The agent is reachable at the `/runtc` endpoint.
#### Input specification
1) `function` - The use case scenario to run the trip centrality analysis.

To run the agent, simply run the following cURL command:

#### Urban Resilience Planning (UR)
```
curl -X POST "localhost:3838/networkanalysisagent/runtc?function=UR"
```

## 6. Debugging
### 6.1 Building Docker Image
In the same directory as this README, run `docker compose build`. This will build the NetworkAnalysisAgent local Docker Image. 

### 6.2 Spinning up with stack-manager
To debug the agent, replace [`networkanalysisagent-debug.json`](stack-manager-config/inputs/config/services/networkanalysisagent-debug.json) instead of [`networkanalysisagent.json`](stack-manager-config/inputs/config/services/networkanalysisagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. TWA-VF Visualisation
1) In the directory [stack-manager-config/data/webspace/](stack-manager-config/data/webspace/), contains the TWA-VF `data.json` prepared for the different scnearios that is meant to be placed inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation).


[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[inputs]: stack-data-uploader-inputs/
