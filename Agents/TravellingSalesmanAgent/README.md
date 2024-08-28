# TravellingSalesmanAgent

## 1. Description

The TravellingSalesmanAgent is an agent that

1) Retrieves Points of Interest (POI) locations from the knowledge graph.
2) Based on the specific usecases, generate geoserver layers on Travelling Salesman Problem.

## 2. Prerequisites

### 2.1. Stack Set Up

The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack. Several pre-configured examples for the different use cases for King's Lynn can be found in [stack-data-uploader-inputs](stack-data-uploader-inputs/).

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

```bash
./credentials/
        repo_username.txt
        repo_password.txt
```

## 5. Deployment

### 5.1 Retrieving TravellingSalesmanAgent's image

The TravellingSalesmanAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/travellingsalesmanagent) using `docker pull ghcr.io/cambridge-cares/travellingsalesmanagent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the TravellingSalesmanAgent Docker container to be deployed in the stack. To do so, place [travellingsalesmanagent.json](stack-manager-config/inputs/config/services/travellingsalesmanagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent

The agent is reachable at the `/runtsp` endpoint.

#### Input specification

1) `function` - The use case scenario to run the travelling salesman problem.

#### Urban Resilience Planning (UR)

Generates travelling salesman route via geoserver SQL view. this route runs through the points of interest and return back to the original location. The SQL view layers takes TWA-VF marker location as the target node for its calculations. The geoserver layers generated include:

- Normal wading depth capability
- 30cm wading depth capability
- 90cm wading depth capability

To run the agent, simply run the following cURL command:

```bash
curl -X POST "localhost:3838/travellingsalesmanagent/runtsp?function=UR"
```

## 6. Debugging

### 6.1 Building Docker Image

In the same directory as this README, run `docker compose build`. This will build the TravellingSalesmanAgent local Docker Image.

### 6.2 Spinning up with stack-manager

To debug the agent, replace [`travellingsalesmanagent-debug.json`](stack-manager-config/inputs/config/services/travellingsalesmanagent-debug.json) instead of [`travellingsalesmanagent.json`](stack-manager-config/inputs/config/services/travellingsalesmanagent.json) in the [stack-manager config directory].

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## 7. TWA-VF Visualisation

### 7.1 Feature Info Agent

1) In the directory [stack-manager-config/data/webspace/](stack-manager-config/data/webspace/), contains the TWA-VF `data.json` prepared for the different scnearios that is meant to be placed inside [`stack-manager/inputs/data/webspace`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/data), following instruction [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#example---including-a-visualisation).

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
