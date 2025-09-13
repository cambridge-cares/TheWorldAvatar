# SeaLevelImpactAgent

## 1. Description

The SeaLevelImpactAgent is an agent that
1) Receives inputs - `SSP Scenario`, `Projection Year`, `Confidence Level`, `Percentage Quantile`, 
2) Instatiate sea level change impacts on:
```
- Buildings 
- Landplots
- OpenStreetMap road network
- Population At Risk
- Monuments
- Heritage Trees
- Historic Sites
- Museums
- Tourist Attractions
```
3) Create geoserver layer for each sealevelprojections

## 2. Prerequisites
This agent is developed as part of the [Singapore-sea-level-rise stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/Singapore-sea-level-rise). 

Data in the [Singapore-sea-level-rise stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/Singapore-sea-level-rise) needs to be uploaded by stack-data-uploader before running this agent.

### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

## 3. Agent Configuration
### 3.1 Config Properties
The [Config.properties](inputs/config.properties) file contain the table name for the different datasets. A default value is set for each parameters following the stack-data-uploader table names specified in [Singapore-sea-level-rise stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/Singapore-sea-level-rise).
1) `dbName` - Specify the postgresql database
2) `buildingsMatViewName` - Specify the table name for CityDB buildings footprint
3) `heritagetreesTable` - Specify the table name for heritage tree
4) `historicsitesTable` - Specify the table name for historic site
5) `monumentsTable` - Specify the table name for monument 
6) `museumsTable` - Specify the table name for museum
7) `touristattractionsTable` - Specify the table name for tourist attraction
8) `landplotTable` - Specify the table name for landplot
9) `populationTable` - Specify the table name for population
10) `osm_streetTable` - Specify the table name for OpenStreetMap road newtwork

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

### 5.1 Retrieving SeaLevelImpactAgent's image

The SeaLevelImpactAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/sealevelimpactagent) using `docker pull ghcr.io/cambridge-cares/sealevelimpactagent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the SeaLevelImpactAgent Docker container to be deployed in the stack. To do so, place [sealevelimpactagent.json](stack-manager-config/inputs/config/services/sealevelimpactagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent

The agent is reachable at the `/slrimpact` endpoint.

## 6. Inputs
### 6.1 Input specification
1) `ssp` - Specify the Shared Socioeconomic Pathways (SSPs), which can take in value `ssp119`,`ssp126`,`ssp245`,`ssp370`,`ssp585`
2) `projectionyear` - Specify the projection year of the sea-level rise, which be in the interval of 10 years from `2020` to `2150`
3) `confidence` - Specify the confidence, which can take in value `medium`, `low`
4) `quantile` - Specify the percentile quantile, which can take in the value `5`, `17`, `50`, `83`, `95`


### 6.2 Single input
To run the agent, simply run the following cURL command in the format below:
```bash
# Sample request with Shared Socioeconomic Pathway - ssp585, projection year - 2150, confidence - low
curl -X POST "localhost:3838/sealevelimpactagent/slrimpact?ssp=ssp585&projectionyear=2150&confidence=medium&quantile=83"
```

### 6.3 Bulk inputs 
To run send multiple requests to the agents, you can specify the inputs at [`input_request.csv`](inputs/input_request.csv), and run the following commands to bulk execute the specified scenarios: 
```bash
./bulkstart.sh 
```

## 7. Debugging

### 7.1 Building Docker Image

In the same directory as this README, run `docker compose build`. This will build the SeaLevelImpactAgent local Docker Image.

### 7.2 Spinning up with stack-manager

To debug the agent, replace [`sealevelimpactagent-debug.json`](stack-manager-config/inputs/config/services/sealevelimpactagent-debug.json) instead of [`sealevelimpactagent.json`](stack-manager-config/inputs/config/services/sealevelimpactagent.json) in the [stack-manager config directory].

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.