# Carpark Input Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the carparks located in Singapore. Its only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [Timeseries Client](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) and the [Remote Store Client](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/query/RemoteStoreClient.java)
from the JPS_BASE_LIB to interact with both the KG and database to mantain the KG instances and timeseries. In addition, the agent will instantiate the carpark's geolocation information in postGIS and Geoserver via the [GDAL Client](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-clients/src/main/java/com/cmclinnovations/stack/clients/gdal/GDALClient.java) and [Geoserver Client](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-clients/src/main/java/com/cmclinnovations/stack/clients/geoserver/GeoServerClient.java). The agent is also able to interact with the [Building Identification Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BuildingIdentificationAgent) to match the carparks to their nearest building based on the carpark's geolocation information (latitude, longitude etc).

## Carpark API
The carpark information are retrieved via two different APIs.

The first API allows the retrieval of carpark avaliable lots. More information can be found at [LTA Data Mall](https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html). This API requires registration and generation of an API key.

The second API allows the retrieval of carpark ratings. More information can be found at [data gov sg](https://beta.data.gov.sg/collections/325/view).

## Property files
For running the agent, three property files are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for the carpark APIs](#api-properties) defining the properties needed to access the API.

### Agent properties
The `agent.properties` file contains the following content:
```
carpark.mapping.folder=CARPARK_AGENT_MAPPINGS
building.identification.agent.endpoint=http://<Building Identification Agent endpoint>:<Port>/buildingidentificationagent/postgis
```
`CARPARK_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. 
`building.identification.agent.endpoint` is the endpoint of the Building Identification Agent.
A sample file is found at `config/agent.properties`. 

### Client properties
The client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph used to store the triples and the postGIS database used to store the timeseries data. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the postGIS database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint (optional)
- `sparql.password` the password to access the SPARQL endpoint (optional)

More information can be found in the example property file `client.properties` in the `config` folder.

### API properties
The API property file contains the API endpoints and tokens required to access and retrieve data via the API, the file should contain the following keys:
- `carpark.api.lot.endpoint` the API endpoint to retrieve the available lots for all carparks
- `carpark.api.lot.token` API token to access the API for carpark lots
- `carpark.api.pricing.endpoint` the API endpoint to retrieve the parking rates for all carparks

More information can be found in the example property file `api.properties` in the `config` folder.

## Geolocation data configurations
In the `Dockerfile`, there are three variables that affects where the geolocation information will be uploaded to in postGIS and the Geoserver:
- `LAYERNAME` the name of the layer in Geoserver, this will also be the name of the table in the postGIS database that will be used to store the geolocation information
- `DATABASE` the name of the database in postGIS to store the geolocation information
- `GEOSERVER_WORKSPACE` the name of the workspace in Geoserver to store the geolocation information

More information can be found in the `Dockerfile` located in the same directory as this README.

Modify `ontop.obda` located at `/CarparkAPIInputAgent/src/main/resources` accordingly if `LAYERNAME` has been modified.

## Building the Carpark Agent
The Carpark Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You will need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

The agent is designed to function as part of the stack.

### Stack Deployment

Modify [api.properties](#api-properties) and [client.properties](#client-properties) in the `config` folder accordingly. The sparql endpoints and postGIS database indicated in the [client.properties](#client-properties) needs to be created manually beforehand. Modify the variables in the `Dockerfile` accordingly [Geolocation data configurations](#geolocation-data-configurations).

Open up the command prompt in the same directory as this README, run the command below to build the docker image:
```
docker compose build
```
Open `stack-manager-input-config-service/carpark-agent.json` and under the `Mounts` section, modify the `Source` and insert the filepath of where the `config` folder is located at (For Windows users using WSL on Docker, the file path should start with `/mnt/c/`, which is equivalent to `C://`).

Copy `stack-manager-input-config-service/carpark-agent.json` to the services folder under your stack-manager directory (By default it should be `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`) and start up the stack.

### Run the agent

The agent has three routes, a status route, a create route and a retrieve route. 

#### Status route

This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:3838/carpark-agent/status
```
and it should return:

{"Result":"Agent is ready to receive requests."}

#### Create route
This request instantiates the ABoxes for the carparks based on [ontoCarpark](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontocarpark/OntoCarpark.owl) and matches each carpark to the closest building (within 100m) via the [Building Identification Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/BuildingIdentificationAgent). The carparks meta data are stored in the sparql endpoints indicated in the [client.properties](#client-properties) while the carpark's geolocation and matched buildings data are stored based on the locations indicated in the `Dockerfile` [Geolocation data configurations](#geolocation-data-configurations). The request has the following format:
```
curl -X POST http://localhost:3838/carpark-agent/create
```

#### Retrieve route

This request gets the latest timeseries for carpark available lots and uploads them to the knowledge graph. The timeseries data will be stored in the endpoints indicated in the [client.properties](#client-properties). The request will also initiate a scheduler to constantly run the retrieve route at a set interval. In the request, the user can set the following parameters for the scheduler:
- `delay` the time delay before the first execution of retrieve route
- `interval` the interval between successive executions 
- `timeunit` the time unit of delay and interval, this is currently limited to the following options: seconds, minutes, hours 

An example of the request in curl syntax is shown below:
```
curl -X POST --header "Content-Type: application/json" -d "{\"delay\":\"0\",\"interval\":\"180\",\"timeunit\":\"seconds\"}" http://localhost:3838/carpark-agent/retrieve
```