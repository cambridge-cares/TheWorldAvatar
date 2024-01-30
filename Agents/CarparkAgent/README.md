# Carpark Input Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the carparks located in Singapore. Its only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) and the [remote store client](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/query/RemoteStoreClient.java)
from the JPS_BASE_LIB to interact with both the KG and database.

## Carpark API
The carpark information are retrieved via two different APIs.

The first API allows the retrieval of carpark avaliable lots. More information can be found at [LTA Data Mall](https://datamall.lta.gov.sg/content/datamall/en/dynamic-data.html). This API requires registration and generation of an API key.

The second API allows the retrieval of carpark ratings. More information can be found at [data gov sg](https://beta.data.gov.sg/collections/325/view).

### Property files
For running the agent, three property files are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for the carpark APIs](#api-properties) defining the properties needed to access the API.

#### Agent properties
The `agent.properties` file only needs to contain a single line:
```
carpark.mapping.folder=CARPARK_AGENT_MAPPINGS
```
where `CARPARK_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. 
A sample file is found at `config/agent.properties`. 

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint
- `sparql.password` the password to access the SPARQL endpoint

More information can be found in the example property file `client.properties` in the `config` folder.

#### API properties
The API properties contain the credentials to authorize access to the weather Station API (see the [API description](#Weather-Station-API)),
as well as, the url of the API and the identifier of the weather station. More specifically, the API properties file should contain the following keys:
- `carpark.api.lot.endpoint` the API endpoint to retrieve the available lots for all car parks
- `carpark.api.lot.token` API token to access the API for car park lots
- `carpark.api.pricing.endpoint` the API endpoint to retrieve the parking rates for all car parks.

More information can be found in the example property file `api.properties` in the `config` folder.

### Deployment

**1) TEST ENVIRONMENT**

Deploy the agent to execute the unit tests by running the following code in the CLI at the <root> directory.
The success of all tests must be verified through the Docker logs.
```
docker compose -f "./docker/docker-compose.test.yml" up -d --build
```

**2) PRODUCTION ENVIRONMENT**

Modify `api.properties` and `client.properties` in the config folder accordingly. Open up the command prompt in the same directory as this README, run the command below to build the docker image:
```
docker compose build
```
Open stack-manager-input-config-service/carpark-agent.json and under the Mounts section, modify the Source and insert the filepath of where the config folder is located at (For Windows users using WSL on Docker, the file path should start with /mnt/c/, which is equivalent to C://).

Copy stack-manager-input-config-service/carpark-agent.json to the services folder under your stack-manager directory (By default it should be TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/) and start up the stack.

#### Run the agent

The agent has two routes, a status route and a retrieve route. 

##### Status route

This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:3838/carpark-agent/status
```
and it should return:

{"Result":"Agent is ready to receive requests."}

##### Retrieve route

This request gets the latest timeseries for carpark available lots, instantiate the ABoxes for the carparks and it's attributes if it does not exist and updates the carpark rates in the knowledge graph. The request will also initiate a scheduler to constantly run the retrieve route at a set interval. In the request, the user can set the following parameters for the scheduler:
- `delay` the time delay before the first execution of retrieve route
- `interval` the interval between successive executions 
- `timeunit` the time unit of delay and interval, this is currently limited to the following options: seconds, minutes, hours 

An example of the request in curl syntax is shown below:
```
curl -X POST --header "Content-Type: application/json" -d "{\"delay\":\"0\",\"interval\":\"180\",\"timeunit\":\"seconds\"}" http://localhost:3838/carpark-agent/retrieve
```