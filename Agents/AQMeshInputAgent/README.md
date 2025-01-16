# AQMesh input agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the AQMesh air 
quality measuring station. It's only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS base lib to interact with both the KG and database.

Before explaining the usage of the agent, we will briefly summarize the AQMesh API that is
contacted by one of the classes in this package to retrieve data.

## AQMesh API

We will here briefly describe the AQMesh API. The official documentation can be found [here](https://www.aqmeshdata.net/api-documentation), 
but requires login credentials to access it.

### Token retrieval
Before accessing any other endpoint, a token needs to be requested that can be used as authorization to 
access data and pod / asset information.

The token can be retrieved from the following endpoint using an HTTP POST request:
```
https://api.aqmeshdata.net/api/Authenticate
```

The body of the request needs to contain the credentials, i.e. username and password, in form of a JSON object like the 
following example:
```json
{"username":"AQMeshAPIUser", "password":"AQMeshAPIPassword"}
```

The response should then contain the token in the response body if the request was successful, like shown here:

![Shows the response body of a successful authorization request.](docs/img/token_example.png "Token example")

The token will be valid for 120 minutes for using it in following requests. To use the token it needs to be added to
the header of the request (how to exactly do it depends on which software and package is used for making the request):
```
Authorization: Bearer [token]
```
where `[token]` is the token retrieved contained in the response body shown above. In java, using the Apache Http library
it can be done like this:
```java
private void setTokenAuthorization(HttpRequest request) {
        String authHeader = "Bearer " + token;
        request.setHeader(HttpHeaders.AUTHORIZATION, authHeader);
}
```

### Server status
To check whether the server is available and whether the token is correctly set, there is a ping endpoint:
```
https://api.aqmeshdata.net/api/serverping
```

When available and if the GET request is correct, the response body should look similar to the following:

![Shows the response body of a successful ping request.](docs/img/ping_example.png "Ping example")

### Retrieve location number
The AQMesh API allows you to connect multiple pods to the same user. To retrieve new data (readings) from a specific pod,
its location number (identifier) is needed. Information about the assets, including the location number, is accessible 
through the asset endpoint via an HTTP GET request:
```
https://api.aqmeshdata.net/api/Pods/Assets
```

The response body will contain a JSON array with an JSON object for each asset (pod) containing information
about the pod. The following is an example of part of the JSON object for one pod:

![Shows part of the response body of a successful asset request for one asset.](docs/img/asset_example.png "Asset example")

### Data retrieval
There are two different ways to retrieve data from the API, namely *Next* and *Repeat*. From the *Next* endpoint one 
will retrieve all data that is available since the last call to the *Next* API, while the *Repeat* call repeats the last 
*Next* call.

For example, when data is retrieved at `2020-08-23T16:17:00` using the *Next* endpoint and the last call was at 
`2020-08-16T09:10:00`, all data between these two timestamps that was recorded will be returned. Let's assume the last 
recording was at `2020-08-23T16:00:00`. From
now on (until the *Next* endpoint is used again) a *Repeat* call will also return data between `2020-08-16T09:10:00` 
and `2020-08-23T16:00:00`. In the next *Next* call all data since `2020-08-23T16:00:00` will be returned, and so on.

Note, that there is only one "pointer", i.e. the starting timestamp for readings of a *Next* call, per user and pod.
If different agents, people or pieces of code use the same credentials to get readings for the same pod, they will 
reset / renew the pointer for all following endpoint accesses!

#### The endpoint
The actual endpoint has the following structure and controls what type of data is retrieved and in which form:
```
https://api.aqmeshdata.net/api/LocationData/Next/[location]/[Params]/[Units]/[TPC]
```
where `[location]` is the number of the specific pod that can be retrieved from the 
[asset endpoint](#retrieve-location-number), `[Params]` is 1 for gas and 2 for particle readings. The `[units]` 
are two digits, the first one controlling the temperature unit (0: Fahrenheit, 1: degree Celsius) and
the second controlling the sensor measure unit (0: ppb, 1: micrograms per cubic meter). The `[TPC]` path is only applicable for
particle readings and defines whether to return TPC (1) in the output or not (0).

For the agent we use 01 (= degree Celsius and micrograms per cubic meter) for the `[Units]` part and 1 for the `[TPC]` 
part.

#### Example readings
Readings are returned in the response body in form of a JSON array with one JSON object per timestamp. The following 
shows a single JSON object example contained in gas readings with units equal to 01:

![Shows part of the response body of a successful gas readings request.](docs/img/example_gas_readings_1.png "Gas readings general")
![Shows part of the response body of a successful gas readings request.](docs/img/example_gas_readings_2.png "Gas readings sensors")
![Shows part of the response body of a successful gas readings request.](docs/img/example_gas_readings_3.png "Gas readings additional sensors")

The following shows a single JSON object example contained in particle readings with units equal to 01 and including the TPC:

![Shows part of the response body of a successful gas readings request.](docs/img/example_particle_readings_1.png "Particle readings general")
![Shows part of the response body of a successful gas readings request.](docs/img/example_particle_readings_2.png "Particle readings sensors")

## Usage 
This part of the README describes the usage of the input agent. The agent needs to be deployed as part of the stack.

### Property files
For running the agent, three property files are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how ot access the database and SPARQL endpoint.
- One [property file for the AQMesh API](#api-properties) defining the access credentials and pod to use.

#### Agent properties
The agent property file only needs to contain a single line:
```
aqmesh.mappingfolder=AQMESH_AGENT_MAPPINGS
```
`AQMESH_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. 
A sample file can be found at `config/agent.properties`.  See [this section](#mapping-files) of the README for an explanation of the mapping files.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of
the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

A sample file can be found at `config/client.properties`.

#### API properties
The API properties contain the credentials to authorize access to the AQMesh API (see the [API description](#aqmesh-api)),
as well as, the API URL and which pod index. It should contain the following keys:
- `aqmesh.username` the username to access the API.
- `aqmesh.password` the password to access the API.
- `aqmesh.url` the URL to use for the API. Normally this should be `https://api.aqmeshdata.net/api/`, but for testing
purposes `https://apitest.aqmeshdata.net/api/` can be used. Using the test api will not change the pointer of the actual
pod (see [Data retrieval](#data-retrieval)). This property also allows to adjust the agent, if the URL should change in the future.
- `aqmesh.podIndex` the index in the asset list (JSON array) of the pod the agent is responsible for (see 
[Retrieve location number](#retrieve-location-number)). This allows to have multiple versions of the agent, one for each
pod. Currently CARES only owns one pod, so the index should be 0. When using the test API the index should be 1, as the 
other test pod is currently not properly working.

A sample file can be found at `config/api.properties`.

#### Mapping files
What are the mapping files and why are they required? The mapping files define how data received from the API is connected
to the knowledge graph (KG). Specifically, each JSON key in the readings (see [Example readings](#example-readings)) 
represents a specific measure that needs to be represented by an IRI, if it should be saved in the database.

Furthermore, measures can be grouped into one time-series (will result in one time-series instance per group in the KG).
This should be done so that all measures in one group are recorded at the same time interval, and so they come from 
the same readings, e.g. gas readings or particle readings, without mixing them together. However, it is possible to 
break down the readings into smaller, logical groups, e.g. general readings and actual measures.

The mapping is achieved in this package by using one property file per group. Each property file contains one line per 
JSON key that should be linked to an IRI, e.g. like:
```
co_slope=http:/example/co_slope
```
If the IRI is left empty (`co_slope=` in the example), i.e. because there is no instance that represents the measure yet, 
it will be automatically created when the agent is run for the first time. This automatically generated URI will have the
following form:
```
[prefix]/[key]_[UUID]
```
where the `[prefix]` is hardcoded into the `AQMeshInputAgent` class in a public, static field called `generatedIRIPrefix`
which is based on the time-series client namespace, `[key]` is the JSON key the URI is generated for, and `[UUID]` is a 
randomly generated UUID.

Note, that not all JSON keys need to be represented in the mapping files (the data will simply be ignored and not stored), 
but there needs to be a 1-1 mapping, i.e. no IRI can be used for multiple JSON keys.

To ensure that the same IRIs are used for each JSON key, the mapping files are saved back after each run (only really 
necessary when some of them are automatically generated). Note, that if you change any mapping in preceding runs, they 
will be seen as new time-series, which can result in inconsistencies in both the KG and database.

Examples for the structure of the mapping folder and files can be found in the `mapping` folder within the `config` 
folder. Here, the keys are grouped into three groups, where the particle readings are split into measures and general 
information, and for the gas readings general information already contained in the particle readings are ignored (see 
also [Example readings](#example-readings)).

### Geolocation data configurations
In the `./stack-manager-input-config-service/aqmesh-input-agent.json`, there are three variables that affects where the geolocation information will be uploaded to in postGIS and the Geoserver:
- `LAYERNAME` the name of the layer in Geoserver, this will also be the name of the table in the postGIS database that will be used to store the geolocation information
- `DATABASE` the name of the database in postGIS to store the geolocation information
- `GEOSERVER_WORKSPACE` the name of the workspace in Geoserver to store the geolocation information

More information can be found in the `./stack-manager-input-config-service/aqmesh-input-agent.json`.

### Stack Deployment

Modify [api.properties](#api-properties) and [client.properties](#time-series-client-properties) in the `config` folder accordingly. The sparql endpoints and postGIS database indicated in the [client.properties](#time-series-client-properties) needs to be created manually beforehand. 

Open up the command prompt in the same directory as this README, run the command below to build the docker image:
```
docker compose build
```
Open `stack-manager-input-config-service/aqmesh-input-agent.json` and under the `Mounts` section, modify the `Source` and insert the filepath of where the `config` folder is located at (For Windows users using WSL on Docker, the file path should start with `/mnt/c/`, which is equivalent to `C://`). Under the `Env` section, modify the variables listed in [Geolocation data configurations](#geolocation-data-configurations) accordingly.

Copy `stack-manager-input-config-service/aqmesh-input-agent.json` to the services folder under your stack-manager directory (By default it should be `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`) and start up the stack.

### Run the agent

The agent has four routes, a status route, a retrieve route, a stop scheduler route and a instantiateGeoLocation route. A description for each route is provided below.

#### Status route

This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:3838/aqmesh-input-agent/status
```
and it should return:

{"Result":"Agent is ready to receive requests."}

#### Retrieve route

This request gets the latest timeseries for the AQMesh and uploads them to the knowledge graph. The timeseries data will be stored in the endpoints indicated in the [client.properties](#time-series-client-properties). The request will also initiate a scheduler to constantly run the retrieve route at a set interval. In the request, the user can set the following parameters for the scheduler:
- `delay` the time delay before the first execution of retrieve route
- `interval` the interval between successive executions 
- `timeunit` the time unit of delay and interval, this is currently limited to the following options: seconds, minutes, hours 

An example of the request in curl syntax is shown below:
```
curl -X POST --header "Content-Type: application/json" -d "{\"delay\":\"0\",\"interval\":\"180\",\"timeunit\":\"seconds\"}" http://localhost:3838/aqmesh-input-agent/retrieve
```

#### Stop Scheduler route

This request will stop the internal scheduler that was initialized in the Retrieve route.

An example of the request in curl syntax is shown below:
```
curl -X POST http://localhost:3838/aqmesh-input-agent/stopScheduler
```

If there is no internal scheduler running at the moment, the agent will return the following:
```
{"result":"Scheduler has not been initialised yet..."}
```

If the scheduler has been stopped successfully, the agent will return the following:
```
{"result":"Stopping Scheduler..."}
```

#### InstatiateGeoLocation route
This request instantiates the geolocation information of the AQMesh pod in postGIS and geoserver based on the locations indicated in the `stack-manager-input-config-service/aqmesh-input-agent.json` ([Geolocation data configurations](#geolocation-data-configurations)). The request has two possible formats:
```
curl -X POST http://localhost:3838/aqmesh-input-agent/instantiateGeoLocation
```
For this request, the agent will create an AQMesh IRI and name for the AQMesh pod indicated in [api.properties](#api-properties) and instantiate them in postGIS and geoserver. The agent will return back the generated AQMesh IRI and name in a JSON Object.

```
curl -X POST --header "Content-Type: application/json" -d "{\"iri\":\"https://www.theworldavatar.com/ontology/ontoaqmesh/AQMesh.owl/AQMesh_3f2c8a39-4bda-4df2-9102-309a8f878375\",\"name\":\"AQMesh 3f2c8a39-4bda-4df2-9102-309a8f878375\"}" http://localhost:3838/aqmesh-input-agent/instantiateGeoLocation
```
For this request, the agent will retrieve the AQMesh IRI and name for the AQMesh pod from the request and instantiate them in postGIS and geoserver. The agent will return back the AQMesh IRI and name in a JSON Object.

If the instance does not already exist in postGIS and geoserver, the following will be returned back:
```
{"iri":"https://www.theworldavatar.com/ontology/ontoaqmesh/AQMesh.owl/AQMesh_c98872ec-ab6a-467b-9faf-60fe0d5851ae","name":"AQMesh c98872ec-ab6a-467b-9faf-60fe0d5851ae","message":"Geospatial information instantiated for the AQMesh pod."}
```

If the instance already exist in postGIS and geoserver, the following will be returned back instead:
```
{"message":"An AQMesh instance already exist for the following: https://www.theworldavatar.com/ontology/ontoaqmesh/AQMesh.owl/AQMesh_3f2c8a39-4bda-4df2-9102-309a8f878375"}
```

#### Recommended execution sequence
To fully instantiate the data (geolocation, timeseries etc) into the knowledge graph, it is recommended to follow the sequence below when executing the routes:
1) Retrieve Route - instantiate the timeseries and start the scheduler to periodically retrieve and upload the latest timeseries to the knowledge graph
2) InstantiateGeoLocation Route - instantiate geolocation data for the AQMesh pods in postGIS and geoserver