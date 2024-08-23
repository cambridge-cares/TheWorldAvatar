# NUS Davis Weather Station input agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the NUS Davis weather station. Its only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS_BASE_LIB to interact with both the KG and database.

Before explaining the usage of the agent, we will briefly summarize the weather station API that is
contacted by one of the classes in this package to retrieve data.

## Weather Station API

We will here briefly describe the weather station API. The official documentation can be found [here](https://weatherlink.github.io/v2-api/) .


### Data retrieval
The daily weather readings are returned every 5 minutes. A new reading is made by the sensor in an interval of 5 minutes.

#### The endpoint
The actual endpoint has the following structure:
```
https://api.weatherlink.com/v2/current/[<stationId>]?apiKey=[<apiKey>]&t=[<timestamp>]&apiSignature=[<apiSignature>]
```
where `[stationId]` is the id of the weather station which is taking the physical readings.  The  
`[apiKey]` is the key needed to access the API. `[timestamp]` refers to the timestamp at which the data is to be retrieved. 
Finally, the option `[apiSignature]` contains the apiSignature. This signature is generated firstly by creating a string 
made by concatenating the parameter name-value pairs in the following manner:
api-key`[apiKey]`t`[timestamp]` (e.g. api-key982634341t1988729481). Then the aforementioned concatenated string can be used
to generate the apiSignature by using the HMAC SHA-256 algorithm with the HMAC secret key being the `[apiSecret]`. Note that every api call
requires a fresh timestamp. (See also the [API documentation](#Weather-Station-API)).
The endpoint controls what type of data is retrieved its form.
#### Example readings
Readings are returned to the response body in form of a JSON Object which consist of key-value pair. The JSONObject has the 
key:"sensors", which contains a JSONArray containing a JSONObject. Inside this JSONObject is a JSONArray associated with the key:"data". This JSONArray contains one JSONObject. The key value pairs within this JSONObject contain all the relevant weather readings corresponding to a particular timestamp. The key for the timestamp is `[ts]`.
The following images are examples of what the JSONObject returned by a `[current]` data api call looks like. 

![Shows part of the response body of a successful weather readings request.](docs/img/sample_reading1.png "First part of the weather data reading obtained using a 'current' data api call")
![Shows part of the response body of a successful weather readings request.](docs/img/sample_reading2.png "Second part of the current weather data reading obtained using a 'current' data api call")
![Shows part of the response body of a successful weather readings request.](docs/img/sample_reading3.png "Third part of the current weather data reading obtained using a 'current' data api call")


## Usage 
This part of the README describes the usage of the input agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client which maintains both instances in a knowledge graph and a Postgres database to store the data, these will be required to be set-up before.  

The [next section](#requirements) will explain the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

### Property files
For running the agent, three property files are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for the weather station API](#api-properties) defining the api_key, stationId and the api_url.

#### Agent properties
The agent property file only needs to contain a single line:
```
nusDavisWeatherStation.mappingfolder=NUSDavisWeatherStation_AGENT_MAPPINGS
```
where `NUSDavisWeatherStation_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. An example property file can be found in the. An example property file can be found in the `config` folder under 
`agent.properties`. See [this section](#mapping-files) of the README for an explanation of the mapping files.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

#### API properties
The API properties contain the credentials to authorize access to the weather Station API (see the [API description](#Weather-Station-API)),
as well as, the API URL and the weather station ID. It should contain the following keys:
- `weather.api_key` the key needed to access the API.
- `weather.api_secret` the secret needed to generate the API signature.
- `weather.stationId` the stationId associated with the sensor.
- `weather.api_url` the URL to use for the API. (see [Data retrieval](#data-retrieval)). This property also allows to adjust the agent, if the URL should change in the future.


More information can be found in the example property file `api.properties` in the `config` folder.

#### Mapping files
What are the mapping files and why are they required? The mapping files define how data received from the API is connected
to the knowledge graph (KG). Specifically, each JSON key in the readings (see [Example readings](#example-readings)) 
represents a specific measure that needs to be represented by an IRI, if it should be saved in the database.

Furthermore, measures can be grouped into one time-series (will result in one time-series instance per group in the KG).
This should be done so that all measures in one group are recorded at the same time interval, and so they come from 
the same readings

The mapping is achieved in this package by using one property file per group. Each property file contains one line per 
JSON key that should be linked to an IRI, e.g. like:
```
temp_in=http:/example/temp_in
```
If the IRI is left empty (`temp_in=` in the example), i.e. because there is no instance that represents the measure yet, 
it will be automatically created when the agent is run for the first time. This automatically generated URI will have the
following form:
```
[prefix]/[key]_[UUID]
```
where the `[prefix]` is hardcoded into the `NUSDavisWeatherStationInputAgent` class in a public, static field called `generatedIRIPrefix`
which is based on the time-series client namespace, `[key]` is the JSON key the URI is generated for, and `[UUID]` is a 
randomly generated UUID.

Note, that not all JSON keys need to be represented in the mapping files (the data will simply be ignored and not stored), 
but there needs to be a 1-1 mapping, i.e. no IRI can be used for multiple JSON keys.

To ensure that the same IRIs are used for each JSON key, the mapping files are saved back after each run (only really 
necessary when some of them are automatically generated). Note, that if you change any mapping in preceding runs, they 
will be seen as new time-series, which can result in inconsistencies in both the KG and database.

Examples for the structure of the mapping folder and files can be found in the `mapping` folder within the `config` 
folder.  (see 
also [Example readings](#example-readings)).

### Building the NUSDavisWeatherStation Agent

The NUSDavisWeatherStation Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties` and `client.properties` in the `config` folder accordingly. You should not modify the `agent.properties` file as the Dockerfile will set the environment variable 
NUSDavisWeatherStation_AGENT_MAPPINGS to point towards the location of the mapping folder. The Dockerfile will copy all 3 properties files and mapping folder and set environment variables pointing 
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```
Note that for building the agent in the Claudius server the following command should be used
```
DOCKER_BUILDKIT=1 docker build .
```
The agent is reachable at "nusdavisweatherstation-agent/retrieve" on localhost port 1082.


#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1082/nusdavisweatherstation-agent/retrieve with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1082/nusdavisweatherstation-agent/retrieve
Content-Type: application/json
{"agentProperties":"NUSDavisWeatherStation_AGENTPROPERTIES","apiProperties":"NUSDavisWeatherStation_APIPROPERTIES","clientProperties":"NUSDavisWeatherStation_CLIENTPROPERTIES"}
```
In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"agentProperties\":\"NUSDavisWeatherStation_AGENTPROPERTIES\",\"apiProperties\":\"NUSDavisWeatherStation_APIPROPERTIES\",\"clientProperties\":\"NUSDavisWeatherStation_CLIENTPROPERTIES\"}" http://localhost:1082/nusdavisweatherstation-agent/retrieve
```

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":["Input agent object initialized.","Time series client object initialized.","API connector object initialized.","Retrieved 1 weather station reading.","Data updated with new readings from API.","Timeseries Data has been updated."]}
```

If the returned JSON Object is as shown below, it means that the request was written wrongly. Check whether the URL, keys and values are written correctly.
```
{"Result":"Request parameters are not defined correctly."}
```