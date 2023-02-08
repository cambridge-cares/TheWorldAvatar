# Thingspeak Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the Thingspeak cloud server. Its only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS_BASE_LIB to interact with both the KG and database.

Before explaining the usage of the agent, we will briefly summarize the Thingspeak API that is
contacted by one of the classes in this package to retrieve data.

## Thingspeak API

We will here briefly describe the Thingspeak API. The official documentation can be found [here](https://www.mathworks.com/help/thingspeak/) .

### Data retrieval

This agent will mainly read data from all fields in a single channel and return the readings in a JSONObject.

#### The endpoint
The actual endpoint has the following structure:
```
https://api.thingspeak.com/channels/<channel_id>/feeds.json?results=<number of results>

or

https://api.thingspeak.com/channels/<channel_id>/feeds.json?api_key=<API_Key>&results=<number of results>
```
where `<channel_id>` is the id of the channel on the Thingspeak server that is storing the readings.  The  
`<API_Key>` is the key needed to access the API. This key is only required if the channel's sharing setting is set to private.
`<number of results>` refers to the number of readings to be retrieved. 

#### Example readings
Readings are returned to the response body in form of a JSON Object which consist of key-value pair. The JSONObject has the 
key:"channel", which contains information about the channel and another key:"feeds" which contains the readings and timestamps values.
Below is an example of how the JSON Object looks like:
```
{"channel":{"id":1888811,"name":"VEML7700 Illuminance Sensor","description":"Measures illuminance in Lux, Lux is converted to irradiance (W/m^2)",
"latitude":"0.0","longitude":"0.0","field1":"Illuminance","field2":"Irradiance","created_at":"2022-10-11T09:47:51Z","updated_at":"2022-11-09T02:36:56Z",
"last_entry_id":161},"feeds":[{"created_at":"2022-10-13T09:18:38Z","entry_id":159,"field1":"411.94","field2":"32.54"},{"created_at":"2022-10-13T09:18:58Z",
"entry_id":160,"field1":"412.15","field2":"32.56"},{"created_at":"2022-10-13T09:19:18Z","entry_id":161,"field1":"411.20","field2":"32.48"}]}
```
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
- One [property file for the Thingspeak API](#api-properties) defining the channel ID, API Key, Number of results and path URL.

#### Agent properties
The agent property file only needs to contain a single line:
```
thingspeak.mappingfolder=THINGSPEAK_AGENT_MAPPINGS
```
where `THINGSPEAK_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. This environment variable is set automatically when the docker container is built and thus there is no need to set the environment variable manually. An example property file can be found in the `config` folder under 
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
The API properties contains the parameters needed to access the Thingspeak API (see the [API description](#Thingspeak-API)). It should contain the following keys:
- `thingspeak.channelNumber` the ID of the channel.
- `thingspeak.apiKey` the API Key needed to read data from a private channel, a public channel do not require an API Key and this key can be set to equal to "None".
- `thingspeak.results` the number of results to retrieve from the Thingspeak server.
- `path.url` the URL to use for the API. (see [Data retrieval](#data-retrieval)). This property also allows to adjust the agent, if the URL should change in the future.


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
where the `[prefix]` is hardcoded into the `ThingspeakInputAgent` class in a public, static field called `generatedIRIPrefix`
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

### Building the Thingspeak Agent
The Thingspeak Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties` and `client.properties` in the `config` folder accordingly. You should not modify the `agent.properties` file as the Dockerfile will set the environment variable 
THINGSPEAK_AGENT_MAPPINGS to point towards the location of the mapping folder. The Dockerfile will copy all 3 properties files and mapping folder and set environment variables pointing 
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually.

Modify 'allFields.properties' in the `mapping` folder within the `config` folder accordingly based on what variables or parameters you are retrieving from the Thingspeak server.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```
The agent is reachable at "thingspeak-agent/retrieve" on localhost port 1015.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1015/thingspeak-agent/retrieve with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1015/thingspeak-agent/retrieve
Content-Type: application/json
{"agentProperties":"THINGSPEAK_AGENTPROPERTIES","apiProperties":"THINGSPEAK_APIPROPERTIES","clientProperties":"THINGSPEAK_CLIENTPROPERTIES"}
```

In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"agentProperties\":\"THINGSPEAK_AGENTPROPERTIES\",\"apiProperties\":\"THINGSPEAK_APIPROPERTIES\",\"clientProperties\":\"THINGSPEAK_CLIENTPROPERTIES\"}" http://localhost:1015/thingspeak-agent/retrieve
```
If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":["Input agent object initialized.","Time series client object initialized.","API connector object initialized.","Retrieved 2 readings.","Data updated with new readings from API.","Timeseries Data has been updated."]}
```

If the returned JSON Object is as shown below, it means that the request was written wrongly.
```
{"Result":"Request parameters are not defined correctly."}
```

### Redeploying the Thingspeak Agent
If the agent needs to be redeployed and you wish to retain the current timeseries IRIs instantiated in the Knowledge Graph, you will need to extract the mapping file from the docker container by typing the following into the command line:
```
docker cp <Docker container ID>://root/mappings/<name of properties file> <destination filepath on your machine>
```

For example, to retrieve the allFields.properties mapping file from the docker container, the following line have to be entered into the command line:
```
docker cp 7956ce42351d://root/mappings/allFields.properties C:\Users\USER01\Desktop\allFields.properties
```
Copy the allFields.properties file that you have just retrieved and use it to replace the allFields.properties file found in the `mapping` folder within the `config` 
folder. This will allow the agent to refer back to the timeseries IRIs in the allFields.properties file when the agent is redeployed.
