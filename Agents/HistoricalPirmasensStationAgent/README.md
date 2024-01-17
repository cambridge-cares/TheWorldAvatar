# Historical Pirmasens Station Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the Pirmasens weather station. It's only purpose is to retrieve 
new/historical data from csv files and download it into the corresponding database, as well as, instantiating KG instances and connection when called for the first time. 
The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS_BASE_LIB to interact with both the KG and database.

### Data retrieval and pre-processing
This agent is designed to work with csv files. Some pre-processing might be required in order to ensure that the csv files are compatible with the agent.
Do note that the keys in the csv file must correspond with the keys found in the weather.properties file. An example properties file can be found in the `mapping` folder within the `config` folder.

#### Example readings
Readings are returned to the response body in form of a JSON Object which consist of key-value pair. The JSONObject has the 
key:"sensors", which contains a JSONArray containing a JSONObject. Inside this JSONObject is a JSONArray associated with the key:"data". This JSONArray contains several JSONObject with one JSONObject per timestamp. The key value pairs within this JSONObject contain all the relevant weather readings corresponding to a particular timestamp. The key for the timestamp is `[ts]`.
The following is an example of the JSONObject response: 

{"sensors":[{"data":[{"Niederschlag":0,"Luftdruck":1021.47998047,"Ozon":27.58182144,
"Temperatur":12.15265083,"Windrichtung_rohwert":158.85850525,"eBC_PM2-5":0.25384998,
"Gesamt_UV_Strahlung":0.21651599,"PM10":13.19515038,"Stickstoffdioxid":2.81819487,
"Relative_Feuchte":75.65049744,"Stickstoffmonoxid":0.80000001,"Windgeschwindigkeit_rohwert":0.469026,
"ts":"2022-07-07T00:00:00"},{"Niederschlag":0,"Luftdruck":1021,"Ozon":29.14218903,
"Temperatur":12.49265003,"Windrichtung_rohwert":151.21800232,"eBC_PM2-5":0.25505501,
"Gesamt_UV_Strahlung":0.0943425,"PM10":10.44490051,"Stickstoffdioxid":2.52042007,
"Relative_Feuchte":74.30400085,"Stickstoffmonoxid":0.80000001,"Windgeschwindigkeit_rohwert":0.48083451,"ts":"2022-07-07T01:00:00"}]}]}


## Usage 
This part of the README describes the usage of the agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client which maintains both instances in a knowledge graph and a Postgres database to store the data, these will be required to be set-up before.  

The [next section](#requirements) will explain the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

### Property and csv files
For running the agent, three property files and one csv file are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for the csv file connector](#csv-connector-properties) defining the number of keys for the csv file.
- One csv file with the name "data.csv" placed under the `config` folder. An example csv file "testData.csv" can be found in the `testData` folder at HistoricalPirmasensStationAgent\testData\testData.csv.

#### Agent properties
The agent property file only needs to contain a single line:
```
pirmasensStation.mappingfolder=PIRMASENS_AGENT_MAPPINGS
```
where `PIRMASENS_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. An example property file can be found in the `config` folder under 
`agent.properties`. See [this section](#mapping-files) of the README for an explanation of the mapping files.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

#### csv connector properties
The csv connector properties contain the number of keys for the csv file. It should contain the following keys:
- `numOfKeys` the number of columns/keys in the data csv file.

More information can be found in the example property file `csvconnector.properties` in the `config` folder.

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
where the `[prefix]` is hardcoded into the `HistoricalPirmasensStationAgent` class in a public, static field called `generatedIRIPrefix`
which is based on the time-series client namespace, `[key]` is the JSON key the URI is generated for, and `[UUID]` is a 
randomly generated UUID.

Note, that not all JSON keys need to be represented in the mapping files (the data will simply be ignored and not stored), 
but there needs to be a 1-1 mapping, i.e. no IRI can be used for multiple JSON keys.

To ensure that the same IRIs are used for each JSON key, the mapping files are saved back after each run (only really 
necessary when some of them are automatically generated). Note, that if you change any mapping in preceding runs, they 
will be seen as new time-series, which can result in inconsistencies in both the KG and database. To retrieve the mapping 
files from a running docker container, you have to type in the following into the command line:

```
docker cp <Docker container ID>://root/mappings/<name of properties file> <destination filepath on your machine>
```

For example, to retrieve the weather.properties mapping file from the docker container, the following line have to be entered into the command line:

```
docker cp 7956ce42351d://root/mappings/weather.properties C:\Users\USER01\Desktop\weather.properties
```

Examples for the structure of the mapping folder and files can be found in the `mapping` folder within the `config` 
folder.  (see 
also [Example readings](#example-readings)).

If there exist a pre-defined mapping file, it can be used to replace the weather.properties file found in the `mapping` folder within the `config` folder.

### Building the HistoricalPirmasensStation Agent

The HistoricalPirmasensStation Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `csvconnector.properties` and `client.properties` in the `config` folder accordingly. You should not modify the `agent.properties` file as the Dockerfile will set the environment variable 
PIRMASENS_AGENT_MAPPINGS to point towards the location of the mapping folder. The Dockerfile will copy all 3 properties files and mapping folder and set environment variables pointing 
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually.

One csv file with the name "data.csv" needs to be placed under the `config` folder. An example csv file "testData.csv" can be found in the `testData` folder at HistoricalPirmasensStationAgent\testData\testData.csv.

#### Docker deployment

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

#### Stack deployment

If you want to spin up this agent as part of a stack, instead of `docker-compose up -d`, do the following:
- Copy the contents of `config/client.properties_stack` into `config/client.properties`, inserting the name of your stack.
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into the `inputs/config` folder of the stack manager.
- Start the stack manager as usual. This should start the container.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1027/historical-pirmasens-station-agent/retrieve with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1027/historical-pirmasens-station-agent/retrieve
Content-Type: application/json
{"agentProperties":"HISTORICAL_AGENTPROPERTIES","connectorProperties":"HISTORICAL_CONNECTORPROPERTIES","clientProperties":"HISTORICAL_CLIENTPROPERTIES"}
```
In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"agentProperties\":\"HISTORICAL_AGENTPROPERTIES\",\"connectorProperties\":\"HISTORICAL_CONNECTORPROPERTIES\",\"clientProperties\":\"HISTORICAL_CLIENTPROPERTIES\"}" localhost:1027/historical-pirmasens-station-agent/retrieve
```

If the agent is part of a stack, you will most likely need to use a different port number in the above requests, e.g. 3838.

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":["Input agent object initialized.","Time series client object initialized.","CSV connector object initialized.","Retrieved 48 weather station reading.","Data updated with new readings from the CSV file.","Timeseries Data has been updated."]}
```
