# ESPHome Update Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding status of components that are controllable via ESPHome.
It's only purpose is to retrieve new data (if available) from the API and download it into 
the corresponding database, as well as, instantiating KG instances and connection when called for the first time. The 
agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS base lib to interact with both the KG and database.

Before explaining the usage of the agent, we will briefly summarize the ESPHome API that is contacted by one of the classes in this package to retrieve data.

## ESPHome API

The API will be briefly described here. 
Official documentations regarding the API can be found [here] (https://esphome.io/web-api/index.html).

### Prerequisite
To use the API, a ESPHome server must be already set up and running. One way to set it up is to have a ESP board such as the ESP8266 and configure it. 
More information can be found [here] (https://esphome.io/components/web_server.html).
A step by step example can be found [here] (https://siytek.com/esp8266-web-server-led/).

Another prerequisite is to have a KG that has the relevant timeseries IRIs instantiated in it and a postgreSQL database with timeseries data.

### switch status
To check the status of a component, a GET request needs to be sent to the following endpoint:
```
http://<IP address of ESP node>/<domain>/<domain ID>
```

If the component such as a switch is in the off state, the response will be:
```
{"id":"switch-generic_output","state":"OFF","value":false}
```

If the component such as a switch is in the on state, the response will be:
```
{"id":"switch-generic_output","state":"ON","value":true}
```

## Usage 
This part of the README describes the usage of the input agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. 
Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client which maintains both instances in a 
knowledge graph and a Postgres database to store the data, these will be required to be set-up before.  

The [next section](#requirements) will explain the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine 
or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README
to explain the set-up of a knowledge graph triple store or Postgres database.

### Property files
For running the agent, three property files are required:
- One [property file for the agent](#agent-properties) itself pointing to the mapping configuration.
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for the ESPHome API](#api-properties) defining the IP address of the ESPHome web server, domain, domain ID.

#### Agent properties
The agent property file only needs to contain a single line:
```
esphome.mappingfolder=ESPHOME_UPDATE_AGENT_MAPPINGS
```
where `ESPHOME_UPDATE_AGENT_MAPPINGS` is the environment variable pointing to the location of a folder containing JSON key to IRI mappings. 
An example property file can be found in the `config` folder under `agent.properties`. See [this section](#mapping-files) of the README for an 
explanation of the mapping files.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of
the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

#### API properties
The API properties contain the parameters needed to get the status of the components via ESPHome API (see the [API description](#esphome-api)),
the port number and keys. It should contain the following keys:
- `esphome.domain` the domain which indicate the type of component (switch, sensor, light etc).
- `domain.ID` the ID or name of the component.
- `path.url` the URL or IP address for where the ESPHome server is located. 
More information can be found in the example property file `api.properties` in the `config` folder.

#### Mapping files
What are the mapping files and why are they required? The mapping files define how data received from the API is connected
to the knowledge graph (KG). Specifically, each JSON key in the readings represents a specific measure that needs to be 
represented by an IRI, if it should be saved in the database.

Furthermore, measures can be grouped into one time-series (will result in one time-series instance per group in the KG).
This should be done so that all measures in one group are recorded at the same time interval, and so they come from 
the same readings. However, it is possible to break down the readings into smaller, logical groups.

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
where the `[prefix]` is hardcoded into the `ESPHomeUpdateAgent` class in a public, static field called `generatedIRIPrefix`
which is based on the time-series client namespace, `[key]` is the JSON key the URI is generated for, and `[UUID]` is a 
randomly generated UUID.

Note, that not all JSON keys need to be represented in the mapping files (the data will simply be ignored and not stored), 
but there needs to be a 1-1 mapping, i.e. no IRI can be used for multiple JSON keys.

To ensure that the same IRIs are used for each JSON key, the mapping files are saved back after each run (only really 
necessary when some of them are automatically generated). Note, that if you change any mapping in preceding runs, they 
will be seen as new time-series, which can result in inconsistencies in both the KG and database.

Examples for the structure of the mapping folder and file can be found in the `mapping` folder within the `config` 
folder. 

### Building the ESPHome Update Agent
The ESPHome Update Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties` and `client.properties` in the `config` folder accordingly. You should not modify the `agent.properties` file as the Dockerfile will set the environment variable 
ESPHOME_UPDATE_AGENT_MAPPINGS automatically to point towards the location of the mapping folder. The Dockerfile will copy all 3 properties files and mapping folder and set environment variables pointing 
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually.

#### Agent deployment out of the stack

To build and start the agent out of the stack, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

#### Stack deployment

If you want to spin up this agent as part of a stack, instead of `docker-compose up -d`, do the following:
- Copy the contents of `config/client.properties_stack` into `config/client.properties`, inserting the name of your stack.
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/services/`.
- Go to the stack manager folder by following this route: `TheWorldAvatar/Deploy/dynamic/stack-manager/`, check whether there is a `<STACK NAME>.json` under the sub folder `/inputs/config/` and create one if it doesn't exist. If it exists already, append the agent to the json file. (Read [Stack configuration](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#stack-configuration) for more information.)
- Start the stack manager as usual. This should start the container.


#### Run the agent
If the agent is outside of the stack, a POST request must be sent to http://localhost:1012/esphome-update-agent/retrieve with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1012/esphome-update-agent/retrieve
Content-Type: application/json
{"agentProperties":"ESPHOME_UPDATE_AGENTPROPERTIES","apiProperties":"ESPHOME_UPDATE_APIPROPERTIES","clientProperties":"ESPHOME_UPDATE_CLIENTPROPERTIES"}
```
In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{\"agentProperties\":\"ESPHOME_UPDATE_AGENTPROPERTIES\",\"apiProperties\":\"ESPHOME_UPDATE_APIPROPERTIES\",\"clientProperties\":\"ESPHOME_UPDATE_CLIENTPROPERTIES\"}" http://localhost:1012/esphome-update-agent/retrieve
```
If the agent is in the stack, a POST request must be sent to http://localhost:3838/esphome-update-agent/retrieve with a correct JSON Object.
Follow the request shown below.
```
POST http://localhost:3838/esphome-update-agent/retrieve
Content-Type: application/json
{"agentProperties":"ESPHOME_UPDATE_AGENTPROPERTIES","apiProperties":"ESPHOME_UPDATE_APIPROPERTIES","clientProperties":"ESPHOME_UPDATE_CLIENTPROPERTIES"}
```
In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{\"agentProperties\":\"ESPHOME_UPDATE_AGENTPROPERTIES\",\"apiProperties\":\"ESPHOME_UPDATE_APIPROPERTIES\",\"clientProperties\":\"ESPHOME_UPDATE_CLIENTPROPERTIES\"}" http://localhost:3838/esphome-update-agent/retrieve
```

If the agent run successfully, you should see a JSON Object returned back that is similar to the one shown below.
```
{"Result":["Input agent object initialized.","Time series client object initialized.","API connector object initialized.","Retrieved status of component.","Data updated with new readings from API.","Timeseries Data has been updated."]}
```

If the JSON Object returned back is as shown below, it means that the request was written wrongly. Check whether the URL, keys and values are written correctly.
```
{"Result":"Request parameters are not defined correctly."}
```