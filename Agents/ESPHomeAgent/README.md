# ESPHome Agent

This agent is designed to query for the relevant IRIs and information from the knowledge graph and subsequently use these IRIs to query for the latest timeseries data from the database. 
The agent will toggle the status of a component that is being controlled via the ESPHome web server based on the latest timeseries data value, latest status of the component and the setpoint value that it queries from the knowledge graph.(e.g. controlling the on off state of a cooling fan based on the latest measured temperature and temperature setpoint queried from the knowledge graph.)


The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) and [Access Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT) to interact with both the KG and database.

Before explaining the usage of the agent, we will briefly summarize the ESPHome API that is contacted by one of the classes in this package to retrieve data.

## ESPHome API

The API will be briefly described here. 
Official documentations regarding the API can be found [here] (https://esphome.io/web-api/index.html).

### Prerequisite
To use the API, a ESPHome server must be already set up and running. One way to set it up is to have a ESP board such as the ESP8266 and configure it. 
More information can be found [here] (https://esphome.io/components/web_server.html).
A step by step example can be found [here] (https://siytek.com/esp8266-web-server-led/).

### switch status
To check the state of the GPIO pin which behaves like a switch, a GET request needs to be sent to the following endpoint:
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

### Toggle component
The switch can be toggled by sending a POST request to the following endpoint:

To turn on the switch:
```
http://<IP address of ESP node>/<domain>/<domain ID>/turn_on
```

To turn off the switch:
```
http://<IP address of ESP node>/<domain>/<domain ID>/turn_off
```

### Requirements
1) It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine 
or need to be accessible from the host machine via a fixed URL. This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

2) An Access Agent instance should be set up and working properly. The necessary routing information should be stored in a triple store such as Blazegraph. Check [Uploading-routing-information](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT#Uploading-routing-information) for more information.

3) It is necessary to have the component and the setpoint that determines when to toggle the component instantiated in the knowledge graph based on [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice). It is necessary to have the timeseries of the status of the component instantiated in the knowledge graph, one such agent that does this is [ESPHome Update Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeUpdateAgent). An example of such an instantiation is shown below:
```
<https://www.theworldavatar.com/kg/ontodevice/CoolingFan-01> rdf:type <https://saref.etsi.org/saref4bldg/Fan> ;
        <https://saref.etsi.org/core/hasState>	<http://www.theworldavatar.com/kb/ontotimeseries/esphome_generic_output_2422cab1-fa7a-4093-a86b-0c3d13732d49> ;
        <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontodevice/Setpoint-01> .

<http://www.theworldavatar.com/kb/ontotimeseries/esphome_generic_output_2422cab1-fa7a-4093-a86b-0c3d13732d49> rdf:type <https://saref.etsi.org/core/OnOffState> ;
        <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeSeries> <http://www.theworldavatar.com/kb/ontotimeseries/Timeseries_003ec3be-a767-4943-835d-8587efb93504> .

<http://www.theworldavatar.com/kb/ontotimeseries/Timeseries_003ec3be-a767-4943-835d-8587efb93504> rdf:type <https://www.theworldavatar.com/kg/ontotimeseries/InstantaneousTimeSeries> ;
        <https://www.theworldavatar.com/kg/ontotimeseries/hasRDB> "jdbc:postgresql://localhost:5432/esphome" ;	
        <https://www.theworldavatar.com/kg/ontotimeseries/hasTimeUnit> "OffsetDateTime" .

<https://www.theworldavatar.com/kg/ontodevice/Setpoint-01> rdf:type <https://www.theworldavatar.com/kg/ontodevice/Setpoint> ;
        <https://www.theworldavatar.com/kg/ontodevice/hasQuantity> <https://www.theworldavatar.com/kg/ontodevice/Setpoint-01-Temperature> .

<https://www.theworldavatar.com/kg/ontodevice/Setpoint-01-Temperature> rdf:type <http://www.ontology-of-units-of-measure.org/resource/om-2/Temperature>	;
        <http://www.ontology-of-units-of-measure.org/resource/om-2/hasValue> <https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature> .

<https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature> rdf:type <http://www.ontology-of-units-of-measure.org/resource/om-2/Measure> ;
        <http://www.ontology-of-units-of-measure.org/resource/om-2/hasUnit> <http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius> ;
        <http://www.ontology-of-units-of-measure.org/resource/om-2/hasNumericalValue> "26.0"^^xsd:double .
```

### Property files
For running the agent, three property files are required:
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database, SPARQL endpoint and which data to retrieve.
- One [property file for the ESPHome time-series client](#ESPHome-time-series-client-properties) defining how to access a different database and SPARQL endpoint for the ESPHome controlled component status.
- One [property file for the ESPHome API](#api-properties) defining the path Url for the web server, domain, domain ID of the component.


#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of
the knowledge graph and the Postgres database that contains the timeseries data of the measured variable. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `dataIRI` the data IRI of the parameter/variable that the agent queries from the knowledge graph and database

More information can be found in the example property file `client.properties` in the `config` folder.

#### ESPHome time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of
the knowledge graph and the Postgres database that contains the status of the ESPHome controlled component. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `dataIRI` the data IRI of the status of the ESPHome controlled component

More information can be found in the example property file `esphome-client.properties` in the `config` folder.

#### API properties
The API properties contain the parameters needed to toggle the status of the components via ESPHome API (see the [API description](#esphome-api)). 
It should contain the following keys:
- `path.url` the URL or IP address for where the ESPHome server is located.
- `esphome.domain` the domain which indicate the type of component (switch, sensor, light etc).
- `domain.ID` the ID or name of the component. 

More information can be found in the example property file `api.properties` in the `config` folder.

### Building the ESPHome Agent
The ESPHome Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties`, `client.properties` and `esphome-client.properties` in the `config` folder accordingly. The `Dockerfile` will copy the 3 properties files and set environment variables pointing to their location thus there is no need to shift the properties files nor add in environment variables manually. 

In the `Dockerfile`, modify the value of "ACCESS_AGENT_TARGETRESOURCEID" accordingly. The value should correspond to where the Access Agent instance is located at and the label of your routing information:
```
http://<IP address>:<port number>/<label>
```
More information can be found in the `Dockerfile` in the same directory as this README section.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

#### Stack deployment

If you want to spin up this agent as part of a stack, instead of `docker-compose up -d`, do the following:
- Copy the contents of `config/client.properties_stack` into `config/client.properties` and copy the contents of `config/esphome-client.properties_stack` into `config/esphome-client.properties`, inserting the name of your stack.
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into `TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/services/`.
- Create TheWorldAvatar/Deploy/dynamic/stack-manager/inputs/config/<STACK NAME>.json manually if it doesn't exist. If it exists already, append the agent to the json file (Read [Stack configuration](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#stack-configuration)).
- Start the stack manager as usual. This should start the container.


#### Run the agent
If the agent is outside of the stack, a POST request must be sent to http://localhost:1011/esphome-agent/toggle with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1011/esphome-agent/toggle
Content-Type: application/json
{"timeseriesDataClientProperties":"CLIENT_PROPERTIES","esphomeStatusClientProperties":"ESPHOME_CLIENT_PROPERTIES","esphomeAPIProperties":"API_PROPERTIES"}
```
In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{\"timeseriesDataClientProperties\":\"CLIENT_PROPERTIES\",\"esphomeStatusClientProperties\":\"ESPHOME_CLIENT_PROPERTIES\",\"esphomeAPIProperties\":\"API_PROPERTIES\"}" http://localhost:1011/esphome-agent/toggle
```

If the agent is in the stack, a POST request must be sent to http://localhost:3838/esphome-agent/toggle with a correct JSON Object.
Follow the request shown below.
```
POST http://localhost:3838/esphome-agent/toggle
Content-Type: application/json
{"timeseriesDataClientProperties":"CLIENT_PROPERTIES","esphomeStatusClientProperties":"ESPHOME_CLIENT_PROPERTIES","esphomeAPIProperties":"API_PROPERTIES"}
```
In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{\"timeseriesDataClientProperties\":\"CLIENT_PROPERTIES\",\"esphomeStatusClientProperties\":\"ESPHOME_CLIENT_PROPERTIES\",\"esphomeAPIProperties\":\"API_PROPERTIES\"}" http://localhost:3838/esphome-agent/toggle
```


If the agent run successfully, either one of the four responses below will be returned as a JSONObject.

component is in OFF state. Latest timeseries value is lesser than the threshold.
```
{"message":["The component is already in the OFF state.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

component is in OFF state. Latest timeseries value is greater than the threshold.
```
{"message":["A POST request has been sent to turn on the device or component.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

component is in ON state. Latest timeseries value is lesser than the threshold.
```
{"message":["A POST request has been sent to turn off the device or component.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

component is in ON state. Latest timeseries value is greater than the threshold.
```
{"message":["The component is already in the ON state.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```