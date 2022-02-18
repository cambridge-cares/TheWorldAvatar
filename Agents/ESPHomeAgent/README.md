# ESPHome Agent

This agent is designed to query for the relevant IRIs from the KG and subsequently use these IRIs to query for the latest timeseries data from the database. 
The latest timeseries data and threshold set by the user will determine the requests sent to the ESPHome web server. If the latest timeseries value is greater
than the threshold, the switch will be turned on and vice versa, this switch can be used to control other devices such as lightings or fans. Note that even 
though this agent is designed to interact with a switch, the ESPHome web server functionality is not limited to the switch alone, it can be used with other 
devices such as sensors, lights, fans etc.

The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
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
To check the state of the GPIO pin which behaves like a switch, a GET request needs to be sent to the following endpoint:
```
http://<IP address of ESP node>/switch/generic_output
```

If the switch is off, the response will be:
```
{"id":"switch-generic_output","state":"OFF","value":false}
```

If the switch is on, the response will be:
```
{"id":"switch-generic_output","state":"ON","value":true}
```

### Toggle switch
The switch can be toggled by sending a POST request to the following endpoint:

To turn on the switch:
```
http://<IP address of ESP node>/switch/generic_output/turn_on
```

To turn off the switch:
```
http://<IP address of ESP node>/switch/generic_output/turn_off
```
### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine 
or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README
to explain the set-up of a knowledge graph triple store or Postgres database.

### Property files
For running the agent, three property files are required:
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database, SPARQL endpoint and which data to retrieve.
- One [property file for the ThingsBoard API](#api-properties) defining the path Url for the web server and the threshold limit which decides whether the
switch is toggled on or off.

#### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of
the knowledge graph and the Postgres database. It should contain the following keys:
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `dataIRI` the data IRI of the parameter/variable that the agent queries from the knowledge graph and database

More information can be found in the example property file `client.properties` in the `config` folder.

#### API properties
The API properties contain the necessary information for sending HTTP requests. It should contain the following keys:
- `esphome.url` the URL for where the ESPHome server is located.
- `esphome.threshold` the value which decides whether to toggle the switch on or off.

More information can be found in the example property file `api.properties` in the `config` folder.


### Building the ESPHome Agent
The ESPHome Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties` and `client.properties` in the `config` folder accordingly. The Dockerfile will copy the 2 properties files and set environment 
variables pointing to their location thus there is no need to shift the properties files nor add in environment variables manually.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

The agent is reachable at "esphome-agent/toggle" on localhost port 1011.


#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1011/esphome-agent/toggle with a correct JSON Object.
Follow the request shown below.

```
POST http://localhost:1011/esphome-agent/toggle
Content-Type: application/json
{"apiProperties":"API_PROPERTIES","clientProperties":"CLIENT_PROPERTIES"}
```

If the agent run successfully, either one of the four responses below will be returned as a JSONObject.

switch is in OFF state. Latest timeseries value is lesser than the threshold.
```
{"message":["The switch is already in the OFF state.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

switch is in OFF state. Latest timeseries value is greater than the threshold.
```
{"message":["A POST request has been sent to turn on the device or component.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

switch is in ON state. Latest timeseries value is lesser than the threshold.
```
{"message":["A POST request has been sent to turn off the device or component.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```

switch is in ON state. Latest timeseries value is greater than the threshold.
```
{"message":["The switch is already in the ON state.","A request has been successfully sent to the ESPHome web server.","POST request has been sent successfully."]}
```