# BMSUpdateAgent
BMSUpdateAgent is an agent designed for multiple functions:
1) It is able to change the setpoint of devices and interact with relevant agents to trigger reactions and processes via the [Set Route](#31-set-route) . This first function is currently limited to only interact with the [ESPHomeAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeAgent) and [ESPHomeUpdateAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeUpdateAgent).

2) It is able to write values to a BMS Bacnet Endpoint via the [Wacnet API](https://hvac.io/docs/wacnet#orgheadline7). Wacnet will need to be set up beforehand and exposed such that it is accessible to the agent. The agent is able to either directly interact via the API if all parameters are provided or it will attempt to query for the missing parameters from the knowledge graph. More information is available at the [Write Route](#32-write-route).

3) It is able to retrieve the latest timeseries value of a data IRI, compare it with an user provided value, if they are equivalent, the agent is able to update the knowledge graph by inserting or/and deleting a set of triples provided to it. More information is available at the [Update Triples Route](#33-update-triples-route).

4) It is able to retrieve the present value for a Bacnet object and update the knowledge graph accordingly. More information is available at the [Update Present Value Route](#34-update-present-value-route).

# 1. Setup
This agent is designed to run in stack, which is spun up by [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

## 1.1. Build Image
The BMSUpdateAgent is set up to use the Maven repository. You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Then build image with:
```
docker build . -t bms-update-agent:1.1.0
```

## 1.2. Edit and Add Agent Config to Stack Manager
Open `stack-manager-input-config-service/bms-update-agent.json` and under the Mounts section, modify the Source and insert the filepath of where the config folder is located at (For Windows users using WSL on Docker, the file path should start with /mnt/c/, which is equivalent to C://).

Copy `stack-manager-input-config-service/bms-update-agent.json` to `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/`.

Create `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/<STACK NAME>.json` manually if it doesn't exist. If it exists already, append the agent to the file as follows:
```json
{
  "services": {
    "includes": [
      "bms-update-agent",
      ...
  ],
    "excludes": [
      ...
  ]
  }
}
```
After this step, the `stack-manager/inputs/config` folder will have the following structure:
```
config/
|_ services/
   |_ bms-update-agent.json
   |_ ...
|_ <STACK NAME>.json
```
More information about adding custom containers to the stack can be found [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#adding-custom-containers).

## 1.3. Spin Up Stack
Follow these [steps](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager#spinning-up-a-stack) to spin up the stack.

## 2. Client Properties File
Each of the route requires a client.properties file containing specific keys and values. The client.properties file are then mapped to an environment variable in the docker container. Refer to the `ENV` section in `stack-manager-input-config-service/bms-update-agent.json` for more information.

### 2.1. Set Route Client Properties File
The [Set Route](#31-set-route) requires a client.properties file that contains the endpoint of the [ESPHomeAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeAgent), endpoint of the [ESPHomeUpdateAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeUpdateAgent), credentials and endpoints to access the SPARQL endpoint of the knowledge graph. It should contain the following keys:
- `esphome.agent.toggle` the endpoint of the ESPHome Agent
- `esphome.update.agent.retrieve` the endpoint of the ESPHome Update Agent
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint
- `sparql.password` the password to access the SPARQL endpoint

More information can be found in the example property file `setClient.properties` in the `config` folder. You can either reuse this file or create a new one and modify the `ENV` section in `stack-manager-input-config-service/bms-update-agent.json` accordingly.

### 2.2. Write Route Client Properties File
This properties file is required for [Write Route](#32-write-route) and it only needs to be modified if the agent is to query for the `bacnetObjectId` and `bacnetDeviceId` from the knowledge graph. It needs to contain the credentials and endpoints to access the SPARQL endpoint of the knowledge graph. It should contain the following keys:
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint
- `sparql.password` the password to access the SPARQL endpoint

More information can be found in the example property file `writeClient.properties` in the `config` folder. You can either reuse this file or create a new one and modify the `ENV` section in `stack-manager-input-config-service/bms-update-agent.json` accordingly.

### 2.3. Update Triples Route Client Properties File
The [Update Triples Route](#33-update-triples-route) requires a client.properties file that contains the credentials and endpoints to access the PostGIS database and SPARQL endpoints of the knowledge graph. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the PostGIS database
- `db.user` the username to access the PostGIS database
- `db.password` the password to access the PostGIS database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint
- `sparql.password` the password to access the SPARQL endpoint

More information can be found in the example property file `updateTriplesClient.properties` in the `config` folder. You can either reuse this file or create a new one and modify the `ENV` section in `stack-manager-input-config-service/bms-update-agent.json` accordingly.

### 2.4. Update Present Value Route Client Properties File
The [Update Present Value Route](#34-update-present-value-route) requires a client.properties file that contains the credentials and endpoints to access the SPARQL endpoints of the knowledge graph. It should contain the following keys:
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username to access the SPARQL endpoint
- `sparql.password` the password to access the SPARQL endpoint

More information can be found in the example property file `updateTriplesClient.properties` in the `config` folder. You can either reuse this file or create a new one and modify the `ENV` section in `stack-manager-input-config-service/bms-update-agent.json` accordingly.

# 3. Usage
## 3.1. Set Route
The set route is currently limited to interactions with the [ESPHomeAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeAgent) and [ESPHomeUpdateAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ESPHomeUpdateAgent). It is able to update the setpoint in the knowledge graph and turn on/off a cooling fan based on the latest measured temperature and the setpoint. These parameters are required:
- `dataIRI` the data IRI of the setpoint
- `temperature` the setpoint temperature to update to the knowledge graph
- `clientProperties` the environment variable in the docker container that points to where the client.properties file is located at, refer to [Client Properties File](#2-client-properties-file) for more information.

### 3.1.1. Before Execution
Prepare a client.properties file with the necessary keys and values. Refer to [Set Route Client Properties File](#21-set-route-client-properties-file) for more information.

### 3.1.2. Execution
The agent accepts a POST request path `/set`. The following command will set the setpoint to `<TEMPERATURE>` and turn on/off a cooling fan based on the latest measured temperature and the setpoint.
```
curl -X POST 'http://localhost:3838/bms-update-agent/set' \
--header 'Content-Type: application/json' \
--data '{
    "dataIRI":"https://www.theworldavatar.com/kg/ontodevice/V_Setpoint-01-Temperature",
    "temperature":<TEMPERATURE>,
    "clientProperties":"SET_CLIENT_PROPERTIES"
}'
```
If the component is in the ON state.
```json
{"fanStatus":"The fan is in the ON state.","message":"The temperature has been set to <TEMPERATURE>"}
```

If the component is in the OFF state.
```json
{"fanStatus":"The fan is in the OFF state.","message":"The temperature has been set to <TEMPERATURE>"}
```

## 3.2. Write Route
The write route allows writing of values to Bacnet points via the [Wacnet API](https://hvac.io/docs/wacnet#orgheadline7). These parameters are required:
- `bacnetObjectId` the ID of the Bacnet Object to write to
- `bacnetDeviceId` the ID of the Bacnet Device that the object is assigned under
- `value` the value to write to the Bacnet Object

`bacnetObjectId` and `bacnetDeviceId` can either be provided in the request sent to the agent or it can be queried from the knowledge graph. 

In order for the agent to query for `bacnetObjectId` and `bacnetDeviceId`, the [Write Route Client Properties File](#22-write-route-client-properties-file) needs to be prepared and populated with the necessary keys and valuse. The following parameters are required in the request sent to the agent:
- `clientProperties` the environment variable in the docker container that points to where the client.properties file is located at, refer to [Client Properties File](#2-client-properties-file) for more information.
- `dataIRI` the data IRI that is linked to the `bacnetObjectId` and `bacnetDeviceId`.
- `value` the value to write to the Bacnet object

 The query run by the agent is structured based on [OntoBMS](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontobms/OntoBMS.owl):

```
<data IRI> ontobms:hasBacnetObjectID "bacnetObjectId" ; 
           ontobms:hasBacnetDeviceID "bacnetDeviceId" .
```

### 3.2.1. API Properties File
This properties file provides the agent with the URL of where the Wacnet Endpoint is located at. It should contain the following key:
- `api.url` the URL of where the Wacnet Endpoint is located at

More information can be found in the example property file `api.properties` in the `config` folder.

### 3.2.2. Before Execution
Prepare a client.properties file and a api.properties file with the necessary keys and values. Refer to [Write Route Client Properties File](#22-write-route-client-properties-file) and [API Properties File](#321-api-properties-file) for more information.


### 3.2.3. Execution
The agent accepts a POST request path `/wacnet/write`. There are two commands that can be utilised:
```
curl -X POST 'http://localhost:3838/bms-update-agent/wacnet/write' \
--header 'Content-Type: application/json' \
--data '{
    "bacnetObjectId":"<object Id>",
    "bacnetDeviceId":"<device Id>",
    "value":"<value>"
}'
```
```
curl -X POST 'http://localhost:3838/bms-update-agent/wacnet/write' \
--header 'Content-Type: application/json' \
--data '{
    "dataIRI":"<data IRI>",
    "value":"<value>",
    "clientProperties":"WRITE_CLIENT_PROPERTIES"
}'
```
A successful run will return the following:
```
{"message":"Successfully written <value> to the object with an ID: <object Id>"}
```

## 3.3. Update Triples Route
The Update Triples Route allows updating of the knowledge graph based on whether the latest timeseries value of a data IRI is equivalent to a user provided value. These parameters are required:
- `clientProperties` the environment variable in the docker container that points to where the client.properties file is located at, refer to [Client Properties File](#2-client-properties-file) for more information.
- `dataIRI` the data IRI to retrieve the latest timeseries value
- `triggerValue` the value to check against the data IRI latest timeseries value
- `DELETE` the set of triples to delete from the knowledge graph, this is optional. Refer to [Execution](#332-execution) for more information.
- `INSERT` the set of triples to insert into the knowledge graph, this is optional. Refer to [Execution](#332-execution) for more information.

### 3.3.1. Before Execution
Prepare a client.properties file with the necessary keys and values. Refer to [Update Triples Route Client Properties File](#23-update-triples-route-client-properties-file) for more information.

### 3.3.2. Execution
The agent accepts a POST request path `/updateTriples`. These are the commands that can be utilised:
This request deletes the following set of triples from the knowledge graph if the latest timeseries value of the data IRI is equivalent to the `triggerValue`.
```
curl -X POST 'http://localhost:3838/bms-update-agent/updateTriples' \
--header 'Content-Type: application/json' \
--data '{
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>",
    "triggerValue":"<value>",
    "DELETE":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_01> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_01> ."
    }]
}'
```
This request inserts the following set of triples from the knowledge graph if the latest timeseries value of the data IRI is equivalent to the `triggerValue`.
```
curl -X POST 'http://localhost:3838/bms-update-agent/updateTriples' \
--header 'Content-Type: application/json' \
--data '{
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>",
    "triggerValue":"<value>",
    "INSERT":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_01> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_01> . <https://www.theworldavatar.com/kg/ontobms/VAV_E7_02> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_02> . "
    }]
}'
```
This request inserts and deletes the following set of triples from the knowledge graph if the latest timeseries value of the data IRI is equivalent to the `triggerValue`. The sequence of insert and delete can be reversed as well.
```
curl -X POST 'http://localhost:3838/bms-update-agent/updateTriples' \
--header 'Content-Type: application/json' \
--data '{
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>",
    "triggerValue":"<value>",
    "INSERT":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_01> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_01> . <https://www.theworldavatar.com/kg/ontobms/VAV_E7_02> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_02> . ",
    "DELETE":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_01> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_01> ."
    }]
}'
```
It is possible to have the agent run multiple checks from one request. An example is shown below:
```
curl -X POST 'http://localhost:3838/bms-update-agent/updateTriples' \
--header 'Content-Type: application/json' \
--data '{ 
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>",
    "triggerValue":"<value>",
    "INSERT":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_01> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_01> . <https://www.theworldavatar.com/kg/ontobms/VAV_E7_02> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_>02 . ",
    "DELETE":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_03> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> \"10\" ."
    },
    {
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>",
    "triggerValue":"<value>",
    "DELETE":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_04> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> <https://www.theworldavatar.com/kg/ontobms/Setpoint_04> .",
    "INSERT":"<https://www.theworldavatar.com/kg/ontobms/VAV_E7_05> <https://www.theworldavatar.com/kg/ontodevice/hasSetpoint> \"10\" ."
    }]
}'
```

## 3.4. Update Present Value Route
The Update Present Value Route requires the user to provide a data IRI where the agent will then query for the Bacnet IDs and use the IDs to retrieve the present-value of the corresponding Bacnet Object via the Wacnet API. The agent will then update the following triples to reflect the present-value of the data IRI. This is currently applicable only to data IRIs that are of rdf:type om:Measure, refer to [ontology-of-units-of-measure](https://github.com/cambridge-cares/OM/tree/master):
```
<data IRI> rdf:type om:Measure.
<data IRI> om:hasNumericalValue "present-value" .
```
These parameters are required:
- `clientProperties` the environment variable in the docker container that points to where the client.properties file is located at, refer to [Client Properties File](#2-client-properties-file) for more information.
- `dataIRI` the data IRI to retrieve the Bacnet IDs and to update it's present-value

### 3.4.1. Before Execution
Prepare a client.properties file with the necessary keys and values. Refer to [Update Present Value Route Client Properties File](#24-update-present-value-route-client-properties-file) for more information.

### 3.4.2. Execution
The agent accepts a POST request path `/updatePresentValue`. These are the commands that can be utilised:
To execute the function for more than one data IRI:
```
curl -X POST 'http://localhost:3838/bms-update-agent/updatePresentValue' \
--header 'Content-Type: application/json' \
--data '{
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>"
    },
    {
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>"
    }]
}'
```
To execute the function for one data IRI:
```
curl -X POST 'http://localhost:3838/bms-update-agent/updatePresentValue' \
--header 'Content-Type: application/json' \
--data '{
  "checks": [{
    "clientProperties": "UPDATETRIPLES_CLIENT_PROPERTIES",
    "dataIRI":"<data IRI>"
    }]
}'
```
