# BMS Instantiation Agent

This agent is designed to instantiate devices found in Building Management System (BMS) based on [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice), [OntoBMS](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontobms) and [OntoCAPE](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocape). 

The agent mainly uses the [remote store client](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/query/RemoteStoreClient.java) from the JPS_BASE_LIB to interact with the knowledge graph.

## Usage 
This part of the README describes the usage of the agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the remote store client to interact with the knowledge graph , the knowledge graph will be required to be set-up beforehand.  

The [next section](#requirements) will explain the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint. These can run on the same machine or need to be accessible from the host machine via a fixed URL. This can be either in the form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store.

### Property file
For running the agent, one properties files are required:
- One [property file for the remote store client](#Client-properties) defining the SPARQL endpoints of the triple store.

#### Client properties
The client properties file needs to contain the endpoints of the triple store. It should contain the following keys:
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

### Building the Agent
The Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).


Modify `client.properties` in the `config` folder accordingly. Do note that the properties file is mounted onto the agent's docker container thus any changes in the properties files does not require the docker container to be rebuilt.

Modify `data.csv` in the `config` folder based on the device to be instantiated. Refer to the template csv files in the `config` folder for the keys and values to include for a specific type of device. 

For templates with [REQUIRED] and [OPTIONAL] sections, all keys and values directly below [REQUIRED] should be kept in `data.csv`. Keys and values below [OPTIONAL] are optional fields that should be deleted if they are not applicable. 

All fields in an optional section have to be provided for the optional data to be instantiated. Each optional section is separated by an empty line. Refer to `Pipe template.csv` for an example device with optional keys and values.

The agent will need to be extended if the device to be instantiated cannot be found in the template files.

#### Deployment
To build and start the agent, open up the command prompt in the same directory as this `README`, run
```
docker-compose up -d
```

To run the agent, a request must be sent to http://localhost:1027/bms-instantiation-agent/instantiate. The request has the following format:
```
curl -X POST --header "Content-Type: application/json" -d "{\"clientProperties\":\"CLIENTPROPERTIES\",\"Device\":\"Device type\"}" http://localhost:1027/bms-instantiation-agent/instantiate
```
and it should return:
```
{"Result":["Input agent object initialized.","The device has been instantiated."]}
```
Change the Device type accordingly. The values currently available are "FumeHood", "WalkInFumeHood", "CanopyHood_VAV", "CanopyHood_CAV", "Pipe", "Duct", "Valve", "PreCoolCoil", "MidCoil", "OffCoil", "Filter", "Fan", "T-Joint", "Cooling", "MakeupAirUnit", "VAV-S", "VAV-E", "Damper", "ExhaustFan", "FCU-HE", "FanCoilUnit". An example request for instantiating a fumehood can be found below:

```
curl -X POST --header "Content-Type: application/json" -d "{\"clientProperties\":\"CLIENTPROPERTIES\",\"Device\":\"FumeHood\"}" http://localhost:1027/bms-instantiation-agent/instantiate
```
The agent is designed to instantiate one device per request. To instantiate one device after another, edit `data.csv` accordingly before sending another request to the agent.

## Note
This agent is able to carry out the above mentioned functions but it is unfinished and missing the following:
1) unit tests