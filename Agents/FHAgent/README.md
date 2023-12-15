# Fumehood agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding fumehood occupancy status.
The agent will retrieve the sensor readings from the Thingsboard (TB) server and determine the fumehood occupancy. The agent is also responsible for instantiating the derivations and agent instances of the occupancy derivation instances using the derivation client.

This agent is derived from the [ThingsboardInputAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/ThingsBoardAgent).
This agent has the basic same functionality to the ThingsBoardInputAgent. 
The difference lies in the agents ability to convert sensor reading to occupancy status before instantiating the timeseries. 

For information regarding Thingsboard API, refer to the following [ThingsBoard rest API documentation](https://thingsboard.io/docs/user-guide/telemetry/#data-query-rest-api).

## Usage

The agent is designed to derive the occupancy status of an entity based on the distance measured by a proximity sensor that is allocated to said entity.

The agent takes in the latest 600 average distance reading from TB server and calculates the occupancy status. The occupancy calculation is done on the following tally algorithm:
- The tally start from 0. 
- If the average distance is less than the threshold, add some value to the tally (eg.: 0.5), otherwise don't add any value. 
- After every reading, then reduce the value by a certain amount (eg.: -0.15). 
- If the tally exceed a certain tally limit, then the fumehood is in use. 
- A max and min value is set to the tally.
- Repeat for all average distance reading. Any change in occupancy is saved.

The occupancy result will then be calculated and instantiated. 

The agent will be packaged to a .war file for the deployment. As the agent will instantiate timeseries data and derivation instances, a connection to a POSTgresql database and a knowledge graph is required.

## Pre-requisites

This agent is used in tandem with the [DevInstAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DevInstAgent). The DevInstAgent is first used to instantiate the ABoxes for the Devices (Sensors, microcontrollers etc) and any measured or observed variables (length, occupancy status etc). An example of the request to instantiate the ABoxes for the proximity sensor, microcontroller, length and occupancy status can be found below:
```
<PlaceHolder>
```
The DevInstAgent will generate the IRIs for length and occupancy status accordingly. Note down the IRIs for the variables and insert them into the following:
1) Under the `config/mapping` folder, create a properties file similar to the examples files located there and add in the occupancy status IRI, for example:
```
occupiedState_FH-03=https://www.theworldavatar.com/kg/ontotimeseries/fh_occupiedState_FH-03_ffc2ca51-0485-49f1-ab6d-e2800b639b0b
```
2) Under the `agent.properties` file, edit `derivation.mapping` accordingly. For example, if length IRI = "https://www.theworldavatar.com/kg/ontoderivation/avgDist_FH-0356411cdc-f26b-4d62-a436-d43fe0b45da9" and occupied state IRI = "https://www.theworldavatar.com/kg/ontotimeseries/fh_occupiedState_FH-03_ffc2ca51-0485-49f1-ab6d-e2800b639b0b", `derivation.mapping` should be edited as such:

```
avgDist_FH:fh_occupiedState_FH-03
```

### Using the stack
The agent can function with and without using the stack. A config file is provided in `./stack-manager-input-config-service/`

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine 
or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README
to explain the set-up of a knowledge graph triple store or Postgres database.

### Config Files
The agent requires 3 configuration file to configure, with an optional IRI map text file.

#### agent.properties
Contains config for the agent operations such as calculations and instantiations.

- `thingsboard.mappingfolder` : Path to the folder containing IRI mapping for timeseries instantiation. Each timeseries IRI is contained in a single file.
- `derivation.mapping` : Maps the raw varables to the derived variables. The raw variable name must be the same with the TB variable keyname. The format is the following: `raw1:deriv1, raw2:deriv2, raw3:deriv1, ...`
- `threshold.tally` : The distance threshold for the tally system
- `tally.limit` : The tally threshold for the occupancy calculation system
- `tally.max` : The tally maximum value for the occupancy calculation system
- `tally.min` : The tally minimum value for the occupancy calculation system
- `decrease.factor` : The factor the tally is decreased by during tally calculation
- `increase.factor` ; The factor the tally is increased by when the distance threshold is breached 
- `derivation.baseurl` : The derivation instances base iri

#### api.properties
Contains the parameters for Thingsboard API

- `thingsboard.username` : Thingsboard server account username
- `thingsboard.password` : Thingsboard server account password
- `path.url` the URL for where the ThingsBoard server is located, the default should be http://localhost:8080. 
- `deviceId` the device Id that indicates which device to retrieve the data from. 
- `keys` the keys that represents the parameters/readings to be retrieved from the ThingsBoard server.

#### client.properties
Contains parameter for timeseries client and knowledge graph endpoints.

- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` Username to be used when accessing the knowledge graph. Optional. 
- `sparql.password` Password to be used when accessing the knowledge graph. Optional.

#### iriMapping.txt
 This file contains the IRI mapping for all raw and derived variable. This file will automatically be generated if not found and all the IRI will be created on call and hence, this file is not included in the repository. The format for the mapping is the following: 
 ```
 var1 = iri1
 var2 = iri2
 ```

This file is made to ensure the IRI of instances created is consistent over multiple call of the agent. This is to prevent instantiating the same object multiple times under the same IRI. If this file is deleted, the agent will generate a new IRI without deleting the previous instance from the knowledge graph, causing the same instance to exist under different IRI. Hence, do back up this file before deleting agent containers.

#### Mapping files
Files under the `config/mapping` folder are properties file for timeseries instantiations. The IRI generated will be saved in these files and are consistent with contents of `iriMapping.txt`. Each file contains derived variables mapped to their respective IRI. If left empty, an IRI will be generated. Different timeseries will require different file.


### Before building and running the agent
The Fumehood Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Modify `api.properties` and `client.properties` in the `config` folder accordingly. You should not modify the `agent.properties` file as the Dockerfile will set the environment variable 
THINGSBOARD_AGENT_MAPPINGS to point towards the location of the mapping folder. The Dockerfile will copy all 3 properties files and mapping folder and set environment variables pointing 
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually. The `config` folder will be mounted onto the docker container as a volume and changes can be made to the properties files even after the agent has been deployed.

### Building the agent

The agent can be depoyed as part of the stack or as a standalone.

#### Part of the stack

To build the agent's image, open up the command prompt in the same directory as this README, run
```
docker-compose build
```
Open `stack-manager-input-config-service/fh-agent.json` and under the Mounts section, modify the Source and insert the filepath of where the config folder is located at (For Windows users using WSL on Docker, the file path should start with /mnt/c/, which is equivalent to C://).

Copy `stack-manager-input-config-service/fh-agent.json` to the services folder under your stack-manager directory (By default it should be TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/) and start up the stack.

The agent is reachable at "fh-agent/retrieve" or "fh-agent/instantiate" on localhost port 3838 by default.

#### Standalone

To build and deploy the agent, open up the command prompt in the same directory as this README, run
```
docker compose up -d
```

The agent is reachable at "fh-agent/retrieve" or "fh-agent/instantiate" on localhost port 1010. 

### Running the agent

The agent currently has two functions:
- "fh-agent/retrieve" will calculate the occupancy status and instantiate timeseries instances
- "fh-agent/instantiate" will instantiate derivation instances

Both URL pattern took in POST request. Follow the request shown below:

```
POST http://localhost:<port number>/fh-agent/retrieve
Content-Type: application/json
{"agentProperties":"THINGSBOARD_AGENTPROPERTIES","apiProperties":"THINGSBOARD_APIPROPERTIES","clientProperties":"THINGSBOARD_CLIENTPROPERTIES", "iriMapFile":"IRI_MAPPINGSTORE"}
```
or

```
POST http://localhost:<port number>/fh-agent/instantiate
Content-Type: application/json
{"agentProperties":"THINGSBOARD_AGENTPROPERTIES","apiProperties":"THINGSBOARD_APIPROPERTIES","clientProperties":"THINGSBOARD_CLIENTPROPERTIES", "iriMapFile":"IRI_MAPPINGSTORE"}
```

The order of instantiation, whether the derivation (`/instantiate`) or the timeseries instantiation (`/retrieve`) is called first does not matter. The IRI should remain consistent as per `iriMapping.txt` file generated by the agent.