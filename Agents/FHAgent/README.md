# Fumehood agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding fumehood occupancy status.
The agent will retrieve the sensor readings from the Thingsboard (TB) server and determine the fumehood occupancy. The agent is also responsible for instantiating the derivations and agent instances of the occupancy derivation instances using the derivation client.

This agent is derived from the [ThingsboardInputAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/dev-1505-proximity-sensor-for-lab_FHAgent/Agents/ThingsBoardAgent).
This agent has the basic same functionality to the ThingsBoardInputAgent. 
The difference lies in the agents ability to convert sensor reading to occupancy status before instantiating the timeseries. 

For information regarding Thingsboard API, refer to the following [documentation from ThingsboardInputAgent](https://github.com/cambridge-cares/TheWorldAvatar/blob/dev-1505-proximity-sensor-for-lab_FHAgent/Agents/ThingsBoardAgent/README.md#thingsboard-api).

## Usage

The agent is connected to a proximity sensor connected to a microcontroller. This module is attached to a fumehood and reads the average distance between the sensor an the nearest object in front of the fumehood. 

The agent takes in the latest 600 average distance reading from TB server and calculates the occupancy status. The occupancy calculation is done on the following tally algorithm:
- The tally start from 0. 
- If the average distance is less than the threshold, add some value to the tally (eg.: 0.5), otherwise don't add any value. 
- After every reading, then reduce the value by a certain amount (eg.: -0.15). 
- If the tally exceed a certain tally limit, then the fumehood is in use. 
- A max and min value is set to the tally.
- Repeat for all average distance reading. Any change in occupancy is saved.

The occupancy result will then be calculated and instantiated. 

The agent will be packaged to a .war file for the deployment. As the agent will instantiate timeseries data and derivation instances, a connection to a POSTgresql database and a knowledge graph is required.

### Using the stack
The agent can function with and without using the stack. The option for this is available in the `agent.properties` file.
Setting the `use_stack` properties to `true` will send the data to the data bridge agent via the url specified in the `data_bridge.url` property.

If used without the stack, the JDBC URL and SPARQL endpoint provided in the `client.properties` file will be used as the Postgres DB and triple store.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine 
or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README
to explain the set-up of a knowledge graph triple store or Postgres database.

### Config Files
The agent requires 3 configuration file to configure, with an optional IRI map text file.

#### agent.properties
Contains config for the agent operations such as calculations and instantiations.

- `thinsboard.mappingfolder` : Path to the folder containing IRI mapping for timeseries instantiation. Each timeseries IRI is contained in a single file.
- `data_bridge.url` : URL to the data bridge agent
- `data_bridge.JDBC_end` : Endpoint URL of the stack's JDBC target URL. Optional, leave empty if using the data bridge agent's default endpoint
- `data_bridge.kg_end` : Endpoint URL of the stack's target SPARQL endpoint. Optional, leave empty if using the data bridge agent's default endpoint
- `use_stack` : Boolean whether the agent is using the stack or not
- `derivation.mapping` : Maps the raw varables to the derived variables. The raw variable name must be the same with the TB variable keyname. The format is the following: `raw1:deriv1, raw2:deriv2, raw3:deriv1, ...`
- `threshold.tally` : The distance threshold for the tally system
- `tally.limit` : The tally threshold for the occupancy calculation system
- `tally.max` : The tally maximum value for the occupancy calculation system
- `tally.min` : The tally minimum value for the occupancy calculation system
- `decrease.factor` : The factor the tally is decreased by during tally calculation
- `increase.factor` ; The factor the tal;ly is increased by when the distance threshold is breached 
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

#### iriMapping.txt
 This file contains the IRI mapping for all raw and derived variable. This file will automatically be generated if not found and all the IRI will be created on call and hence, this file is not included in the repository. The format for the mapping is the following: 
 ```
 var1 = iri1
 var2 = iri2
 ```

This file is made to ensure the IRI of instances created is consistent over multiple call of the agent. This is to prevent instantiating the same object multiple times under the same IRI. If this file is deleted, the agent will generate a new IRI without deleting the previous instance from the knowledge graph, causing the same instance to exist under different IRI. Hence, do back up this file before deleting agent containers.

#### Mapping files
Files under the `config/mapping` folder are properties file for timeseries instantiations. The IRI generated will be saved in these files and are consistent with contents of `iriMapping.txt`. Each file contains derived variables mapped to their respective IRI. If left empty, an IRI will be generated. Different timeseries will require different file.


### Building the Fumehood Agent
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
to their location thus you do not need to shift the properties files and mapping folder nor add in environment variables manually.

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

### Running the agent

The agent is reachable at "fh-agent/retrieve" or "fh-agent/instantiate" on localhost port 1010. 
- "fh-agent/retrieve" will calculate the occupancy status and instantiate timeseries instances
- "fh-agent/instantiate" will instantiate derivation instances

Both URL pattern took in POST request. Follow the request shown below:

```
POST http://localhost:1010/fh-agent/retrieve
Content-Type: application/json
{"agentProperties":"THINGSBOARD_AGENTPROPERTIES","apiProperties":"THINGSBOARD_APIPROPERTIES","clientProperties":"THINGSBOARD_CLIENTPROPERTIES", "iriMapFile":"IRI_MAPPINGSTORE"}
```
or

```
POST http://localhost:1010/fh-agent/instantiate
Content-Type: application/json
{"agentProperties":"THINGSBOARD_AGENTPROPERTIES","apiProperties":"THINGSBOARD_APIPROPERTIES","clientProperties":"THINGSBOARD_CLIENTPROPERTIES", "iriMapFile":"IRI_MAPPINGSTORE"}
```

The order of instantiation, whether the derivation (`/instantiate`) or the timeseries instantiation (`/retrieve`) is called first does not matter. The IRI should remain consistent as per `iriMapping.txt` file generated by the agent.