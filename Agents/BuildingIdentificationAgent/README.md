# BuildingIdentification agent


The BuildingIdentificationAgent queries factories' coordinates and heat emissions from a blazegraph namespace. It then queries buildings data from a user-specified 
POSTGRES database and assigns a building to each factory whose footprint polygon is closest to its coordinates.  

The agent will create a POSTGRES table called 'factories' with the following columns in the user-specified database and the 'citydb' schema : iri, height, heat, factory_type, name, building_id. The meanings of these names are as follows:



| Column name                      |            Meaning                                                  | 
|:--------------------------------:|:-------------------------------------------------------------------:|
|    ```iri```                     |       Factory IRI in blazegraph namespace                           |
|    ```height```                  |       Height of building assigned to this factory in meters.        |
|    ```heat```                    |       Heat emissions of this building in megawatts.                 |
|    ```factory_type```            |       Type of factory                                               |
|    ```name```                    |       Name of company operating this factory                        |
|    ```building_id```             |       ID of building as given in building and cityobject tables.    |


## 1. Agent Deployment

The agent is designed to be run within a Docker container as part of a stack.

### 1.1 Preparation
#### Maven repository credentials
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central). You'll need to provide your credentials (github username/personal access token) in single-word text files located:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

#### Stack containers

This agent requires the following tools, which **MUST** run on the same stack. The details for setting them up are explained at [stack manager page](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).


(1) SPARQL endpoint

There must be an existing namespace in the stack blazegraph which contains the heat emissions and coordinates data of various companies. The coordinates must be in the EPSG:4326 CRS. These are converted internally to the CRS used in the PostgreSQL database. These triples must be instantiated according to the [OntoCompany](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocompany) ontology. This can be done by running the [heat instantiation agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1690-add-heat-instantiation-agent/Agents/HeatInstantiationAgent) followed by the [heat emission agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1683-update-heat-agent-queries/Agents/HeatEmissionAgent).

(2) PostgreSQL database

The agent is designed to use the stack PostgreSQL. It requires the buildings data to be stored in a schema called 'citydb' in the user-specified database. This schema must have three tables called 'database_srs', 'building' and 'cityobject'. The building height is retrieved from the 'measured_height' column of the 'building' table. The building footprint is obtained from the 'wkb' column of the 'cityobject' table. The EPSG coordinate reference system used in the database is retrieved from the 'database_srs' table. The record for a single building must have identical values for the 'id' column in the 'building' and 'cityobject' tables. This id will appear in the 'building_id' column of the 'factories' table created by this agent.

### 1.2 Docker deployment

- Build this agent's image by executing `docker compose build` within this folder. Do not start the container.
- Copy the `buildingidentificationagent.json` file from the `stack-manager-input-config` folder into the `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager.
- Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

## 2. Agent Routes

The agent supports POST requests and is reachable at http://localhost:3838/buildingidentificationagent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. The agent provides a single endpoint at http://localhost:3838/buildingidentificationagent/run, which accepts the following POST request parameters :

- ```maxDistance```: The maximum allowable distance between the centre of the matched building footprint and the factories' coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs.
- ```dbName```: Name of the database in the stack from which to query the buildings data. This database must contain the buildings data in the 'citydb' schema.
- ```namespace```: A namespace in the stack blazegraph containing the factories' coordinates, heat emissions and other data.

The following is an example POST request :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","dbName":"test","namespace":"sgbusinessunits"}'  "localhost:3838/buildingidentificationagent/run"
```

Upon successful completion, the agent returns a JSON object indicating the number of factories queries from the Blazegraph namespace and the number of buildings matched. Both numbers should be equal. A table called 'factories' will be created in the 'citydb' schema of the user-specified database with the columns mentioned above.






