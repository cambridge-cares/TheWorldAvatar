# BuildingIdentification agent


The BuildingIdentificationAgent takes a set of geospatial coordinates as input in the form of a 2D JSON array. It identifies the building nearest to each point by querying buildings footprint data from the "citydb" schema of the "postgres" database in the stack. The identification numbers of the matched buildings are returned within a JSONObject.


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


(1) PostgreSQL database

The agent is designed to use the stack PostgreSQL. It requires the buildings data to be stored in a schema called 'citydb' in the "postgres" database. This schema must have three tables called 'database_srs', 'building' and 'cityobject'. The building height is retrieved from the 'measured_height' column of the 'building' table. The building footprint is obtained from the 'wkb' column of the 'cityobject' table. The EPSG coordinate reference system used in the database is retrieved from the 'database_srs' table. The record for a single building must have identical values for the 'id' column in the 'building' and 'cityobject' tables. This id will appear in the JSONObject returned by this agent.

### 1.2 Docker deployment

- Build this agent's image by executing `docker compose build` within this folder. Do not start the container.
- Copy the `buildingidentificationagent.json` file from the `stack-manager-input-config` folder into the `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager.
- Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

## 2. Agent Routes

The agent supports POST requests and is reachable at http://localhost:3838/buildingidentificationagent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. The agent provides a single endpoint at http://localhost:3838/buildingidentificationagent/run, which accepts the following POST request parameters. All the parameters except the first are optional :

- ```coordinates```: The value of this parameter should be a 2D JSONArray containing the coordinates of the points to be matched. 
- ```maxDistance``` [Optional]: The maximum allowable distance between the centre of the matched building footprint and the specified coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs. The default value of this parameter is 100 meters.
- ```srid``` [Optional]: An integer specifying the coordinate reference system of the input coordinates. The input coordinates will be converted to the SRID corresponding to the buildings data in the citydb schema as required.

The following is an example POST request :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","srid":"4326","coordinates":[[103.67455581177452, 1.2711156279472327], [103.68455581177452, 1.2611156279472327]]}'  "localhost:3838/buildingidentificationagent/run"
```

Upon successful completion, the agent returns a JSONObject indicating the number of buildings matched and their corresponding ids.  





