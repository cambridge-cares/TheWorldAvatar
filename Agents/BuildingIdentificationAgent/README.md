# BuildingIdentification agent


The BuildingIdentificationAgent takes a list of geospatial coordinates as input. These coordinates can specified as part of the JSONObject sent in the POST request to this agent. Alternatively, the user can store these coordinates as Point geometries in a PostgreSQL table and specify the name of this table. The agent identifies the building nearest to each point by querying buildings footprint data from the "citydb" schema of the "postgres" database in the stack. The identification numbers of the matched buildings are returned within a JSONObject. If the list of coordinates was queried from a user-specified PostgreSQL table, these identification numbers will be appended as a column with the name "building_id" to the same table. This "building_id" column will have the integer data type.



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

The agent is designed to use the stack PostgreSQL. It requires the buildings data to be stored in a schema called 'citydb' in the "postgres" database. This schema must have two tables called 'database_srs'and 'cityobject'. The building footprint is obtained from the 'envelope' column of the 'cityobject' table. The EPSG coordinate reference system used in the database is retrieved from the 'database_srs' table. The integer in the 'id' column of the 'cityobject' table will appear in the JSONObject returned by this agent for the matched buildings.

If the user-specified coordinates are stored as Point geometries in a PostgreSQL table, the column containing these geometries must have the name "geometry" and an associated SRID. The agent automatically converts the coordinates from the SRID in the user-specified table to the SRID used to store the building footprints in the "citydb" schema. The user-specified table must also have a column called 'ogc_fid' containing integers which are unique and non-null. This requirement can be automatically satisfied if the table was created using the [stack data uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader). In addition, the user-specified table must be in the same "postgres" database that contains the buildings data.

### 1.2 Docker deployment

- Build this agent's image by executing `docker compose build` within this folder. Do not start the container.
- Copy the `buildingidentificationagent.json` file from the `stack-manager-input-config` folder into the `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager.
- Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

## 2. Agent Routes

The agent supports POST requests and is reachable at http://localhost:3838/buildingidentificationagent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. 

The agent provides the following two API routes: 

### 2.1 Array Route (http://localhost:3838/buildingidentificationagent/array)

This endpoint should be used if the user wishes to specify a list of coordinates in the form of a JSON Array. This endpoint accepts the following POST request parameters. All the parameters except the first are optional :

- ```coordinates```: The value of this parameter should be a 2D JSONArray containing the coordinates of the points to be matched. 
- ```maxDistance``` (Optional): The maximum allowable distance between the centre of the matched building footprint and the specified coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs. The default value of this parameter is 100 meters.
- ```srid``` (Optional): An integer specifying the coordinate reference system of the input coordinates. The input coordinates will be converted to the SRID corresponding to the buildings data in the citydb schema as required. If the value of this parameter is not specified, a default value of 4326 is assumed. 

The following is an example POST request for the array route :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","srid":"4326","coordinates":[[103.67455581177452, 1.2711156279472327], [103.68455581177452, 1.2611156279472327]]}'  "localhost:3838/buildingidentificationagent/array"
```

### 2.2 Table route (http://localhost:3838/buildingidentificationagent/table)

This endpoint should be used when the user has stored the coordinates that need to be matched as Point geometries in a PostgreSQL table. The column containing these geometries must have the name "geometry" and an associated SRID. The agent automatically converts the coordinates from the SRID in the user-specified table to the SRID used to store the building footprints in the "citydb" schema. The user-specified table must also have a column called 'ogc_fid' containing integers which are unique and non-null.

This endpoint accepts the following POST request parameters.  :

- ```table```: The name of the PostgreSQL table containing the coordinates to be matched as POSTGIS Point geometries. If the table is not located within the "public" schema, the schema name should also be included. For example, if the table is called "test" and it is located in the "industry" schema, the value of this parameter should be specified as the string "industry.test". This table must be stored in a schema within the "postgres" database.
- ```maxDistance``` (Optional): The maximum allowable distance between the centre of the matched building footprint and the specified coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs. The default value of this parameter is 100 meters.

The following is an example POST request for the table route, assuming that the user has created a table called "test" in a schema called "industry" with the required format :


```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","table":"industry.test"}'  "localhost:3838/buildingidentificationagent/table"
```

Upon successful completion, the agent returns a JSONObject indicating the number of buildings matched and their corresponding ids. The following is a representative example: 

```
{"building_id":[3571,3838],"number_matched":2}
```

For the table route, the list of building identification numbers are also appended as an additional column called "building_id" to the user-specified table.




