# Building Identification Agent


The Building Identification Agent takes a list of geometries as input. If these geometries are points, they can specified as part of the JSONObject sent in the POST request to this agent. Alternatively, the user can store these geometries in a PostgreSQL table and specify the name of this table. The agent retrieves the Universally Unique Identifier (UUID) of the buildings associated with each geometry by querying buildings footprint data from the 'citydb' schema of the 'postgres' database in the stack. These UUIDs are returned within a JSONObject. If the geometries were queried from a user-specified PostgreSQL table, the building UUIDs are returned to the user in two possible formats. If the user specifies that there should only be one building matched to each geometry, the building UUIDs are appended as a column with the name 'building_uuid' to the same table. This 'building_uuid' column will have the character varying PostgreSQL data type. Alternatively, if the user specifies that multiple buildings need to be matched with each input geometry, a new table is created containing the unique identifiers from the user-specified table and the UUIDs of the buildings matched to each user-specified geometry.


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

The agent is designed to use the stack PostgreSQL. It requires the buildings data to be stored in a schema called 'citydb' in the 'postgres' database. The buildings data can be instantiated in the required format by uploading the raw data using the [stack data uploader](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader). The user-specified table must be in the same 'postgres' database that contains the buildings data.

### 1.2 Docker deployment

- Build this agent's image by executing `docker compose build` within this folder. Do not start the container.
- Copy the `buildingidentificationagent.json` file from the `stack-manager-input-config` folder into the `TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services` folder of the stack manager.
- Start the stack manager as usual following [these instructions](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager).

## 2. Agent Routes

The agent supports POST requests and is reachable at http://localhost:3838/buildingidentificationagent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. 

The agent provides the following two API routes: 

### 2.1 Location Route (http://localhost:3838/buildingidentificationagent/location)

This endpoint should be used if the user wishes to specify a list of coordinates in the form of a JSON Array. This endpoint accepts the following POST request parameters. All the parameters except the first are optional :

- ```coordinates```: The value of this parameter should be a 2D JSONArray containing the coordinates of the points to be matched. 
- ```maxDistance``` (Optional): The maximum allowable distance between the centre of the matched building footprint and the specified coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs. The default value of this parameter is 100 meters.
- ```srid``` (Optional): An integer specifying the coordinate reference system of the input coordinates. The input coordinates will be converted to the SRID corresponding to the buildings data in the citydb schema as required. If the value of this parameter is not specified, a default value of 4326 is assumed.

The following is an example POST request for the location route :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","srid":"4326","coordinates":[[103.67455581177452, 1.2711156279472327], [103.68455581177452, 1.2611156279472327]]}'  "localhost:3838/buildingidentificationagent/location"
```

Upon successful completion, the agent returns a JSONObject indicating the number of buildings matched and their corresponding UUIDs. The following is a representative example: 

```
{"number_matched":2,"building_iri":["UUID_fe4d706f-eb92-4382-ba38-9b953af41301","UUID_b86a1c8a-84b8-4ce9-8df6-72016de1fb54"]}
```

### 2.2 Postgis route (http://localhost:3838/buildingidentificationagent/postgis)

This endpoint should be used when the user has stored the geometries that need to be matched in a PostgreSQL table. If the name of the column containing these geometries is not specified, a default value of "geometry" is assumed. The geometries need to have an associated SRID. The agent automatically converts the geometries from the SRID in the user-specified table to the SRID used to store the building footprints in the "citydb" schema. The user-specified table must also have a column called 'ogc_fid' containing integers which are unique and non-null.

This endpoint accepts the following POST request parameters.  :

- ```table```: The name of the PostgreSQL table containing the geometries to be matched. If the table is not located within the "public" schema, the schema name should also be included. For example, if the table is called "test" and it is located in the "industry" schema, the value of this parameter should be specified as the string "industry.test". This table must be stored in a schema within the "postgres" database.
- ```column```: The column in the PostgreSQL table containing the geometries to be matched.
- ```maxDistance``` (Optional): The maximum allowable distance between the centre of the matched building footprint and the specified coordinates in meters. If the nearest building is further than ```maxDistance``` from a particular user-specified geometry, it will not be matched to any building. The default value of this parameter is 100 meters. This parameter is only applicable if the geometries in the user-specified table and column are points.
- ```overlapFraction```: If the user-specified geometries are polygons, the matching is performed based on the extent of overlap between each polygon and the buildings with which it intersects. The ratio of the intersection area to the user-specified polygon is required to be at least as large as overlapFraction in order for matching to occur.
- ```oneToMany``` (Optional): A Boolean parameter specifying whether each user-specified geometry should be associated with one or several buildings. If this parameter value is set to "true", the code identifies the geometry in the user-specified column closest to each building while excluding geometries based on user-specified attributes. The default value of this parameter is false.
The following optional parameters are only applicable if ```oneToMany``` is "true".
- ```newTable``` (Optional): The name of the table in which the results of the matching should be stored. By default, this table is called 'matched_buildings' and it is created in the 'public' schema.
- ```filterColumns``` (Optional): A JSONArray of strings specifying the columns of the user-specified table containing the attributes required to exclude unwanted geometries. This parameter
- ```excludedValues``` (Optional): A JSONArray of JSONArrays. Each nested JSONArray contains a list of strings specifying values of the corresponding attribute specified in ```filterColumns``` which should be excluded from the matching.
- ```mapping``` (Optional): The name of an Ontop mapping file specifying the triples linking the user-specified geometries to the buildings to which they are matched. The path of this mapping file should be specified as the value of the "Source" key in line 10 of "stack-manager-input-config\buildingidentificationagent.json". This ensures that the local file path is mapped to the 'resources' folder of the agent's Docker container through a bind mount. 


For both routes, the desired values of various input parameters can be set in the input.json file. Subsequently, the POST request is sent to the agent as follows:

```
curl -X POST -H "Content-Type: application/json" -d @input.json  "localhost:3838/buildingidentificationagent/postgis"
```

The POST request can also be manually typed out as in the following example, which assumes that the user has created a table called "landplot" in the "public" schema with the required format :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","table":"public.landplot","column":"lod1Geometry","oneToMany":"true","filterColumns":["LU_DESC"],"excludedValues":[["ROAD", "PARK"]]}'  "localhost:3838/buildingidentificationagent/postgis"
```

Upon successful completion, the agent returns a JSONObject indicating the number of buildings matched. The following is a representative example: 

```
{"number_matched":106359}
```

If ```oneToMany`` is false, the list of building UUIDs are appended as an additional column called "building_uuid" to the user-specified table. If it is true, a new table is created containing the building IRIs matched to each user-specified geometry. 




