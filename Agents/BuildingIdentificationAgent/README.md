# BuildingIdentification agent

## 1. Agent Description

The BuildingIdentificationAgent queries factories' coordinates and heat emissions from a blazegraph namespace. It then queries buildings data from a user-specified 
POSTGRES database and assigns a building to each factory whose footprint polygon is closest to its coordinates.  

The agent will create a POSTGRES table called 'factories' with the following columns in the user-specified database and the 'citydb' schema : iri, height, heat, factory_type, name, building_id. The meanings of these names are as follows:

building ID, building geometry, building height, factory IRI, factory heat emissions and factory class.

| Column name                      |            Meaning                                                  | 
|:--------------------------------:|:-------------------------------------------------------------------:|
|    ```iri```                     |       Factory IRI in blazegraph namespace                           |
|    ```height```                  |       Height of building assigned to this factory in meters.        |
|    ```heat```                    |       Heat emissions of this building in megawatts.                 |
|    ```factory_type```            |       Type of factory                                               |
|    ```name```                    |       Name of company operating this factory                        |
|    ```building_id```             |       ID of building as given in buildings and cityobject tables.   |


## 2. Build Instructions

### 2.1. Required credentials
The docker image uses TheWorldAvatar maven repository (https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). You'll need to provide your credentials (github username/personal access token) in single-word text files located:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 2.2. Stack Set Up
The agent is designed to run in the stack. To start the stack, spin up the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager).

### 2.3. Blazegraph Set Up
The agent is designed to use the stack Blazegraph. There must be an existing namespace in the stack blazegraph which contains the heat emissions data of various companies. These triples must be instantiated according to the OntoCompany (https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocompany) ontology. 


### 2.5. PostgreSQL Set Up
The agent is designed to use the stack PostgreSQL. It requires the buildings data to be stored in a schema called 'citydb' in the user-specified database. This schema must have three tables called 'database_srs', 'building' and 'cityobject'. The building height is retrieved from the 'measured_height' column of the 'building' table. The building footprint is obtained from the 'wkb' column of the 'cityobject' table. 



### 2.6. Build and Run
In the same directory as this README, first build the Docker image by running
```
docker-compose build
```

After the image is built, copy ```./stack-manager-input-config/buildingidentificationagent.json``` and place it in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager.

The agent supports POST requests and is reachable at http://localhost:3838/buildingidentificationagent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. The agent provides a single endpoint at http://localhost:3838/buildingidentificationagent/run, which accepts the following POST request parameters :

- ```maxDistance```: The maximum allowable distance between the centre of the matched building footprint and the factories' coordinates in meters. If the nearest building is further than ```maxDistance```, the matching still takes place but a warning is printed in the Docker logs.
- ```dbName```: Name of the database in the stack from which to query the buildings data. This database must contain the buildings data in the 'citydb' schema.
- ```namespace```: A namespace in the stack blazegraph containing the factories' coordinates, heat emissions and other data.

The following is an example POST request :

```
curl -X POST -H "Content-Type: application/json" -d '{"maxDistance":"100.0","dbName":"test","namespace":"sgbusinessunits"}'  "localhost:3838/buildingidentificationagent/run"
```



### 2.7. Debugging
To debug, put ```./stack-manager-input-config/buildingidentificationagent-debug.json``` instead of ```./stack-manager-input-config/openmeteo-agent.json```  in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager. The debugger port will be available at 5005.







