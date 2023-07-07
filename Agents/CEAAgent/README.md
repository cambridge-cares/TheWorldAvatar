# CEA Agent
## 1. Agent Description

The CEA Agent can be used to interact with the [City Energy Analyst (CEA)](https://www.cityenergyanalyst.com/)  and the data it produces on building energy demands and installable solar energy generators.

The agent currently queries for building geometry, surrounding buildings geometries, building usage and historical weather data stored in knowledge graph, and terrain data from PostGIS, which are passed to CEA as inputs. The energy demands and potential energy of solar energy generators (which include photovoltaic panels, photovoltaic-thermal collectors, and solar collectors) calculated by CEA are extracted by the agent and stored on the knowledge graph.

## 2. Build Instructions

### 2.1. Maven

The docker image uses TheWorldAvatar maven repository (https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/).
You will need to provide your credentials (github username/personal access token) in single-word text files located:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
### 2.2. Stack Set Up
The agent is designed to run in the stack. To start the stack, spin up the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Deploy/stacks/dynamic/stack-manager).

### 2.3. Access Agent Set Up
The agent is designed to use the [Access Agent](../AccessAgent). Spin the access agent up as part of the same stack as spun up by the Stack Manager.

### 2.4. Blazegraph Set Up
The agent is designed to use the stack Blazegraph. Please ensure that the routing information and the Blazegraph namespace corresponding to ```cea.label``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```, is set up in the stack Blazegraph. See the [Access Agent README](../AccessAgent) for instruction on uploading routing information.

### 2.5. PostGIS Set Up
The agent is designed to use the stack PostGIS. The calculation results of CEA will be stored in the stack PostGIS database, which is specified as ```cea.database``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```. Please ensure the database specified by ```cea.database``` is set up in the stack PostGIS.

### 2.6. TargetresourceID for Building Geometry Query
For building geometry queries, the CEA agent defaults to TheWorldAvatar Blazegprah. The targetResourceIDs in ```./cea-agent/src/main/resources/CEAAgentConfig.properties``` provides mappings to namespaces in TheWorldAvatar Blazegraph (http://www.theworldavatar.com:83/citieskg/). The targetResourceID are passed to the access agent in order to query from TheWorldAvatar Blazegraph. Check that a targetResourceID to pass to the access agent exists for the namespace being used for SPARQL queries.
Currently included are:
- ```citieskg-berlin```
- ```singaporeEPSG24500```
- ```citieskg-singaporeEPSG4326```
- ```citieskg-kingslynnEPSG3857```
- ```citieskg-kingslynnEPSG27700```
- ```citieskg-pirmasensEPSG32633```

If not included, you will need to add the targetResourceID in ```./cea-agent/src/main/resources/CEAAgentConfig.properties``` and add the corresponding mapping from cityObject IRI to targetResourceID in accessAgentRoutes in the ```readConfig``` method of ```./cea-agent/src/main/java/uk/ac/cam/cares/jps/agent/cea/CEAAgent.java```.

### 2.6. Build
In the same directory as this README, first build the Docker image by running
```
docker-compose build
```

After the image is built, copy ```./stack-manager-input-config/cea-agent.json``` and place it in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager.

### 2.7. Debugging
To debug, put ```./stack-manager-input-config/cea-agent-debug.json``` instead of ```./stack-manager-input-config/cea-agent.json```  in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager. The debugger port will be available at 5005.

## 3. Agent Endpoints
The CEA Agent is reachable at http://localhost:3838/cea-agent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. The agent provides three endpoints: run endpoint (http://localhost:3838/cea-agent/run), where the agent runs CEA; update endpoint (http://localhost:3838/cea-agent/update), where the agent updates the knowledge graph with triples on CEA outputs; query endpoint (http://localhost:3838/cea-agent/run), where the agent returns CEA outputs. 

### 3.1. Run Endpoint
Available at http://localhost:3838/cea-agent/run.

The run endpoint accepts the following request parameters:
- ```iris```: array of cityObject IRIs.
- ```geometryEndpoint```: (optional) endpoint where the geospatial information of the cityObjects from ```iris``` is stored; if not specified, agent will default to setting ```geometryEndpoint``` to TheWorldAvatar Blazegraph with the namespace retrieved from the cityObject IRI and the targetresourceID mappings provided in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```usageEndpoint```: (optional) endpoint where the building usage information of the cityObjects from ```iris``` is stored, if not specified, agent will default to setting ```usageEndpoint``` to be the stack Blazegraph namespace, labelled by ```usage.label``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples, i.e. energy demand and solar energy generator information, instantiated by the agent is to be stored; if not specified, agent will default to setting ```ceaEndpoint``` to the stack Blazegraph namespace, labelled by ```cea.label``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```weatherEndpoint```: (optional) endpoint where historical weather information is stored; if not specified, agent will default to setting ```weatherEndpoint``` to TheWorldAvatar Blazegraph.
- ```terrainDatabase``` (optional) database from which the agent will attempt to query terrain data; if not specified, it will be set to the stack database specified as ```postgis.database``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```terrainTable```: (optional) table from which the agent will attempt to query terrain data; if not specified, it will be set to the stack table specified as ```postgis.table``` in ```./cea-agent/src/main/resources/postgis.properties```. Please ensure that ```terrainTable``` is in public schema, since this is the set-up that the agent assumes.
- ```graphName```: (optional) named graph to which the CEA triples belong to and will be instantiated under. If ```graphName``` is not specified, the agent will assume no graph usage.

After receiving request to the run endpoint, the agent will query for the building geometry, surrounding buildings' geometry, building usage, historical weather, and terrain data from the endpoints specified in the request parameters. The agent will then run CEA with the queried information as inputs, and send request with the CEA output data to the update endpoint afterwards.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"],
"geometryEndpoint" : "http://host.docker.internal:48888/kingslynnEPSG27700",
"ceaEndpoint": "http://host.docker.internal:48888/cea"}
```
In the above request example, the CEA Agent will be querying building geometry from the Blazegraph that ```http://host.docker.internal:48888/kingslynnEPSG27700``` is pointed to. All the other inputs for CEA will be queried from the default endpoints within stack as specified in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```. The CEA triples will be instantiated, with no graph reference, in the Blazegraph where ```http://host.docker.internal:48888/cea``` is pointed to.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"],
"geometryEndpoint" : "http://host.docker.internal:48888/kingslynnEPSG27700",
"ceaEndpoint": "http://host.docker.internal:48888/cea",
"graphName": "http://127.0.0.1:9999/blazegraph/namespace/cea/cea"}
```
In the above request example, the CEA Agent will be querying building geometry from the Blazegraph that ```http://host.docker.internal:48888/kingslynnEPSG27700``` is pointed to. All the other inputs for CEA will be queried from the default endpoints within stack as specified in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```. The CEA triples will be instantiated under the ```http://127.0.0.1:9999/blazegraph/namespace/cea/cea``` graph in the Blazegraph where ```http://host.docker.internal:48888/cea``` is pointed to.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"]}
```
In the above request example, the CEA Agent will be querying building geometry from the ```citieskg-kingslynnEPSG27700``` namespace in TheWorldAvatar Blazegraph.  All the other inputs for CEA will be queried from the default endpoints within stack as specified in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```. The CEA triples will be instantiated in the stack Blazegraph.

### 3.2. Update

Available at http://localhost:3838/cea-agent/update.

Requests to the update endpoint is automatically sent by the CEA Agent after running and receiving requests to the run endpoint. The update endpoint updates the knowledge graph with CEA outputs.

### 3.3. Query

Available at http://localhost:3838/cea-agent/query.

The query endpoint accepts the following request parameters:
- ```iris```: array of cityObject IRIs.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples, i.e. energy demand and solar energy generator information, instantiated by the agent is to be stored; if not specified, agent will default to setting ```ceaEndpoint``` to the stack Blazegraph namespace, labelled by ```cea.label``` in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```graphName```: (optional) named graph to which the CEA triples belong to and will be instantiated under. If ```graphName``` is not specified, the agent will assume no graph.

After receiving request sent to the query endpoint, the agent will retrieve energy demand and solar energy generator information calculated by CEA for the cityObject IRIs provided in ```iris```. The energy demand and solar energy generator information will only be returned if the cityObject IRIs provided in ```iris``` has already been passed to the run endpoint of the CEA Agent beforehand.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"]
}
```

Example response:
```
{
    "path": "/cea-agent/query",
    "iris": [
        "http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"
    ],
    "acceptHeaders": "*/*",
    "method": "POST",
    "requestUrl": "http://localhost:58085/cea-agent/query",
    "ceaOutputs": [
        {
            "EastWallSolarSuitableArea": "106.29 m^2",
            "Annual PVTPlateWallSouth Heat Supply": "58659.89 kWh",
            "SouthWallSolarSuitableArea": "74.82 m^2",
            "Annual PVRoof Electricity Supply": "14837.76 kWh",
            "Annual PVWallNorth Electricity Supply": "2149.17 kWh",
            "Annual ThermalPlateRoof Heat Supply": "124592.48 kWh",
            "Annual PVTPlateWallNorth Electricity Supply": "1705.71 kWh",
            "Annual PVTPlateWallWest Electricity Supply": "9670.28 kWh",
            "Annual HeatingConsumption": "55132.02 kWh",
            "Annual PVTTubeRoof Electricity Supply": "31488.02 kWh",
            "Annual ThermalTubeWallWest Heat Supply": "2419.43 kWh",
            "Annual ElectricityConsumption": "5897.54 kWh",
            "Annual PVTTubeWallEast Heat Supply": "22086.78 kWh",
            "Annual ThermalPlateWallEast Heat Supply": "699.96 kWh",
            "Annual PVWallSouth Electricity Supply": "5414.0 kWh",
            "Annual PVTTubeWallSouth Electricity Supply": "11508.16 kWh",
            "Annual PVTTubeWallEast Electricity Supply": "12317.57 kWh",
            "Annual PVTTubeWallNorth Heat Supply": "9826.32 kWh",
            "Annual ThermalTubeRoof Heat Supply": "53682.57 kWh",
            "Annual PVTPlateWallWest Heat Supply": "7347.86 kWh",
            "Annual PVTPlateWallSouth Electricity Supply": "12017.34 kWh",
            "Annual PVWallWest Electricity Supply": "2673.61 kWh",
            "Annual PVTTubeWallSouth Heat Supply": "49508.46 kWh",
            "Annual PVTPlateWallNorth Heat Supply": "6394.9 kWh",
            "Annual ThermalTubeWallSouth Heat Supply": "41769.6 kWh",
            "Annual GridConsumption": "5897.54 kWh",
            "WestWallSolarSuitableArea": "106.29 m^2",
            "Annual PVTTubeWallNorth Electricity Supply": "1696.94 kWh",
            "Annual PVTTubeRoof Heat Supply": "71644.21 kWh",
            "Annual PVTPlateWallEast Electricity Supply": "12464.7 kWh",
            "Annual ThermalPlateWallWest Heat Supply": "4080.53 kWh",
            "Annual PVTTubeWallWest Heat Supply": "4561.7 kWh",
            "Annual ThermalPlateWallSouth Heat Supply": "50605.26 kWh",
            "NorthWallSolarSuitableArea": "74.82 m^2",
            "Annual PVWallEast Electricity Supply": "8051.66 kWh",
            "Annual ThermalPlateWallNorth Heat Supply": "2765.64 kWh",
            "Annual PVTTubeWallWest Electricity Supply": "3442.28 kWh",
            "Annual ThermalTubeWallEast Heat Supply": "17521.69 kWh",
            "Annual PVTPlateRoof Electricity Supply": "36713.79 kWh",
            "Annual PVTPlateRoof Heat Supply": "152928.79 kWh",
            "Annual ThermalTubeWallNorth Heat Supply": "5332.34 kWh",
            "Annual PVTPlateWallEast Heat Supply": "4701.85 kWh",
            "RoofSolarSuitableArea": "98.21 m^2"
        }
    ],
    "body": "{\"iris\": \r\n[\"http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/\"]\r\n}",
    "contentType": "application/json"
}
```

## 4. Information Retrieved from Knowledge Graph
The agent will attempt to retrieve information on building geometry, surrounding buildings geometries, building usage and historical weather from knowledge graph to pass as input to CEA. The retrieval is done with SPARQL queries. The agent assumes that the IRIs in the knowledge graph queried to follow a consistent format. The assumption is that the IRIs inside the knowledge graph follow the format of `<{PREFIX}cityobject/{UUID}/>` where PREFIX is the prefix to IRIs in the namespace that the agent is working with.

For example:
- `http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_b5a7b032-1877-4c2b-9e00-714b33a426f7/` - the PREFIX is: `http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/`

- `http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_7cb00a09-528b-4016-b3d6-80c5a9442d95/` - the PREFIX is `http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/`.

### 4.1. Coordinate Reference System (CRS)
The agent will generate shapefile for the building and its surrounding buildings, which are passed to CEA as input. In order to generate the correct shapefile, the agent will be querying for CRS information with the following query:

```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?CRS
WHERE
  { GRAPH <{PREFIX}databasesrs/>
      { <{PREFIX}> ocgml:srid  ?CRS}}
```
If no CRS is stored in the namespace please insert the data in the databasesrs graph. For example:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

INSERT DATA
{
GRAPH <{PREFIX}databasesrs/> {
<{PREFIX}>	ocgml:srid	27700 .
<{PREFIX}>	ocgml:srsname "EPSG:27700" .
}}
```
### 4.2. Building Footprint
The agent will query for the footprint geometry of the building. The agent will first try querying for the thematic surface with surface id 35, which corresponds to a ground surface:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?Footprint
WHERE
  { GRAPH <{PREFIX}surfacegeometry/>
      { ?surf  ocgml:cityObjectId  ?id ;
               ocgml:GeometryType  ?Footprint
        FILTER ( ! isBlank(?Footprint) )
   GRAPH <{PREFIX}thematicsurface/> 
       { ?id ocgml:buildingId <{PREFIX}building/{UUID}/>;
    		 ocgml:objectClassId ?groundSurfId.
   FILTER(?groundSurfId = 35) 
   }}}
```
If unsuccessful, the agent will query all building surface geometries with the following query, where the ground surface geometries are selected by searching for the surface with the minimum constant height:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?Footprint
WHERE
  { GRAPH <{PREFIX}surfacegeometry/>
      { ?surf  ocgml:cityObjectId  <{PREFIX}building/{UUID}/> ;
               ocgml:GeometryType  ?Footprint
        FILTER ( ! isBlank(?Footprint) )
      }}
```

After getting the ground surface geometries of the building, the agent will merge the ground surface geometries to extract the footprint geometry.

### 4.3. Building Height
For building height, there are three different possible queries that can retrieve the building height. Each of the following queries are tried, in the order they are listed, until a result is retrieved. If all the queries return empty results, a default value of 10.0m for building height is set.

First query to for building height:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?Height
WHERE
  { GRAPH <{PREFIX}building/>
      { <{PREFIX}building/{UUID}/> ocgml:measuredHeigh  ?Height}}
```

Second query to try for building height:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?Height
WHERE
  { GRAPH <{PREFIX}building/>
      { <{PREFIX}building/{UUID}/> ocgml:measuredHeight  ?Height}}
```

Third query to try for building height:
```
PREFIX  ocgml: <http://www.theworldavatar.com/ontology/ontocitygml/citieskg/OntoCityGML.owl#>

SELECT  ?Height
WHERE
  { GRAPH <{PREFIX}cityobjectgenericattrib/>
      { ?o  ocgml:attrName      "height" ;
            ocgml:realVal       ?Height ;
            ocgml:cityObjectId  <{PREFIX}cityobject/{UUID}/>}}

```

### 4.4. Building Usage
The agent queries for the building usage, and the usage share if the building is a multi-usage building, with the following query:
```
PREFIX ontobuiltenv: <https://www.theworldavatar.com/kg/ontobuiltenv/>

SELECT  ?BuildingUsage ?UsageShare
WHERE
  { ?building  ontobuiltenv:hasOntoCityGMLRepresentation  <{PREFIX}building/{UUID}/> ;
              ontobuiltenv:hasPropertyUsage  ?usage .
    ?usage a ?BuildingUsage
    OPTIONAL
      { ?usage ontobuiltenv:hasUsageShare ?UsageShare}
  }
ORDER BY DESC(?UsageShare)
```

If no building usage is returned from the query, the default value of ```MULTI_RES``` building usage is set, consistent with the default building usage type used by the CEA. In the case of multiple usages for one building, the OntoBuiltEnv usage concepts are first mapped to the CEA defined usage types according to the mapping at the bottom section of this README; then, since CEA only allows up to three usage types per building, the top three usages and their usage share are passed to CEA as input.

#### 4.4.1. Building Usage Mapping
The agent will attempt to query for the building usage type, which are stored with ```OntoBuiltEnv``` concepts, to pass to CEA as input.

In the CEA, there are 19 defined building usage types. In the ```OntoBuiltEnv``` ontology, there are 23 classes for building usage type (see left 2 columns of table below). After querying for the ```OntoBuiltEnv``` concepts, the agent will map the concepts to the CEA defined usage types as shown in the right 2 columns of the following mapping table:

| CEA available usage types | ```OntoBuiltEnv``` concepts |   | ```OntoBuiltEnv``` concepts | Mapped to CEA usage type |
|:-------------------------:|:---------------------------:|:-:|:---------------------------:|:------------------------:|
|         COOLROOM          |          Domestic           |   |          Domestic           |        MULTI_RES         |
|         FOODSTORE         |      SingleResidential      |   |      SingleResidential      |        SINGLE_RES        |
|            GYM            |      MultiResidential       |   |      MultiResidential       |        MULTI_RES         |
|         HOSPITAL          |      EmergencyService       |   |      EmergencyService       |         HOSPITAL         |
|           HOTEL           |         FireStation         |   |         FireStation         |         HOSPITAL         |
|        INDUSTRIAL         |        PoliceStation        |   |        PoliceStation        |         HOSPITAL         |
|            LAB            |         MedicalCare         |   |         MedicalCare         |         HOSPITAL         |
|          LIBRARY          |          Hospital           |   |          Hospital           |         HOSPITAL         |
|         MULTI_RES         |           Clinic            |   |           Clinic            |         HOSPITAL         |
|          MUSEUM           |          Education          |   |          Education          |        UNIVERSITY        |
|          OFFICE           |           School            |   |           School            |          SCHOOL          |
|          PARKING          |     UniversityFacility      |   |     UniversityFacility      |        UNIVERSITY        |
|        RESTAURANT         |           Office            |   |           Office            |          OFFICE          |
|          RETAIL           |     RetailEstablishment     |   |     RetailEstablishment     |          RETAIL          |
|          SCHOOL           |      ReligiousFacility      |   |      ReligiousFacility      |          MUSEUM          |
|        SERVERROOM         |     IndustrialFacility      |   |     IndustrialFacility      |        INDUSTRIAL        |
|        SINGLE_RES         |     EatingEstablishment     |   |     EatingEstablishment     |        RESTAURANT        |
|         SWIMMING          |    DrinkingEstablishment    |   |    DrinkingEstablishment    |        RESTAURANT        |
|        UNIVERSITY         |            Hotel            |   |            Hotel            |          HOTEL           |
|                           |       SportsFacility        |   |       SportsFacility        |           GYM            |
|                           |      CulturalFacility       |   |      CulturalFacility       |          MUSEUM          |
|                           |      TransportFacility      |   |      TransportFacility      |        INDUSTRIAL        |
|                           |        Non-Domestic         |   |        Non-Domestic         |        INDUSTRIAL        |

### 4.5. Historical Weather Data
The agent will attempt to retrieve historical weather data for the location of the building in the request received, to use as input to CEA. Then, the agent will create a EPW file based on the retrieved historical data, which will be passed to CEA as the weather input. The historical weather data need to be at least 1 year duration in hourly format for CEA to run successfully. In the event where the retrieved historical weather data do not satisfy the aforementioned requirement by CEA or where the agent fails to retrieve historical weather data, the agent will run CEA with the default EPW file defined by CEA's own database as the weather input.

WARNING: Please note that the CEA Agent assumes that the historical weather data is stored in a PostgreSQL database that uses the same username and password as the stack PostgreSQL.

## 5. Information Retrieved from PostGIS
### 5.1. Terrain Data
The agent will attempt to retrieve terrain data from the stack PostGIS. The queried area for the terrain data will have the building received in the request at the center. The queried area will cover the center building and its surrounding buildings that were retrieved in the surroundings query.