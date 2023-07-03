# CEA Agent
## Agent Description

The CEA Agent can be used to interact with the [City Energy Analyst (CEA)](https://www.cityenergyanalyst.com/)  and the data it produces on building energy demands and installable solar energy generators.

The agent currently queries for building geometry, surrounding buildings geometries, building usage and historical weather data stored in knowledge graph, and terrain data from PostGIS, which are passed to CEA as inputs. The energy demands and potential energy of solar energy generators (which include photovoltaic panels, photovoltaic-thermal collectors, and solar collectors) calculated by CEA are extracted by the agent and stored on the knowledge graph.

## Build Instructions

### Maven

The docker image uses TheWorldAvatar maven repository (https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/).
You'll need to provide your credentials (github username/personal access token) in single-word text files located:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

### PostgreSQL

The agent also requires a postgreSQL database for the time series client to save data in. The address of the database used need to be provided as ```db.url``` in:
```
./cea-agent/src/main/resources/
    timeseriesclient.properties
```

The username and password for the PostgreSQL database need to be provided in single-word text files in:
```
./credentials/
    postgres_username.txt
    postgres_password.txt
```

### PostGIS

The agent will attempt to query for terrain data from a PostGIS database, the address of which needs to be provided as ```db.url``` in 
```
./cea-agent/src/main/resources/
    postgis.properties
```
Inside the same ```postgis.properties``` file, the default table from which the agent will be querying from needs to be provided as ```db.table```. Please note that the table will have to be in the public schema, since the agent is set up to query the provided table from public schema.

The username and password for the PostGIS database need to be provided in single-word text files in:
```
./credentials/
    postgis_username.txt
    postgis_password.txt
```

### Access Agent

The CEA Agent also uses the [access agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT).
The targetResourceIDs in ```./cea-agent/src/main/resources/CEAAgentConfig.properties``` provides mappings to namespaces in TheWorldAvatar Blazegraph (http://www.theworldavatar.com:83/citieskg/). The targetResourceID are passed to the access agent in order to query from TheWorldAvatar Blazegraph. Check that a targetResourceID to pass to the access agent exists for the namespace being used for SPARQL queries.
Currently included are:

- ```citieskg-berlin```
- ```singaporeEPSG24500```
- ```citieskg-singaporeEPSG4326```
- ```citieskg-kingslynnEPSG3857```
- ```citieskg-kingslynnEPSG27700```
- ```citieskg-pirmasensEPSG32633```

If not included, you will need to add the targetResourceID in ```./cea-agent/src/main/resources/CEAAgentConfig.properties``` and add the corresponding mapping from cityObject IRI to targetResourceID in accessAgentRoutes in the ```readConfig``` method of ```./cea-agent/src/main/java/uk/ac/cam/cares/twa/cities/ceaagent/CEAAgent.java```.

### For Developers
In order to use a local Blazegraph, you will need to run the access agent locally and set the accessagent.properties storerouter endpoint url to your local Blazegraph, as well as add triples for your namespace to a local ontokgrouter as is explained [here](https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#install-and-build-local-accessagent-for-developers). In order fo the time series client to use the local PostgreSQL and the local Blazegraph, in ```/cea-agent/src/main/resources/timeseriesclient.properties```, change ```db.url``` to the local PostgreSQL database.

### Running

To build and start the agent, you simply need to spin up a container.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run
```
docker-compose up -d
```


## Agent Endpoints
The CEA Agent provides three endpoints: run endpoint (http://localhost:58085/agents/cea/run), where the agent runs CEA; update endpoint (http://localhost:58085/agents/cea/update), where the agent updates the knowledge graph with triples on CEA outputs; query endpoint (http://localhost:58085/agents/cea/query), where the agent returns CEA outputs. 

### 1. Run Endpoint
Available at http://localhost:58085/agents/cea/run.

The run endpoint accepts the following request parameters:
- ```iris```: array of cityObject IRIs.
- ```geometryEndpoint```: (optional) endpoint where the geospatial information of the cityObjects from ```iris``` is stored; if not specified, agent will default to setting ```geometryEndpoint``` to TheWorldAvatar Blazegraph with the namespace retrieved from the cityObject IRI and the mapping provided in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```usageEndpoint```: (optional) endpoint where the building usage information of the cityObjects from ```iris``` is stored, if not specified, agent will default to setting ```usageEndpoint``` to be the same as ```geometryEndpoint```.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples, i.e. energy demand and solar energy generator information, instantiated by the agent is to be stored; if not specified, agent will default to setting ```ceaEndpoint``` to TheWorldAvatar Blazegraph with the namespace retrieved from the cityObject IRI and the mapping provided in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```weatherEndpoint```: (optional) endpoint where historical weather information is stored; if not specified, agent will default to setting ```weatherEndpoint``` to TheWorldAvatar Blazegraph.
- ```terrainTable```: (optional) table from which the agent will attempt to query terrain data; if not specified, it will be set to the default table specified as ```db.table``` in ```./cea-agent/src/main/resources/postgis.properties```. Please ensure that ```terrainTable``` is in public schema, since this is the set-up that the agent assumes.
- ```graphName```: (optional) named graph to which the CEA triples belong to. In the scenario where ```ceaEndpoint``` is not specified, if ```graphName``` is not specified, the default graph is ```http://www.theworldavatar.com:83/citieskg/namespace/{namespace}/sparql/energyprofile/```, where {namespace} is a placeholder for the namespace of the cityObject IRI, e.g. kingslynnEPSG27700. If ```ceaEndpoint``` is specified, the agent will assume no graph usage if ```namedGraph``` is not specified.

After receiving request to the run endpoint, the agent will query for the building geometry, surrounding buildings' geometry and building usage from the endpoints specified in the request parameters. The agent will then run CEA with the queried information as inputs, and send request with the CEA output data to the update endpoint afterwards.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"],
"geometryEndpoint" : "http://host.docker.internal:48888/kingslynnEPSG27700",
"ceaEndpoint": "http://host.docker.internal:48888/cea"}
```
In the above request example, the CEA Agent will be querying geometry and usage from the Blazegraph that ```http://host.docker.internal:48888/kingslynnEPSG27700``` is pointed to. The CEA triples will be instantiated, with no graph reference, in the Blazegraph where ```http://host.docker.internal:48888/cea``` is pointed to.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"],
"geometryEndpoint" : "http://host.docker.internal:48888/kingslynnEPSG27700",
"ceaEndpoint": "http://host.docker.internal:48888/cea",
"graphName": "http://127.0.0.1:9999/blazegraph/namespace/cea/cea"}
```
In the above request example, the CEA Agent will be querying geometry and usage from the Blazegraph that ```http://host.docker.internal:48888/kingslynnEPSG27700``` is pointed to. The CEA triples will be instantiated under the ```http://127.0.0.1:9999/blazegraph/namespace/cea/cea``` graph in the Blazegraph where ```http://host.docker.internal:48888/cea``` is pointed to.

Example request:
```
{ "iris" :
["http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"],
```
In the above request example, the CEA Agent will be querying geometry and usage, as well as instantiating CEA triples, from the ```citieskg-kingslynnEPSG27700``` namespace in TheWorldAvatar Blazegraph. And the CEA triples will be instantiated under the ```http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/energyprofile/``` graph.

### 2. Update

Available at http://localhost:58085/agents/cea/update.

Requests to the update endpoint is automatically sent by the CEA Agent after running and receiving requests to the run endpoint. The update endpoint updates the knowledge graph with CEA outputs.

### 3. Query

Available at http://localhost:58085/agents/cea/query.

The query endpoint accepts the following request parameters:
- ```iris```: array of cityObject IRIs.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples instantiated by the agent is stored; if not specified, agent will default to setting ```ceaEndpoint``` to TheWorldAvatar Blazegraph with the namespace retrieved from the cityObject IRI and the mapping provided in ```./cea-agent/src/main/resources/CEAAgentConfig.properties```.
- ```graphName```: (optional) named graph to which the CEA triples belong to. In the scenario where ```ceaEndpoint``` is not specified, if ```graphName``` is not specified, the default graph is ```http://www.theworldavatar.com:83/citieskg/namespace/{namespace}/sparql/energyprofile/```, where {namespace} is a placeholder for the namespace of the cityObject IRI, e.g. kingslynnEPSG27700. If ```ceaEndpoint``` is specified, the agent will assume no graph usage if ```namedGraph``` is not specified.

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
    "path": "/agents/cea/query",
    "iris": [
        "http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/"
    ],
    "acceptHeaders": "*/*",
    "method": "POST",
    "requestUrl": "http://localhost:58085/agents/cea/query",
    "ceaOutputs": [
        {
            "ThermalTubeWallEastArea": "106.29 m^2",
            "Annual PVTPlateWallSouth Heat Supply": "616.76 kWh",
            "PVTTubeWallSouthArea": "74.82 m^2",
            "Annual PVRoof Electricity Supply": "5750.72 kWh",
            "PVWallNorthArea": "74.82 m^2",
            "Annual PVTPlateWallNorth Electricity Supply": "1380.62 kWh",
            "Annual HeatingConsumption": "58000.21 kWh",
            "ThermalPlateWallNorthArea": "74.82 m^2",
            "Annual PVTTubeRoof Electricity Supply": "5656.85 kWh",
            "PVTPlateWallWestArea": "106.29 m^2",
            "Annual ElectricityConsumption": "5901.74 kWh",
            "Annual PVWallSouth Electricity Supply": "2741.93 kWh",
            "Annual PVTTubeWallNorth Heat Supply": "7827.28 kWh",
            "ThermalPlateWallEastArea": "106.29 m^2",
            "ThermalTubeWallNorthArea": "74.82 m^2",
            "PVTPlateWallSouthArea": "74.82 m^2",
            "PVWallWestArea": "106.29 m^2",
            "Annual PVTTubeWallSouth Heat Supply": "13007.03 kWh",
            "Annual PVTPlateWallNorth Heat Supply": "2255.04 kWh",
            "Annual PVTTubeRoof Heat Supply": "20041.26 kWh",
            "PVRoofArea": "45.39 m^2",
            "PVTTubeWallEastArea": "106.29 m^2",
            "PVTTubeWallWestArea": "106.29 m^2",
            "Annual ThermalPlateWallWest Heat Supply": "2281.06 kWh",
            "Annual PVTTubeWallWest Heat Supply": "8263.92 kWh",
            "ThermalPlateWallSouthArea": "74.82 m^2",
            "Annual ThermalPlateWallNorth Heat Supply": "387.05 kWh",
            "PVTPlateRoofArea": "45.39 m^2",
            "ThermalTubeWallWestArea": "106.29 m^2",
            "Annual ThermalTubeWallEast Heat Supply": "6030.27 kWh",
            "Annual PVTPlateRoof Heat Supply": "18171.69 kWh",
            "Annual ThermalTubeWallNorth Heat Supply": "4125.84 kWh",
            "ThermalPlateWallWestArea": "106.29 m^2",
            "Annual PVTPlateWallEast Heat Supply": "824.91 kWh",
            "ThermalPlateRoofArea": "45.39 m^2",
            "PVTTubeWallNorthArea": "74.82 m^2",
            "PVTTubeRoofArea": "45.39 m^2",
            "Annual PVWallNorth Electricity Supply": "1602.03 kWh",
            "Annual ThermalPlateRoof Heat Supply": "9290.34 kWh",
            "Annual PVTPlateWallWest Electricity Supply": "2532.95 kWh",
            "ThermalTubeRoofArea": "45.39 m^2",
            "Annual ThermalTubeWallWest Heat Supply": "7760.16 kWh",
            "Annual PVTTubeWallEast Heat Supply": "14180.33 kWh",
            "PVWallEastArea": "106.29 m^2",
            "Annual PVTTubeWallSouth Electricity Supply": "7125.34 kWh",
            "Annual PVTTubeWallEast Electricity Supply": "12644.96 kWh",
            "Annual ThermalTubeRoof Heat Supply": "13408.38 kWh",
            "PVWallSouthArea": "74.82 m^2",
            "Annual PVTPlateWallWest Heat Supply": "6890.0 kWh",
            "Annual PVTPlateWallSouth Electricity Supply": "7195.41 kWh",
            "PVTPlateWallNorthArea": "74.82 m^2",
            "ThermalTubeWallSouthArea": "74.82 m^2",
            "Annual PVWallWest Electricity Supply": "2451.2 kWh",
            "Annual ThermalTubeWallSouth Heat Supply": "7222.0 kWh",
            "Annual GridConsumption": "5901.74 kWh",
            "Annual PVTTubeWallNorth Electricity Supply": "1360.94 kWh",
            "Annual PVTPlateWallEast Electricity Supply": "12385.81 kWh",
            "PVTPlateWallEastArea": "106.29 m^2",
            "Annual PVWallEast Electricity Supply": "4647.88 kWh",
            "Annual PVTTubeWallWest Electricity Supply": "2505.21 kWh",
            "Annual PVTPlateRoof Electricity Supply": "5696.97 kWh"
        }
    ],
    "body": "{\"iris\": \r\n[\"http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_0595923a-3a83-4097-b39b-518fd23184cc/\"]\r\n}",
    "contentType": "application/json"
}
```

## Information Retrieved from Knowledge Graph
The agent will attempt to retrieve information on building geometry, surrounding buildings geometries, building usage and historical weather from knowledge graph to pass as input to CEA. The retrieval is done with SPARQL queries. The agent assumes that the IRIs in the knowledge graph queried to follow a consistent format. The assumption is that the IRIs inside the knowledge graph follow the format of `<{PREFIX}cityobject/{UUID}/>` where PREFIX is the prefix to IRIs in the namespace that the agent is working with.

For example:
- `http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/cityobject/UUID_b5a7b032-1877-4c2b-9e00-714b33a426f7/` - the PREFIX is: `http://127.0.0.1:9999/blazegraph/namespace/kings-lynn-open-data/sparql/`

- `http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/cityobject/UUID_7cb00a09-528b-4016-b3d6-80c5a9442d95/` - the PREFIX is `http://www.theworldavatar.com:83/citieskg/namespace/kingslynnEPSG27700/sparql/`.

### Coordinate Reference System (CRS)
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
### Building Footprint
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
   }}
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

### Building Height
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

### Building Usage
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


### Visualisation
The 3dWebMapClient can be set up to visualise data produced by the CEA Agent (instructions to run are [here](https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#3dcitydb-web-map-client)). The City Information Agent (CIA) is used when a building on the 3dWebMapClient is selected, to query data stored in the KG on the building. If the parameter "context=energy" is included in the url, the query endpoint of CEA will be contacted for energy data. eg `http://localhost:8000/3dwebclient/index.html?city=kingslynn&context=energy` (NB. this currently requires running web map client and CitiesKG/agents from [develop branch](https://github.com/cambridge-cares/CitiesKG/tree/develop/agents))

### Historical Weather Data
The agent will attempt to retrieve historical weather data for the location of the building in the request received, to use as input to CEA. Then, the agent will create a EPW file based on the retrieved historical data, which will be passed to CEA as the weather input. The historical weather data need to be at least 1 year duration in hourly format for CEA to run successfully. In the event where the retrieved historical weather data do not satisfy the aforementioned requirement by CEA or where the agent fails to retrieve historical weather data, the agent will run CEA with the default EPW file defined by CEA's own database as the weather input. 

WARNING: Please note that the CEA Agent assumes that the historical weather data is stored in a Postgres database that uses the username and password provided in
```
./credentials/
    postgres_username.txt
    postgres_password.txt
```

### Building Usage Mapping
The agent queries for the building usage type, which are stored with ```OntoBuiltEnv``` concepts, to pass to CEA as input.

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

