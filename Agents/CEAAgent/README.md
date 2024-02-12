# CEA Agent
## 1. Agent Description

CEA agent can be used to interact with the [City Energy Analyst (CEA)](https://www.cityenergyanalyst.com/)  and the data it produces on building energy demands and installable solar energy generators.

The agent currently attempts to query for building geometry, surrounding buildings geometries, building usage and historical weather data stored in knowledge graph, and terrain data from PostGIS, which are passed to CEA as inputs. 
With the exception of building geometry data, which is a necessary input, all the other inputs are optional and the agent will run CEA with its corresponding default CEA values for un-retrievable inputs.
The energy demands and potential energy of solar energy generators (which include photovoltaic panels, photovoltaic-thermal collectors, and solar collectors) calculated by CEA are extracted by the agent and stored on the knowledge graph.

## 2. Build Instructions
Everything in this section is mandatory except when indicated otherwise.

### 2.1. Maven
The docker image uses TheWorldAvatar maven repository (https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files located:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
### 2.2. Stack Set Up
The agent is designed to run in the stack. To start the stack, spin up [stack-manager].

### 2.3. Blazegraph Set Up
The agent is designed to use the stack Blazegraph. Please ensure that the Blazegraph namespace corresponding to ```cea.label``` in [CEAAgentConfig.properties], is set up in the stack Blazegraph.

### 2.4. Access Agent Set Up
The agent is designed to use the [Access Agent](../AccessAgent). Spin the access agent up as part of the same stack as spun up by the Stack Manager. Please ensure that the routing information for the Blazegraph namespace corresponding to ```cea.label``` in [CEAAgentConfig.properties] is uploaded, see the [Access Agent README](../AccessAgent) for instruction on uploading routing information.

### 2.5. PostGIS Set Up
The agent is designed to use the stack PostGIS. Time series outputs on energy demands and photovoltaic potentials calculated by CEA will be stored in the stack PostGIS database, which is specified as ```cea.database``` in [CEAAgentConfig.properties]. Please ensure the database specified by ```cea.database``` is set up in the stack PostGIS.

### 2.6. Ontop Endpoint for Building Geometry Query
For building geometry, the CEA agent defaults to the Ontop endpoint within the same stack. 
Please make sure the building geometry data and its corresponding OBDA are uploaded to the same stack as this agent via [stack-data-uploader].

### 2.7. Ontop Endpoint for Building Usage Query (Optional)
The CEA agent will attempt to query for building usage information from the Ontop endpoint within the same stack.
Please make sure that the [OSMAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OSMAgent) is spun up in the same stack and ran, to get the building usage information of which this agent will query for.

### 2.8. Terrain Data in PostGIS (Optional)
The CEA agent will attempt to query for terrain data. Upload the terrain data as raster files via [stack-data-uploader].
Please ensure that `terrain.database` and `terrain.table` in [CEAAgentConfig.properties] match the database and table where the terrain data is uploaded to.

### 2.9. Agent for Historical Weather Data (Optional)
The CEA agent will attempt to send request to [OpenMeteoAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OpenMeteoAgent) for instantiation of historical weather data, which the CEA agent will attempt to query for to use as input to CEA.
Please ensure that the OpenMeteoAgent is spun up in the same stack as this agent, by following the [OpenMeteoAgent README](../OpenMeteoAgent/README.md). 
Please ensure that `weather.label` in [CEAAgentConfig.properties] is the same value as `route.label` in OpenMeteoAgent's [config.properties](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/OpenMeteoAgent/openmeteo-agent/src/main/resources/config.properties).

### 2.10. Build
To build the Docker image, in the same directory as this README, run
```
docker compose build
```

The latest version of the image should be pushed to the GitHub container registry, which can be done by running
```
docker push ghcr.io/cambridge-cares/cea-agent:X.Y.Z
```
where X.Y.Z is the new version number.

### 2.11. Run
Copy [cea-agent.json] from ```./stack-manager-input/``` and place it in [stack-manager config services] directory. 
Then, in the [stack-manager] directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager.

### 2.12. Debugging
To debug, put [cea-agent-debug.json] from ```./stack-manager-input/``` instead of [cea-agent.json]  in [stack-manager config services].
Please ensure that `Source` in `Mounts` of [cea-agent-debug.json] points correctly to the local `CEAAgentConfig.properties`.Then, in the [stack-manager]  directory, run 
```
./stack.sh start <STACK NAME>
```
The debugger port will be available at 5005.

### 2.13. Visualisation
Visualisation of CEA results in [TWA Visualisation Framework] can be achieved with [FeatureInfoAgent].
Spin up [TWA Visualisation Framework] by following its README, for the base visualisation of the buildings. 
Copy the `.sparql` and `.json` files in [feature-info-agent-input](./feature-info-agent-input), and place them inside [stack-manager]'s `/input/data/fia-queries` directory, according to the path specified in the stack-manager config file, [feature-info-agent.json](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/Agents/FeatureInfoAgent/sample/feature-info-agent.json).
Then, spin up FeatureInfoAgent inside the stack.
Once the visualisation is up, after clicking on a building, one should see the following CEA results: solar suitable area, annual building energy demands and annual photovoltaic potentials as scalar values, and building demands and photovoltaic potentials time series of 24 hours duration.


## 3. Agent Endpoints
The CEA Agent is reachable at http://localhost:3838/cea-agent, where 3838 is the default port number used by stack manager.
If another port number was specified when spinning up the stack, please replace 3838 with the specified port number.
The agent provides three endpoints: run endpoint (http://localhost:3838/cea-agent/run), where the agent runs CEA; update endpoint (http://localhost:3838/cea-agent/update), where the agent updates the knowledge graph with triples on CEA outputs; query endpoint (http://localhost:3838/cea-agent/run), where the agent returns CEA outputs. 

### 3.1. Run Endpoint
Available at http://localhost:3838/cea-agent/run.

The run endpoint accepts the following request parameters:
- ```iris```: array of building IRIs.
- ```geometryEndpoint```: (optional) endpoint where the geospatial information of the buildings from ```iris``` is stored; if not specified, agent will default to querying building geometry from Ontop endpoint within the same stack.
- ```usageEndpoint```: (optional) endpoint where the building usage information of the buildings from ```iris``` is stored, if not specified, agent will default to querying building usage from Ontop endpoint within the same stack.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples, i.e. energy demand and photovoltaic potential information, instantiated by the agent is to be stored; if not specified, agent will default to setting ```ceaEndpoint``` to the stack Blazegraph namespace, labelled by ```cea.label``` in [CEAAgentConfig.properties].
- ```weatherEndpoint```: (optional) endpoint where historical weather information is stored; if not specified, agent will default to setting ```weatherEndpoint``` agent will default to setting ```ceaEndpoint``` to the stack Blazegraph namespace, labelled by ```cea.label``` in [CEAAgentConfig.properties].
- ```terrainDatabase``` (optional) database from which the agent will attempt to query terrain data; if not specified, it will be set to the stack database specified as ```postgis.database``` in [CEAAgentConfig.properties].
- ```terrainTable```: (optional) table, inclusive of schema prefix, from which the agent will attempt to query terrain data; if not specified, it will be set to the stack table specified as ```postgis.table``` in ```./cea-agent/src/main/resources/postgis.properties```.

After receiving request to the run endpoint, the agent will query for the following CEA inputs from the endpoints specified in the request parameters: building geometry, surrounding buildings' geometry, building usage, historical weather data, and terrain data. The agent will then run CEA with the queried information as inputs, and send request with the CEA output data to the update endpoint afterwards. With the exception of building geometry query, whose results are necessary for the agent to run, if any of the CEA inputs queries fails, the agent will run CEA with its corresponding CEA default values for the missing inputs.
Provide only the `iris` parameter in the request if agent is to query CEA inputs from the same stack as this agent with the default endpoints provided in [CEAAgentConfig.properties].

Example request:
```
curl -X POST 'http://localhost:3838/cea-agent/run' -H "Content-Type: application/json" -d '{"iris": ["https://www.theworldavatar.com/kg/Building/0004c6a2-f58a-48a1-8488-09318dfac5da"], "geometryEndpoint" : "http://localhost:3838/access-agent/kingslynnEPSG27700", "ceaEndpoint": "http://localhost:3838/access-agent/outputs"}}'
```
In the above request example, the CEA Agent will be querying building geometry from the Blazegraph that ```http://localhost:3838/access-agent/kingslynnEPSG27700``` is pointed to. All the other inputs for CEA will be queried from the default endpoints within stack as specified in [CEAAgentConfig.properties]. The CEA triples will be instantiated in the Blazegraph where ```http://localhost:3838/access-agent/outputs``` is pointed to.

Example request:
```
curl -X POST 'http://localhost:3838/cea-agent/run' -H "Content-Type: application/json" -d '{"iris": ["https://www.theworldavatar.com/kg/Building/0004c6a2-f58a-48a1-8488-09318dfac5da"]}'
```
In the above request example, the CEA agent will be all the CEA inputs from the default endpoints within stack as specified in [CEAAgentConfig.properties]. The CEA triples will be instantiated in the stack Blazegraph.

### 3.2. Update
Available at http://localhost:3838/cea-agent/update.

Requests to the update endpoint is automatically sent by the CEA agent after running and receiving requests to the run endpoint. The update endpoint updates the knowledge graph with CEA outputs.
Users should not be sending requests to the update endpoint.

### 3.3. Query
Available at http://localhost:3838/cea-agent/query.

The query endpoint accepts the following request parameters:
- ```iris```: array of building IRIs.
- ```ceaEndpoint```: (optional) endpoint where the CEA triples, i.e. energy demand and photovoltaic potential information, instantiated by the agent is to be stored; if not specified, agent will default to setting ```ceaEndpoint``` to the stack Blazegraph namespace, labelled by ```cea.label``` in [CEAAgentConfig.properties].

After receiving request sent to the query endpoint, the agent will retrieve energy demand and photovoltaic potential information calculated by CEA for the building IRIs provided in ```iris```. The energy demand and photovoltaic potential information will only be returned if the building IRIs provided in ```iris``` has already been passed to the run endpoint of the CEA Agent beforehand.

Example request:
```
curl -X POST 'http://localhost:3838/cea-agent/query' -H "Content-Type: application/json" -d '{"iris": ["https://www.theworldavatar.com/kg/Building/0004c6a2-f58a-48a1-8488-09318dfac5da"]}'
```

Example response:
```
{
    "path": "/cea-agent/query",
    "iris": [
        "https://www.theworldavatar.com/kg/Building/0004c6a2-f58a-48a1-8488-09318dfac5da"
    ],
    "acceptHeaders": "*/*",
    "method": "POST",
    "requestUrl": "http://localhost:3838/cea-agent/query",
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

### 4.1. Building Footprint and Coordinate Reference System (CRS)
The agent will attempt to retrieve the building footprint and its CRS used, to generate shapefile to pass to CEA as input, with the following query: 
```
PREFIX geo: <http://www.opengis.net/ont/geosparql#>
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
PREFIX grp:	 <http://www.opengis.net/citygml/cityobjectgroup/2.0/>
PREFIX geof: <http://www.opengis.net/def/function/geosparql/>

SELECT ?wkt (geof:getSRID(?wkt) AS ?crs)
WHERE
{
  <buildingIRI> bldg:lod0FootPrint ?fp .
  ?g grp:parent ?fp;
    geo:asWKT ?wkt .
}
```

If the above query fails, the request will fail, since footprint geometry is necessary for CEA to run.

### 4.2. Building Height
The agent will attempt to retrieve the building height with the following query:

```
PREFIX bldg: <http://www.opengis.net/citygml/building/2.0/>
SELECT ?height
WHERE
{
  <buildingIRI> bldg:measuredHeight ?height .
}
```

If the height query fails, the agent will set the building height to 10.0m.

### 4.3. Building Usage
The agent will attempt to retrieve the building usage, and the usage share if the building is a multi-usage building, with the following query:
```
PREFIX ontobuiltenv: <https://www.theworldavatar.com/kg/ontobuiltenv/>

SELECT  ?BuildingUsage ?UsageShare
WHERE
{ 
    <buildingIRI> ontobuiltenv:hasPropertyUsage  ?usage .
    ?usage a ?BuildingUsage
    OPTIONAL
      {?usage ontobuiltenv:hasUsageShare ?UsageShare}
}
ORDER BY DESC(?UsageShare)
```

If no building usage is returned from the query, the default value of ```MULTI_RES``` building usage is set, consistent with the default building usage type used by the CEA. In the case of multiple usages for one building, the OntoBuiltEnv usage concepts are first mapped to the CEA defined usage types according to the mapping at the bottom section of this README; then, since CEA only allows up to three usage types per building, the top three usages and their usage share are passed to CEA as input.

#### 4.3.1. Building Usage Mapping
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

WARNING: Please note that the CEA Agent, by default, assumes that the historical weather data is stored in the Blazegraph and PostgreSQL of the same stack as this agent.

## 5. Information Retrieved from PostGIS
### 5.1. Terrain Data
The agent will attempt to retrieve terrain data from the stack PostGIS. The queried area for the terrain data will have the building received in the request at the center. The queried area will cover the target buildings in the `iri` request parameter and its surrounding buildings that were retrieved in the surroundings query.

[CEAAgentConfig.properties]: ./cea-agent/src/main/resources/CEAAgentConfig.properties
[cea-agent.json]: ./stack-manager-input-config/cea-agent.json
[cea-agent-debug.json]: ./stack-manager-input-config/cea-agent-debug.json
[stack-data-uploader]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader
[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config services]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services
[TWA Visualisation Framework]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/web/twa-vis-framework
[FeatureInfoAgent]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/FeatureInfoAgent