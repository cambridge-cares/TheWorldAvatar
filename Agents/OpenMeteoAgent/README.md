# OpenMeteo agent

## 1. Agent Description

The OpenMeteo agent is intended for instantiating historical weather data retrieved with the Open-Meteo API (https://open-meteo.com/).

The agent will instantiate historical weather data for the weather parameters below, as defined by Open-Meteo, retrieved with the Open-Meteo API. The weather parameters will be instantiated as instances of the Environmental Measurement Station Ontology (OntoEMS) concepts.

| Open-Meteo API Weather Parameter | Corresponding OntoEMS concept |
|:--------------------------------:|:-----------------------------:|
|      ```temperature_2m```        |        AirTemperature         |
|    ```relativehumidity_2m```     |       RelativeHumidity        |
|        ```dewpoint_2m```         |           DewPoint            |
|        ```pressure_msl```        |      AtmosphericPressure      |
|            ```rain```            |           Rainfall            |
|          ```snowfall```          |           Snowfall            |
|         ```cloudcover```         |          CloudCover           |
|  ```direct_normal_irradiance```  |    DirectNormalIrradiance     |
|     ```diffuse_radiation```      |  DiffuseHorizontalIrradiance  |
|       ```windspeed_10m```        |           WindSpeed           |
|     ```winddirection_10m```      |         WindDirection         |

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
The agent is designed to use the stack Blazegraph. Please ensure that the Blazegraph namespace corresponding to ```route.label``` in ```./openmeteo-agent/src/main/resources/config.properties```, is set up in the stack Blazegraph.

### 2.4. Access Agent Set Up
The agent is designed to use the [Access Agent](../AccessAgent). Spin the access agent up as part of the same stack as spun up by the Stack Manager. Please ensure that the routing information for the Blazegraph namespace corresponding to ```route.label``` in ```./openmeteo-agent/src/main/resources/config.properties``` is uploaded, see the [Access Agent README](../AccessAgent) for instruction on uploading routing information.

### 2.5. PostgreSQL Set Up
The agent is designed to use the stack PostgreSQL. The historical weather time series will be stored in the stack PostgreSQL database, which is specified as ```db.name``` in ```./openmeteo-agent/src/main/resources/config.properties```. Please ensure the database specified by ```db.name``` is set up in the stack PostgreSQL.

### 2.6. Build and Run
In the same directory as this README, first build the Docker image by running
```
docker-compose build
```

After the image is built, copy ```./stack-manager-input-config/openmeteo-agent.json``` and place it in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager.

WARNING: If tests are executed, they will fail for agent in stack, since the agent relies on methods provided by other classes that retrieve information on stack related configuration and such methods fail when not inside stack, which is the case at the time of test execution.

### 2.7. Debugging
To debug, put ```./stack-manager-input-config/openmeteo-agent-debug.json``` instead of ```./stack-manager-input-config/openmeteo-agent.json```  in ```../Deploy/stacks/dynamic/stack-manager/inputs/config/services```. Then, in the ```../Deploy/stacks/dynamic/stack-manager/``` directory, run 
```
./stack.sh start <STACK NAME>
```
Replace ```<STACK NAME>``` with the name of the stack that was spun up by Stack Manager. The debugger port will be available at 5005.

## Agent Endpoints 

The agent supports POST requests and is reachable at http://localhost:3838/openmeteo-agent, where 3838 is the default port number used by stack manager. If another port number was specified when spinning up the stack, please replace 3838 with the specified port number. The agent provides two endpoints: run endpoint (http://localhost:3838/openmeteo-agent/run), where the agent will instantiate the weather data retrieved with the Open-Meteo API for the user specified coordinate and time; delete endpoint (http://localhost:3838/openmeteo-agent/delete), where the agent will delete the instantiated weather triples at the user specified coordinate.

### 1. Run Endpoint
Available at http://localhost:3838/openmeteo-agent/run

The run endpoint accepts the following POST request parameters:
- ```latitude```: the latitude of the weather station in WGS84
- ```longitude```: the longitude of the weather station in WGS84
- ```start_date```: the start date of the historic weather data wanted in "yyyy-mm-dd" as a string
- ```end_date```: the end date of the historic weather data wanted in "yyyy-mm-dd" as a string
- ```route```: (optional) access agent route to the Blazegraph namespace where the weather triples will be instantiated; if not provided, the weather triples will be instantiated at the default stack access agent route specified as ```route.label``` in ```./openmeteo-agent/src/main/resources/config.properties```

The agent will instantiate a weather station located at (```latitude```, ```longitude```), and the historical weather data reported by the station will be stored as time series. 

The agent will also return the instantiated weather station IRI, e.g. ```{"stationIRI": ["https://www.theworldavatar.com/kg/ontoems/ReportingStation_6f883bc9-e3ba-493e-9db6-f7ce67844442"]}```.

If there is already a weather station located at (```latitude```, ```longitude```), the agent will delete old historical weather data from the weather time series and update the time series with the historical weather data with the specified date in the request.

Example request:
```
{
    "latitude": "52.52",
    "longitude": "-14.41",
    "start_date": "2021-01-02",
    "end_date": "2021-01-03"
}
```

After receiving the above request, the agent will instantiate a weather station located at (52.52, -14.41) with historical weather data, retrieved with Open-Meteo API, from 2021-01-02 to 2021-01-03.

### 2. Delete Endpoint
Available at http://localhost:3838/openmeteo-agent/delete

The delete endpoint accepts the following POST request parameters:
- ```latitude```: the latitude of the weather station in WGS84
- ```longitude```: the longitude of the weather station in WGS84
- ```route```: (optional) access agent route to the Blazegraph namespace where the weather triples are stored; if not provided, the agent will attempt to delete weather triples from the default stack access agent route specified as ```route.label``` in ```./openmeteo-agent/src/main/resources/config.properties```

If there is a weather station instance located at (```latitude```, ```longitude```), the agent will delete the weather station located at (```latitude```, ```longitude```) and its associated triples and time series.

Example request:
```
{
    "latitude": 52.52,
    "longitude": -14.41
}
```

After receiving the above request, the agent will delete the weather station located at (52.52, -14.41) and its associated triples and time series.