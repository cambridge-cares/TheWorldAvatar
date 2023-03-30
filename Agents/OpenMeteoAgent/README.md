# OpenMeteo agent

## Description

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

## Build Instructions

### Required credentials
The docker image uses TheWorldAvatar maven repository (https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/). You'll need to provide your credentials (github username/personal access token) in single-word text files located:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt should contain your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

The agent requires a PostgreSQL database to save time series data in. The address of the database used is provided as ```db.url``` in
```
./openmeteo_agent/src/main/resource
        config.properties
```

The username and password for the PostgreSQL database need to be provided in
```
./credentials/
        postgres_username.txt
        postgres_password.txt
```

### Building the agent
To build and start the agent, you simply need to spin up a container from the image.

In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

## Agent Endpoints 

The agent is reachable at two endpoints: run endpoint (http://localhost:10101/openmeteo_agent/run), where the agent will instantiate the weather data retrieved with the Open-Meteo API for the user specified coordinate and time; delete endpoint (http://localhost:10101/openmeteo_agent/delete), where the agent will delete the instantiated weather triples at the user specified coordinate.

### 1. Run Endpoint
Available at http://localhost:10101/openmeteo_agent/run

The run endpoint accepts the following request parameters:
- ```latitude```: the latitude of the weather station in WGS84 as a number
- ```longitude```: the longitude of the weather station n WGS84 as a number
- ```start_date```: the start date of the historic weather data wanted in "yyyy-mm-dd" as a string
- ```end_date```: the end date of the historic weather data wanted in "yyyy-mm-dd" as a string

Example request:
```
{
    "latitude": 52.52,
    "longitude": -14.41,
    "start_date": "2021-01-02",
    "end_date": "2021-01-03"
}
```

After receiving the above request, the agent will instantiate a weather station located at (52.52, -14.41) with historical weather data, retrieved with Open-Meteo API, from 2021-01-02 to 2021-01-03.

### 2. Delete Endpoint
Available at http://localhost:10101/openmeteo_agent/delete

The delete endpoint accepts the following request parameters:
- ```latitude```: the latitude of the weather station in as a number
- ```longitude```: the longitude of the weather station as a number

Example request:
```
{
    "latitude": 52.52,
    "longitude": -14.41
}
```

After receiving the above request, the agent will delete the weather station located at (52.52, -14.41) and its associated triples.