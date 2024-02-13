# Weather Agent
This service provides the ability to instantiate and query weather stations anywhere around the world. Data are obtained from https://openweathermap.org/api by specifying the coordinates in the query.

It is designed to interact with the stack spun up by the stack manager.

## Setup
This section specifies the minimum requirement to build the docker image. 

This agent uses the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide your credentials in single-word text files located like this:
```
credentials/
  repo_username.txt
  repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Create a file called docker-compose.yml using docker-compose.yml.template, then insert the API_KEY for openweather.

The environment variables used by this container:
1) STACK_NAME
2) API_KEY (API key for https://openweathermap.org/api, note that you need to enable OneCall API in your subscription)
3) LAYERNAME
4) DATABASE
5) GEOSERVER_WORKSPACE

## Spinning up the stack
Navigate to ```Deploy/stacks/dynamic/stack-manager``` and run the following command:
```
./stack.sh start weather-stack 
```

## Deploying weather agent to the stack
In the same folder as this README, execute the following command
```
./stack.sh start weather-stack
```

## Technical specifications
The Docker container has the following access URLs

1. WeatherAgent/CreateStation (PUT)
2. WeatherAgent/UpdateStation (PUT)
3. WeatherAgent/DeleteStation (DELETE)

### WeatherAgent/CreateStation (PUT)
Inputs in the form of parameters:
1) lat - latitude of station
2) lon - longitude of station
3) name - name of the station, optional

If there is a station at the given coordinates, the request will be ignored.

Output: IRI of the created station with key "station", e.g. {"station": "http://station1"}.

An example request using curl:
```
curl -X PUT "http://localhost:8086/WeatherAgent/CreateStation?lat=0&lon=0&name=Station+1"
```

### WeatherAgent/UpdateStation (PUT)
Input in the form of parameter:
1) iri (compulsory) - IRI of the station to update
2) timestamp (optional) - Unix timestamp of the weather data you wish to add
For a successful update, it will return "Updated station: [IRI of station]" in its response.

Example request using curl:
```
curl -X PUT "http://localhost:8086/WeatherAgent/UpdateStation?iri=http://station1"
```

### WeatherAgent/DeleteStation (DELETE)
Input in the form of parameter:
1) iri - IRI of the station to delete

If successful, it will return "Deleted station: [IRI of station]".

Example request using curl:
```
curl -X DELETE "http://localhost:8086/WeatherAgent/DeleteStation?iri=http://station1"
```

## Ontology
The instantiated stations follow the OntoEMS ontology. Coordinates of weather stations are stored in PostGIS and queryable via Ontop, time series data is stored in PostGIS, and the remaining triples are instantiated in the triple-store.


