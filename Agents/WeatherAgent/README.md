# Weather Agent
This service provides the ability to instantiate and query weather stations anywhere around the world. Data are obtained from https://openweathermap.org/api by specifying the coordinates in the query.

## Minimum requirements
This section specifies the minimum requirement to build the docker image. 

This agent uses the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide your credentials in single-word text files located like this:
```
credentials/
  repo_username.txt
  repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Next, you'll need to specify the urls/credentials for the Blazegraph and PostgreSQL for this agent to use in `WeatherAgent/src/main/resources/credentials.properties`. User and password for Blazegraph can be left empty if not required.

Weather data are obtained from https://openweathermap.org/api. You will need an API key to use this agent and specify it in the `credentials.properties` file under `apikey`.

To build the image, you can run 
```
docker build -f Dockerfile .
```

## Local testing
A script can be written to make this process easier but not done due to time constraints at the time of writing.

A `docker-compose.yml` file is provided to spin up a stack with a Blazegraph and a PostgreSQL within the same network as the agent container. The dockerised weather agent will not be able to interact with the databases outside of its docker network. For the agent to access the Blazegraph, the hostname is `blazegraph` (specified in the compose file), port number = 8080. The `kg.url` to enter in the `credentials.properties` will be in the form of `http://blazegraph:8080/blazegraph/namespace/[NAME OF NAMESPACE]/sparql`. The Blazegraph namespace must have geospatial enabled.

The hostname for the PostgreSQL container is `postgres`, accessible via the default port 5432. The field to enter for `db.url` will be in the form `jdbc:postgresql://postgres/[NAME OF DATABASE]`.

Two extra files are needed in the credentials folder to define the passwords for the Blazegraph and PostgreSQL containers, i.e.:
```
  credentials/
      repo_username.txt
      repo_password.txt
      blazegraph_password
      postgres_password
```

## Technical specifications
The `weatheragent` Docker image has the following access URLs

1. WeatherAgent/CreateStation
2. WeatherAgent/UpdateStation
3. WeatherAgent/GetWeatherData/latest
4. WeatherAgent/GetWeatherData/history
5. WeatherAgent/GetStationsInCircle
6. WeatherAgent/GetStationsInRectangle
7. WeatherAgent/DeleteStation

These classes inherit the JPSAgent class, easiest way to interact with them is to use the AgentCaller class. These servlets receive HTTP GET requests with the input parameter being a JSON object within the `query` parameter, e.g. `http://localhost:8080/WeatherAgent/CreateStation?query=ENCODED_JSON_STRING`.

### WeatherAgent/CreateStation
Input: Coordinates in the form "lat#lon" where lat and lon are numerical values. The JSON key is "latlon". E.g. {"latlon": "0#1"}
Output: IRI of the created station with key "station", e.g. {"station": "http://station1"}.

### WeatherAgent/UpdateStation
Input: IRI of the station to be updated with the key "station", e.g. {"station": "http://station1"}.

For a successful update, it will return {"status": "update successful"} in its response.

If the station was last updated 30 min ago, the request will be ignored. If the update process fails, check the logs saved at `~/.jps/logs` by default.

### WeatherAgent/GetWeatherData
Weather data can be obtained via two different URLs, both options return a time series object. If the weather data at this station is more than 30 minutes old, it will be updated automatically before returning the values.

1) WeatherAgent/GetWeatherData/latest
As the name suggests, this gives the latest data point.
Input: IRI of the station to update with the "station" key., e.g. {"station": "http://station1"}
Output: Serialised TimeSeries object using Gson

To deserialise
```
Type timeSeriesType = new TypeToken<TimeSeries<Long>>() {}.getType();
TimeSeries<Long> ts_deserialise = new Gson().fromJson(JSONSTRING, timeSeriesType);
 ```
 where `JSONSTRING` is the response.

 2) WeatherAgent/GetWeatherData/history
 This requires an additional input, the number of hours to query back, in addition to the station IRI.
 Input: This requires an additional input, the number of hours to query back with the key "hour", in addition to the station IRI, e.g. {"station": "http://station1", "hour":x}, where x is the number of hours before the current time.
 Output: Serialised TimeSeries object using Gson.

### WeatherAgent/GetStationsInCircle
This function returns a list of station IRIs that are located within the given circle.
Input: Coordinates of the centre and radius in km with keys "centre" and "radius", e.g. {"centre": latlon, "radius":x} where latlon is in the form "lat#lon" and x is the radius in km.

Output: Station IRIs in a JSON array, i.e. {"station": [LIST OF STATIONS]}.

### WeatherAgent/GetStationsInRectangle
This function returns a list of station IRIs that are located within the given rectangle.
Input: Coordinates of the south-west and north-east corners with the keys "southwest" and "northeast", e.g. {"southwest":"lat#lon", "northeast":"lat#lon"}.
Output: Station IRIs in a JSON array, i.e. {"station": [LIST OF STATIONS]}.

### WeatherAgent/DeleteStation
Deletes a given station including time series data associated with it.
Input: IRI of station to delete with "station" key, e.g. {"station": "http://station1}.
Output: {"status": "delete success"}, if it is successful.




