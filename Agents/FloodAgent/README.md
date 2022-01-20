# Flood Agent
This agent downloads data from https://environment.data.gov.uk/flood-monitoring/doc/reference and stores them in Blazegraph (station info) and PostgreSQL (time series data). After updating the databases, the code writes a time series json file for visualisation (in addition to a geojson file containing locations of the stations that is written once).

## Building and running
This section specifies the minimum requirement to build the docker image. 

This agent uses the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide your credentials in single-word text files located like this:
```
credentials/
  repo_username.txt
  repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

Next, you'll need to specify the urls/credentials for the Blazegraph and PostgreSQL for this agent to use in `FloodAgent/src/main/resources/credentials.properties`. A template `credentials.properties.template` is provided in the same folder. To access Blazegraph and Postgres on your local machine, but not within the same docker network, the host name to use is `host.docker.internal`. User and password for Blazegraph can be left empty if not required. 

In addition to the credentials for the databases, a directory to write the geojson and flood needs to be specified.

There are two images that can be built, depending on the requirements and where it's being deployed

### Option 1
Updating the database and writing a time series JSON file daily

```
docker build --target default -t [TAGNAME]
```

This docker image runs the `LaunchScheduledUpdaterAndWriter` class.

### Option 2
Skip update and only write the output files
```
docker build --target write-only -t [TAGNAME]
```

This docker image runs the `LaunchScheduledWriterOnly` class.
### Execution
To run
```
docker run -d [TAGNAME]
```

This will run the container in detached mode. This application is designed to be left in the background, there is a scheduler that runs the process once a day.

Logs are saved at `root/.jps/` by default, you can copy the logs into your local environment by using the following command
```
docker cp flood:/root/.jps .
```

## Main entrypoint
The main entrypoint is the `LaunchScheduledUpdaterAndWriter` class, it is set as the main class in the manifest, i.e. running the `java -jar FloodAgent-1.0.0-SNAPSHOT.jar` command will launch this by default.

When launched, it will initialise the flood monitoring stations if they are not initialised, and start a scheduled task that runs once a day. The code will always download readings from the day before and upload the data to the time series tables in PostgreSQL.

## LaunchWriterOnly
This will write the latest output files only.

## Initialisation
To initialise manually, it is possible to run the `InitialiseStations` class directly. It has a `main` function that does not need any inputs.

To run it on the command line:
```
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.InitialiseStations
```

## Updating the stations
To manually add data from a specific date, run the `UpdateStations` class with a date as its input in ISO-8601 format, e.g. `2021-09-30`.

This class can be useful to populate the database with historical data, e.g. you can run the following in your script
```
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.UpdateStations 2021-09-30
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.UpdateStations 2021-10-01
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.UpdateStations 2021-10-02
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.UpdateStations 2021-10-03
```

## Resetting endpoints
Run the `ResetEndpoints` class to reset both the Blazegraph and Postgres database.
```
java -cp FloodAgent-1.0.0-SNAPSHOT.jar uk.ac.cam.cares.jps.agent.flood.ResetEndpoints
```

## Outputs
The `WriteOutput` takes in a date in its input, e.g. `2021-09-30`, and writes out a JSON file containing the time series, and a GeoJSON file containing the station locations for visualisation.
