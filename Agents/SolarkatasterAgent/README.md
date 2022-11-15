# Solarkataster Agent

## Description

The Solarkataster Agent is intended for querying Solarkataster data for the following 12 parameters and instantiating them as a time series:
```
jan_median, feb_median, mrz_median, apr_median, mai_median, jun_median, jul_median, aug_median, sep_median, okt_median, nov_median, dez_median
```
For each of the building in the Solarkataster data, the 12 parameters are instantiated as one time series, with the time series being an instance of *OntoTimeSeries:AverageTimeSeries* and having an averaging period of one month.

## Post Request

The agent is available at http://localhost:10101/solarkataster_agent/run

The agent accepts one parameter, `table`, which should be the name of the table that the photovoltaik.gdb or the solarthermie.gdb from the Solartakataser data is uploaded to. 

Example request:
```
{ "table" : "photovoltaik" }
```
```
{ "table" : "solarthermie" }
```

## Build Instructions

### Stack set up
The agent has been implemented to work with stack, which requires the stack to be [set up](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) and for the Solarkataster data to be [uploaded to stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-data-uploader), before building and running the agent.
Before building, change the placeholder `<STACK-NAME>` in `./solarkataster_agent/src/main/resources/config.properties` to the name of your stack.

### Required credentials
You will also need to provide your credentials in single-word text files located like this:
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
        solarkataster_username.txt
        solarkataster_password.txt
        timeseries_username.txt
        timeseries_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

solarkataster_username.txt and solarkataster_password.txt should contain the username and password that grants access to the Postgres database where the Solarkataster data are stored.

timeseries_username.txt and timeseries_password.txt should contain the username and password that grants access to the database used by the time series client

### Building the agent
To build and start the agent, you simply need to spin up a container from the image.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The agent is reachable at "solarkataster_agent/run" on localhost port 10101.
