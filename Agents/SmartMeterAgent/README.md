# Smart Meter Agent

## Purpose
The purpose of Smart Meter Agent is to handle HTTP requests to retrieve latest reading from a database storing smart meter readings every minute, or retrieve all valid historical readings from a database or a CSV file, and upload the data to instantiated time series in the KG.

## Requirements
- In order to run SmartMeterAgent, a local version (or if you are running in a stack, a stack version) of (TripleStore)AccessAgent needs to be deployed. Refer to [AccessAgent README](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_ACCESS_AGENT/README.md) for Access Agent setup. Routing information of the target blazegraph should be uploaded accordingly before calling SmartMeterAgent.

- The target blazegraph should contain a power network instantiated according to [OntoPowSys](http://www.theworldavatar.com/ontology/ontopowsys/), and the related time series should be instantiated before calling SmartMeterAgent.

- As SmartMeterAgent interacts with time series data stored in a relational database, URL, username and password of the target PostgreSQL database are required. Refer to the [time series client properties](#time-series-client-properties) section below for more details.

- A mapping file called `mappings.csv` should be put inside `agent/config` folder, which contains mapping information between device names in smart meter readings and bus number values in the microgrid power system. Please note that all bus numbers should be put in the mapping file even if they do not have device/readings. Example is given for a 17-bus microgrid in `agent/config/mappings.csv` file.

- If reading from an SQLite database file, `db.url` in `agent/config/db.properties` file should be changed to `jdbc:sqlite:` + path to the database file. Database username and password should also be changed accordingly if applicable. If reading from a csv file, the csv file should be named as `readings.csv` and put inside `agent/database` folder.

### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in `agent/config` folder.

## How it works
SmartMeterAgent only retrieves 1 group of valid reading every minute from database or csv file (refer to [data filtering](#data-filtering) section for definiition of valid readings). The agent queries the microgrid triple store for dataIRIs of timeseries data of each bus node, and maps the IRIs to devices in smart meter readings. Reading data are filtered and processed before uploading to the timeseries database.

### Data filtering
A group of readings for a time point is valid if and only if:
1. All devices in the mapping file are switched on (have readings), i.e. the group of readings has exactly one reading for each device;
2. There is no data package lost, i.e. Pd load, current, voltage and frequency values should be non-zero and not null;
3. Timestamp of every reading is the same and is within the given time boundary.

## Deployment
### Deploying SmartMeterAgent outside the stack
- Replace information in the `client.properties` file in `agent/config` folder with details of the target blazegraph and timeseries relational database you are connecting to.
- Fill in mapping information for your power system in `agent/config/mappings.csv` file.
- If reading from a database, replace information in `agent/config/db.properties` file with details of the smart meter reading database you are connecting to, and put the database file inside `agent/database` folder. Uncomment line 27 (COPY database file to working directory) of `Dockerfile`, which can be found in the same directory as this README, and replace the database file name according to the instruction in the file.
- If reading from a csv file, rename your csv file as `readings.csv` and put it in `agent/database` folder.
- From the command line, and in the same directory as this README, run:
```
docker-compose up -d
```
The agent is reachable on localhost port 39998 by default (you can change this in docker-compose.yml). Example of requests is given in the [Run the agent](#run-the-agent) section below.

### Deploying SmartMeterAgent as part of a stack
- Replace information in the `client.properties` file in `agent/config` folder with details of the target blazegraph and timeseries relational database you are connecting to. Replace the placeholders for stack name with the name of your stack if you are connecting to the blazegraph in stack.
- Fill in mapping information for your power system in `agent/config/mappings.csv` file.
- If reading from a database, replace information in `agent/config/db.properties` file with details of the smart meter reading database you are connecting to, and put the database file inside `agent/database` folder. Uncomment line 27 (COPY database file to working directory) of `Dockerfile`, which can be found in the same directory as this README, and replace the database file name according to the instruction in the file.
- If reading from a csv file, rename your csv file as `readings.csv` and put it in `agent/database` folder.
- To build Docker image of SmartMeterAgent, from the command line, and in the same directory as this README, run (replace the version number if applicable):
```
docker build -t "smart-meter-agent:1.0.0" .
```
- In the `access-agent.json` file within the `stack-manager-input-config` folder, adjust the image version if applicable, and replace the placeholder for the stack name in the endpoint environment variables with the name of your stack. 
- Copy the `access-agent.json` file and `smart-meter-agent.json` file into the `inputs/config/services` folder of the stack manager.
- Start the stack manager as usual. This should start an access agent container and an SmartMeterAgent container as part of your stack.

## Run the agent
The SmartMeterAgent is accessible at `localhost:39998`, or `host.docker.internal:39998` from inside a Docker container (on Windows/Mac), or, if running the agent within a stack, `localhost:3838` (by default) from outside the stack, `<STACK NAME>-smart-meter-agent:8080` from within. To run the agent, a POST request must be sent to http://localhost:39998/smart-meter-agent/upload (replace the host and port number if applicable) with a correct JSON Object. Follow the example request shown below (note that dataBefore and dataAfter are optional in the request).
```
POST http://localhost:39998/smart-meter-agent/upload
Content-Type: application/json
{"dataSource":"csv","dataRequired":"historical","microgrid":"http://host.docker.internal:48888/microgrid", "dataAfter":"2022-11-09 21:04", "dataBefore":"2023-03-09 21:09"}
```
In curl syntax:
```
curl -v http://localhost:39998/smart-meter-agent/upload -X POST --header "Content-Type: application/json" -d "{\"dataSource\":\"csv\",\"dataRequired\":\"historical\",\"microgrid\":\"http://host.docker.internal:48888/microgrid\",\"dataAfter\":\"2022-11-09 21:04\",\"dataBefore\":\"2023-03-09 21:09\"}"
```

### Request content
The input JSON object should contain the following keys:
- `microgrid` the URL to call Access Agent. E.g.
    ```
    http://localhost:48888/label or http://host.docker.internal:48888/label
    ```
    or, if running within a stack,
    ```
    http://<STACK NAME>-access-agent:8080/label,
    ```
    where the label corresponds to the label uploaded to the router.
- `dataSource` where to retrieve smart meter readings, should be either `database` or `csv`
- `dataRequired` type of data to retrieve, should be either `historical` or `latest`
- [Optional] `dataAfter` time point in `yyyy-MM-dd HH:mm` format, SmartMeterAgent will retrieve data after this time (inclusive), not applicable when `dataRequired` is `latest`.
- [Optional] `dataBefore` time point in `yyyy-MM-dd HH:mm` format, SmartMeterAgent will retrieve data before this time (inclusive), not applicable when `dataRequired` is `latest`.
