# OPFAgent

## Purpose
The purpose of OPFAgent is to handle HTTP requests to perform Optimal Power Flow (OPF) analysis on a power network instantiated in the knowledge graph. Information about the power network will be retrieved from the triple store and time series data will be extracted from a relational database. After running the simulation, OPF results will be stored back into the relational database as time series data.

## Requirements
- In order to run OPFAgent, a local version (or if you are running in a stack, a stack version) of (TripleStore)AccessAgent needs to be deployed. Refer to [AccessAgent README](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_ACCESS_AGENT/README.md) for Access Agent setup. Routing information of the target blazegraph should be uploaded accordingly before calling OPFAgent.

- As OPFAgent interacts with time series data stored in a relational database, URL, username and password of the database are required. Refer to the [time series client properties](#time-series-client-properties) section below for more details.

### Time-series client properties
The time-series client property file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `opf-agent`->`config` folder.

## Deployment

### Deploying OPFAgent outside the stack
- Replace information in the `client.properties` file in `opf-agent`->`config` folder with details of the blazegraph and relational database you are connecting to.
- From the command line, and in the same directory as this README, run:
```
docker-compose up -d
```
The agent is reachable on localhost port 39999 by default (you can change this in docker-compose.yml). Example of requests is given in the [Run the agent](#run-the-agent) section below.

### Deploying OPFAgent as part of a stack
- Replace information in the `client.properties` file in `opf-agent`->`config` folder with details of the blazegraph and relational database you are connecting to. Replace the placeholders for stack name with the name of your stack if you are connecting to the blazegraph in stack.
- To build Docker image of OPFAgent, from the command line, and in the same directory as this README, run (replace the version number if applicable):
```
docker build -t "opf-agent:1.0.0" .
```
- In the `access-agent.json` file within the `stack-manager-input-config` folder, adjust the image version if applicable, and replace the placeholder for the stack name in the endpoint environment variables with the name of your stack. 
- Copy the `access-agent.json` file and `opf-agent.json` file into the `inputs/config` folder of the stack manager.
- Start the stack manager as usual. This should start an access agent container and an OPFAgent container as part of your stack.

## Run the agent
The OPFAgent is accessible at `localhost:39999`, or `host.docker.internal:39999` from inside a Docker container (on Windows/Mac), or, if running the agent within a stack, `localhost:3838` (by default) from outside the stack, `<STACK NAME>-opf-agent:8080` from within. To run the agent, a POST request must be sent to http://localhost:39999/opf-agent/startsimulationOPF (replace the host and port number if applicable) with a correct JSON Object. Follow the example request shown below.
```
POST http://localhost:39999/opf-agent/startsimulationOPF
Content-Type: application/json
{"electricalnetwork":"http://localhost:48888/ntuenergy","baseMVA":"1","time":"2020-01-01T01:00:00+00:00","hasSolar":"false"}
```
In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{\"electricalnetwork\":\"http://localhost:48888/ntuenergy\",\"baseMVA\":\"1\",\"time\":\"2020-01-01T01:00:00+00:00\",\"hasSolar\":\"false\"}" http://localhost:39999/opf-agent/startsimulationOPF
```

### Request content
The input JSON object should contain the following keys:
- `electricalnetwork` the URL to call Access Agent. E.g.
    ```
    http://localhost:48888/label or http://host.docker.internal:48888/label
    ```
    or, if running within a stack,
    ```
    http://<STACK NAME>-access-agent:8080/label,
    ```
    where the label corresponds to the label uploaded to the router.
- `baseMVA` the baseMVA of the power system
- `time` the time point to run OPF analysis on, in the format of `yyyy-MM-ddTHH:mm:ss`
- `hasSolar` boolean value indicating whether solar PVs are considered in this system, should be either `true` or `false`
