# TrafficIncidentAgent
## 1. Description

This agent downloads real-time traffic-incident data from [LTA Datamall](https://datamall.lta.gov.sg/content/datamall/en.html) and stores them in Postgres database in the stack. The data collected is instantiated using [Ontop mapping](inputs/trafficincident.obda). TrafficIncidentAgent also creates a Geoserver layer for visualisation purposes.  

## 2. Prerequisites

This agent is developed as part of the [Singapore-sea-level-rise stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/Singapore-sea-level-rise). 

### 2.1. Stack Set Up
The agent has been implemented to work in the stack. Follow the instructions in the [stack-manager]'s README to set up the stack.

## 3. Agent Configuration
### 3.1 Config Properties
You need to have an `API_key` which can be obtained by registering from [Land Transport Data Mall](https://datamall.lta.gov.sg/content/datamall/en/request-for-api.html). The API key needs to entered in the file [config.properties](inputs/config.properties) at `trafficincident.accountKey=` to retrieve the data from LTA API.

## 4. Build
### 4.1. GitHub Credentials
The docker image uses TheWorldAvatar [maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:

```bash
./credentials/
        repo_username.txt
        repo_password.txt
```
## 5. Deployment

### 5.1 Retrieving TrafficIncidentAgent's image

The TrafficIncidentAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/traffic-incident-agent) using ` docker pull ghcr.io/cambridge-cares/traffic-incident-agent:<LATEST-VERSION>`

### 5.2 Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the TrafficIncidentAgent Docker container to be deployed in the stack. To do so, place [TrafficIncidentAgent.json](./stack-manager-config/inputs/config/services/trafficincidentagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

### 5.3 Running the Agent

The agent is reachable at the `/start` endpoint.

```
curl -L -X POST "http://localhost:3838/traffic-incident-agent/start"
```

The modeling of the Traffic Incidents is achieved by maintaining a time interval to track the start and end time of the incident. When the incident first appears, the end time field will be left as 0 and only gets updated when the incident is not appearing in the newly queried result. Hence, the accuracy of data needs to be maintained via having regular call of the query. While you have the container running, you do not need to create any database or table as it is already automated. By opening the Adminer (PostgreSQL GUI) at http://localhost:3838/adminer/ui/?username=postgres&pgsql=. The Database slot is the default `postgres` and the table is named as `TrafficIncident`. The table should include `starttime:bigint`, `endtime:bigint`, `type:character varing`, `message:character varying`, `latitude:double precision`, `longitude:double precision`, `location: geography NULL`, `status:Boolean`.

# Author 
Phua Shin Zert (shinzert.phua@cares.cam.ac.uk) <br>
Sun Xin Yu (https://github.com/Echomo-Xinyu) <br>
October 2024 


[stack-data-uploader]: https://github.com/TheWorldAvatar/stack/tree/main/stack-data-uploader
[stack-manager]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager
[stack-manager config directory]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/inputs/config/services