# TrajectoryQueryAgent
## 1. Description
TrajectoryQueryAgent is an agent that handles trajectory related tasks. It currently supports the following functions through the endpoints:
- `/createlayer`: create geoserver layer and postgis functions for the geoserver query
- `/getDatesWithData`: get dates that have trajectory data

### Workflow: /createlayer
1) Receives `userIDs`s from UserAgent.
2) SPARQL Query for `pointIRIs`, `speedIRIs`, `altitudeIRIs` and `bearingIRIs` using `userIDs`
3) Create postgis function in Postgres and geoserver layers in GeoServer
4) Return `pointIRIs`, `speedIRIs`, `altitudeIRIs` and `bearingIRIs` to application as response

Sample request of `/createlayer`, where `<USERID>` is the id of the user. The USERID is to get the default viewparam values of the geoserver layer, one may choose to send other viewparam values to the geoserver layer for their own trajectories.
```
curl -X POST "localhost:3838/trajectoryqueryagent/createlayer?userID=<USERID>"
```

To use the layer created, send the request directly to the geoserver with the returned `pointIRI`, `speedIRI`, `altitudeIRI`, `bearingIRI` and a specific date in TIMESTAMPTZ format.
```
http://localhost:3838/geoserver/twa/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=twa:trajectoryLine&outputFormat=application/json&viewparams=pointiri:<POINTIRI>;speediri:<SPEEDIRI>;altitudeiri:<ALTITUDEIRI>;bearingiri:<BEARINGIRI>;date:<DATE>;
```

The layer is created in a way such that it is able to accept comma separated list of IRIs in the viewparams. Number of IRIs for each field should be the same.
```
pointiri:<POINTIRI_1>,<POINTIRI_2>,<POINTIRI_3>;
```

## 2. Pre-requisites
Launch [stack](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) with the default containers and the following additional containers:
- [UserAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/1771-dev-user-agent/Agents/UserAgent)
- information from [SensorLoggerMobileAppAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/SensorLoggerMobileAppAgent) to be instantiated

## 3. Deploy 
### 3.1 Retrieving TrajectoryQueryAgent's image
The TrajectoryQueryAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/trajectoryqueryagent) using `docker pull ghcr.io/cambridge-cares/trajectoryqueryagent:<LATEST-VERSION>`

### 3.2 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the TrajectoryQueryAgent Docker container to be deployed in the stack. To do so, place [trajectoryqueryagent.json](stack-manager-config/inputs/config/services/trajectoryqueryagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

## 4. Develop and Debug
## 4.1 Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 4.2 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the TrajectoryQueryAgent local Docker Image. 

### 4.3 Spinning up with stack-manager
To debug the agent, replace [`trajectoryqueryagent-debug.json`](stack-manager-config/inputs/config/services/trajectoryqueryagent-debug.json) instead of [`trajectoryqueryagent.json`](stack-manager-config/inputs/config/services/trajectoryqueryagent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services