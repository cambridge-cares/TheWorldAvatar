# TrajectoryQueryAgent

TrajectoryQueryAgent is an agent that handles trajectory related tasks. It currently supports the following functions through the endpoints:

- `/createLayer`: create geoserver layer and postgis functions for the geoserver query
- `/getDatesWithData`: get dates that have trajectory data

## Requirements

Launch [stack](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager) with the default containers and the following additional containers:

- information from [SensorLoggerMobileAppAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/SensorLoggerMobileAppAgent) to be instantiated

## Deploy

### Retrieving TrajectoryQueryAgent's image

The TrajectoryQueryAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/trajectoryqueryagent) using `docker pull ghcr.io/cambridge-cares/trajectoryqueryagent:<LATEST-VERSION>`

### Starting with the stack-manager

The agent has been implemented to work in the stack, which requires the TrajectoryQueryAgent Docker container to be deployed in the stack. To do so, place [trajectoryqueryagent.json](stack-manager-config/inputs/config/services/trajectoryqueryagent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager] main folder. This will spin up the agent in the stack.

## Develop and Debug

### Credentials

The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:

```
./credentials/
        repo_username.txt
        repo_password.txt
```

### Building Docker Image

In the same directory as this README, run `./stack.sh build`. This will build the TrajectoryQueryAgent local Docker Image.

### Spinning up with stack-manager

To debug the agent, replace [`trajectoryqueryagent-debug.json`](stack-manager-config/inputs/config/services/trajectoryqueryagent-debug.json) instead of [`trajectoryqueryagent.json`](stack-manager-config/inputs/config/services/trajectoryqueryagent.json) in the [stack-manager config directory].

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

## Routes

### 1. /createLayer

No input parameters required for this route

```bash
curl -X POST "localhost:3838/trajectoryqueryagent/createLayer"
```

Currently, three layers are created in the twa workspace. Unix timestamp in milliseconds is used for the layers.

1. trajectoryDeviceId
2. trajectoryUserId
3. bufferedLineDeviceId

In the following sections, attributes refer to variables in the SQL view table, SQL view parameters refer to parameters passed from the URL and substituted in the SQL view, e.g. %device_id% in [line layer (device id)].

SQL view parameters are specified as key:value pairs separated by semicolons in the viewparams parameter in the URL, e.g. `viewparams=p1:v1;p2:v2`, refer to the [official documentation](https://docs.geoserver.org/main/en/user/data/database/sqlview.html) for more information

#### trajectoryDeviceId layer

This layer consists of line segments with associated speed, altitude and bearing values, so that styling is possible. Refer to [line layer (device id)].

Attributes: time, speed, altitude, bearing, iri  
View parameters: device_id

#### trajectoryUserId

This layer consists of a single line, so detailed styling according to attributes such as speed is not possible. Refer to [line layer (user id)].

Attributes: iri  
View parameters: user_id

#### bufferedLineDeviceId

To be used in conjunction with trajectoryDeviceId, adds a 100 m buffer to the original line. lowerbound and upperbound are Unix timestamps in milliseconds.

Attributes: iri  
View parameters: device_id, lowerbound (optional), upperbound (optional)

### 2. /getDatesWithData

This route is primarily used by the timeline app, this route requires two additional environment variables and an additional header with the JSON web token.

Environment variables:

1. KEYCLOAK_SERVER
    - Base URL of KeyCloak server that generates the JSON web token for verification

2. KEYCLOAK_REALM
    - Name of KeyCloak realm of the requesting users

Needs the `Authorization` header in the following form:

`Authorization: Bearer <JWT_TOKEN>`

Parameters required:

1. timezone (refer [here](https://www.postgresql.org/docs/current/view-pg-timezone-names.html) for permissible names)

Response given in the form of
```json
{"result":[{"month":1,"year":2024,"days":"{1,2,3}"}],"message":"Succeed"}
```

[stack-manager]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager
[stack-manager config directory]: https://github.com/TheWorldAvatar/stack/tree/main/stack-manager/inputs/config/services
[line layer (device id)]: ./trajectoryqueryagent/src/main/resources/line_layer_device_id.sql
[line layer (user id)]: ./trajectoryqueryagent/src/main/resources/line_layer_user_id.sql
