# UserAgent
## 1. Description
UserAgent is an agent that manages TWA user with other services account or devices (e.g. smartphones). It currently supports the following functions through the endpoints:
- `/registerPhone`: register a new phone with the user
- `/getPhoneIds`: get the phone ids of the user
- `/registerOuraRing`: register an Oura ring device to a user
- `/status`: get the status of the agent. Used for testing.

## 2. Requirements
Launch stack with the default containers and the following additional containers:
- keycloak

Requests `/registerPhone`, `/getPhoneIds` and `/registerOuraRing` require Bearer user access token from Keycloak.

## 3. Usage
### `/registerPhone`

Input body:
```json
{"phoneId": "<phone-id>"}
```
Sample query:

Bearer access token should be added in header.
```
curl --location 'http://localhost:3838/user-agent/registerPhone' \
--header 'Content-Type: application/json' \
--header 'Authorization: ••••••' \
--data '{"userId": "<user-id>", "phoneId": "<phone-id>"}'
```

### `/registerOuraRing`

Input body:
```json
{"ouraRingApiKey": "<oura-ring-api-key>"}
```
Sample query:

Bearer access token should be added in header.
```
curl --location 'http://localhost:3838/user-agent/registerOuraRing' \
--header 'Content-Type: application/json' \
--header 'Authorization: ••••••' \
--data '{"ouraRingApiKey": "<oura-ring-api-key>"}'
```

### `/getPhoneIds`

No data needed. Sample query:

Bearer access token should be added in header.
```
curl --location --request GET 'http://localhost:3838/user-agent/getPhoneIds' \
--header 'Content-Type: application/json' \
--header 'Authorization: ••••••' \
```

## 4. Deploy 
### 4.1 Retrieving UserAgent's image
The UserAgent should be pulled automatically with the stack-manager, if not you can pull the latest version from [cambridge_cares package](https://github.com/orgs/cambridge-cares/packages/container/package/user-agent) using `docker pull ghcr.io/cambridge-cares/user-agent:<LATEST-VERSION>`

### 4.2 Import Keycloack configuration
1. Login to the Keycloak admin console
2. Create realm
3. Import [user-agent.json](stack-manager-config/inputs/data/user-agent.json) as a client in the realm
4. Update the root url in client setting
5. Download user-agent client adapter config from Keycloak admin console, and place the json adapter to [resources](UserAgent/src/main/resources)

Check this Keycloak [guide](https://www.keycloak.org/docs/latest/authorization_services/index.html#_resource_server_overview) for more information.

### 4.3 Starting with the stack-manager
The agent has been implemented to work in the stack, which requires the UserAgent Docker container to be deployed in the stack. To do so, place [user-agent.json](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory].

Then, run `./stack.sh start <STACK NAME>` in the [stack-manager](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager) main folder. This will spin up the agent in the stack.

## 5. Build and debug
## 5.1 Credentials
The docker image uses TheWorldAvatar maven repository (`https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/`).
You will need to provide your credentials (GitHub username/personal access token) in single-word text files as follows:
```
./credentials/
        repo_username.txt
        repo_password.txt
```

### 5.2 Building Docker Image
In the same directory as this README, run `./stack.sh build`. This will build the TrajectoryQueryAgent local Docker Image. 

### 5.2 Spinning up with stack-manager
To debug the agent, replace [`user-agent-debug.json`](stack-manager-config/inputs/config/services/user-agent-debug.json) instead of [`user-agent.json`](stack-manager-config/inputs/config/services/user-agent.json) in the [stack-manager config directory]. 

Spin up with `./stack.sh start <STACK NAME>` in the [stack-manager]'s main folder.
The debugger port will be available at 5005.

[stack-manager]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager
[stack-manager config directory]: https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager/inputs/config/services