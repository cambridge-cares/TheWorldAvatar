# PIPS TimeSeries Agent
This agent is designed to receive an access token, carry out verification with Keycloak to check whether the user is authorized, if authorized, return a JSON Object containing timeseries data.

# Prerequisite
1. It is necessary to have Keycloak set up properly. Refer to the official [Keycloak guides](https://www.keycloak.org/guides#getting-started) for how to get started. 

2. The TWA (TheWorldAvatar) stack can also be used to set up the Keycloak service along with a variety of other services. Refer to [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for more information.

3. It is necessary to have a PostgreSQL database set up properly. The tables and columns should have a structure similar to how the [OPCUAAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OPCUAAgent) construct its tables and columns. This agent is originally designed to work with the [OPCUAAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/OPCUAAgent) but it is possible to reuse the agent for other databases as long as they have a similar structure.

# Building the Agent
The Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You will need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

# Set up and deployment
## Set up
The agent can be deployed as a standalone or as part of the stack.

### Standalone
In `docker-compose.yml`, under the environment section, modify the following:
- `CLIENT_ID` ID of the client in Keycloak being used for authorization 
- `KEYCLOAK_REALM_PATH` Keycloak path for the specific realm

### Part of the stack
In `./stack-manager-input-config-service/pips-timeseries-agent.json`, under the `ENV` section, modify the following:
- `CLIENT_ID` ID of the client in Keycloak being used for authorization 
- `KEYCLOAK_REALM_PATH` Keycloak path for the specific realm

## Deployment
### Standalone
Open a terminal in the same directory as this README and run the following to spin up the container:
```
docker compose up -d
```
The agent will be located at port 1080.

### Part of the stack
Open up the command prompt in the same directory as this README, run the command below to build the docker image:
```
docker build --target agent -t pips-timeseries-agent:1.0.0 .
```
Copy `stack-manager-input-config-service/pips-timeseries-agent.json` to the services folder under your stack-manager directory and start up the stack.

### Run the agent
The agent has two routes, a status route and a timeseries route. A description for each route is provided below.

#### Status route
This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:1080/pips-timeseries-agent/status
```
and it should return:

{"Result":"Agent is ready to receive requests."}

#### TimeSeries route
This request checks whether the requestor is authorized before returning a JSONObject (e.g. timeseries data or invalid token message or unauthorized message). The request has the following format:
```
curl -H "Authorization: Bearer YOUR_TOKEN_HERE" http://localhost:1080/pips-timeseries-agent/timeseries?source=<schema>&num=<integer>
```

The following parameters will need to be provided:
- A valid token obtained from Keycloak, replace `YOUR_TOKEN_HERE` with the valid token
- The source from which to retrieve the data from, the source refers to the schema in which the tables are located under. Replace `<schema>` with the appropriate value.
- The number of latest readings to retrieve from the database. Replace `<integer>` with the desired number.

# Tests
Unit and integration tests have been developed for this agent. To run the tests, open a terminal in the same directory as this README and run the following to spin up the containers:
```
docker compose -p test_pips_timeseries_agent -f "docker-compose-test.yml" up -d
```

This will spin up the following containers:
1. a container containing the tests
2. a Keycloak container with testrealm, authorization scopes, policies, resources, two users and two clients. Refer to `exported-realm.json` located at `./PIPSTimeSeriesAgent/src/test/resources`.
3. a PostgreSQL container