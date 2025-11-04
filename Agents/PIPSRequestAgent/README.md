# PIPS Request Agent
This agent is designed to go through an authentication process with Keycloak, receive back an access token if authenticated, submit the token to the PIPS TimeSeries Agent for authorization and if authorized, receive back a JSON Object containing the response (e.g. timeseries data or unauthorized message or invalid token message or invalid credentials message).

The agent also allows for the option of including a client certificate (P12 format) in the request sent to the PIPS TimeSeries Agent. This is to accomodate for cases where the PIPS TimeSeries Agent is protected by client certificate authentication (e.g. Nginx).

# Prerequisite
1. It is necessary to have Keycloak set up properly. Refer to the official [Keycloak guides](https://www.keycloak.org/guides#getting-started) for how to get started. 

2. The TWA (TheWorldAvatar) stack can also be used to set up the Keycloak service along with a variety of other services. Refer to [Stack Manager](https://github.com/TheWorldAvatar/stack/tree/main/stack-manager) for more information.

3) It is necessary to have the PIPSTimeSeriesAgent set up properly. Refer to [PIPSTimeSeriesAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PIPSTimeSeriesAgent).

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
In `docker-compose.yml`, under the environment section:
- `PIPS_AGENT_TIMESERIES_PATH` endpoint for accessing the PIPSTimeSeriesAgent timeseries route
- `KEYCLOAK_REALM_PATH` Keycloak path for the specific realm
- `CLIENT_ID` ID of the client in Keycloak being used for authentication

Create a `secrets` folder in the same directory as this README, create the following files and folders in this folder:
- `client_secrets.txt` text file containing the secrets of the client in Keycloak being used for authentication
- `username.txt` text file containing the username of a valid user in Keycloak
- `password.txt` text file containing the password of a valid user in Keycloak
- `client_certificates` folder containing the client certificate and password
    - `client_cert.p12` client certificate in P12 format
    - `client_cert_password.txt` text file containing the password for the client certificate

## Deployment
Open a terminal in the same directory as this README and run the following to spin up the container:
```
docker compose up -d
```
The agent will be located at port 1080.

## Run the agent
The agent has two routes, a status route and a retrieve route. A description for each route is provided below.

### Status route
This request gets the status of the agent. The request has the following format:
```
curl -X GET http://localhost:1080/pips-request-agent/status
```
and it should return:

{"Result":"Agent is ready to receive requests."}

### Retrieve route
This route submits the credentials, Client ID and Client secrets to Keycloak for authentication, receive back an access token if authenticated, submit the token to the PIPS TimeSeries Agent for authorization and if authorized, receive back a JSON Object containing the response (e.g. timeseries data or unauthorized message or invalid token message or invalid credentials message). 

This route will also check the token's validity and interact with Keycloak to renew it if necessary.

The request has the following format:
```
curl -X GET "http://localhost:1080/pips-request-agent/retrieve?source=<source>&num=<number of readings to retrieve>&client_cert_auth=<true or false>"
```
- `<source>` The source from which to retrieve the data from, Refer to [PIPSTimeSeriesAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PIPSTimeSeriesAgent) for more information. Replace `<schema>` with the appropriate value.
- `<number of readings to retrieve>` The number of latest readings to retrieve. Replace `<number of readings to retrieve>` with the desired number.
- `<true or false>` Whether client certificate authentication is required or not. Please make sure to set up the necessary directory and files as described under the section [Set up](#set-up).

# Tests
Unit and integration tests have been developed for this agent. To run the tests, open a terminal in the same directory as this README and run the following to spin up the containers:
```
docker compose -p test_pips_request_agent -f "docker-compose-test.yml" up -d
```

This will spin up the following containers:
1. a container containing the tests
2. a Keycloak container with testrealm and a test user, refer to `exported-realm.json` located at `./PIPSRequestAgent/src/test/resources`.
3. a MockServer container