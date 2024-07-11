# PIPS Request Agent
This agent is designed to go through an authentication process with Keycloak, receive back an access token if authenticated, submit the token to the PIPS TimeSeries Agent for authorization and if authorized, receive back a JSON Object containing the response (e.g. timeseries data or unauthorized message or invalid token message or invalid credentials message).

The agent also allows for the option of including a client certificate (p12 format) in the request sent to the PIPS TimeSeries Agent. This is to accomodate for cases where the PIPS TimeSeries Agent is protected by client certificate authentication (e.g. Nginx).

# Prerequisite
1. It is necessary to have Keycloak set up properly. Refer to the official [Keycloak guides](https://www.keycloak.org/guides#getting-started) for how to get started. 

2. The TWA (TheWorldAvatar) stack can also be used to set up the Keycloak service along with a variety of other services. Refer to [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for more information.

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
- `PIPS_AGENT_TIMESERIES_PATH` endpoint for accessing the PIPSTimeSeriesAgent
- `KEYCLOAK_REALM_PATH` Keycloak path for the specific realm
- `CLIENT_ID` ID of the client in Keycloak being used for authentication

Create a `secrets` folder in the same directory as this README, create the following files and folders in this folder:
- `client_secrets.txt` file containing the secrets of the client in Keycloak being used for authentication
- `username.txt` username
- `password.txt` password
- `client_certificates` folder
    - `client_cert.p12`
    - `client_cert_password.txt`

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
This route submits the credentials, Client ID and Client secrets to Keycloak for authentication, receive back an access token if authenticated, submit the token to the PIPS TimeSeries Agent for authorization and if authorized, receive back a JSON Object containing the response (e.g. timeseries data or unauthorized message or invalid token message or invalid credentials message). The request has the following format:
```
curl -X GET http://localhost:1080/pips-request-agent/retrieve?source=<source>&num=<number of items to retrieve>&client_cert_auth=<true or false>
```
- `<source>` The source from which to retrieve the data from, the source refers to the schema in which the tables are located under. Replace `<schema>` with the appropriate value.
- `<integer>` The number of latest readings to retrieve. Replace `<integer>` with the desired number.
- `<true or false>` Whether client certificate authentication is required or not. Please make sure to set up the necessary directory and files as described under the section [Set up](#set-up).