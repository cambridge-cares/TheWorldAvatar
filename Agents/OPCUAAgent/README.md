# OPCUA Agent
This agent is designed to read tags from csv files, connect to an OPCUA server, read the values of the tags and insert them as timeseries into a PostgreSQL database. At the moment, the agent is only able to handle basic username and password authentication. If the OPCUA server have any other security or authentication features enabled, the agent will need to be modified.

# Set up and deployment
## Set up
1) Create csv files containing the tags to be read and insert them into the `filtered_tags` folder. Examples of the csv files can be found in the `filtered_tags` folder.

2) In `docker-compose.yml`, under the volumes section:
-  Replace `<path on host>` with the actual location of the `config` and `filtered_tags` folders, this will mount the folders onto the container and any changes to the files in these folders will be reflected in the container automatically.

3) In `./OPCUAAgent/config/postgres_conf.properties`, modify the parameters accordingly:
- `dbname` name of the database
- `user` username
- `password` password
- `host` IP Address of the machine hosting the database
- `port` port in which postgreSQL is listening on

4) In `./OPCUAAgent/config/opcua_conf.properties`, modify the parameters accordingly:
- `opcua_server_url` endpoint url of the OPC-UA server
- `user` username
- `password` password
- `interval` interval for data collection

## Deployment
Open a terminal in the same directory as this README and run the following to spin up the container:
```
docker compose up -d
```
The agent will start running automatically at the interval defined in `./OPCUAAgent/config/opcua_conf.properties`. To change the interval, modify the interval parameter in `./OPCUAAgent/config/opcua_conf.properties`.

## Tests
Unit and integration tests have been written for this agent and these tests can be executed by opening a terminal in the same directory as this README and running the following:
```
docker compose -p test_opcua_agent -f "docker-compose-test.yml" up -d
```
This will start up the test containers which at the moment only include a container containing the tests and a PostgreSQL container. Attempts at creating an OPCUA server in a docker container for the tests have not been successful but the agent have been tested against the [KEPServerEX](https://www.ptc.com/en/products/kepware/kepserverex).

### TO-DO
Create an OPCUA server in a docker container for running the tests.