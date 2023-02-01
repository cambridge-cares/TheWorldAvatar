# District Heating Agent

This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the district heating network located in a midsize town in Germany. Its purpose is to instantiate instances of the district heating network. The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries)
from the JPS_BASE_LIB to interact with both the KG and database.

## Usage 
This part of the README describes the usage of the heating network agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client which maintains both instances in a knowledge graph and a Postgres database to store the data, these will be required to be set-up before.  

The next section explains the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

This agent requires a running instance of the Access Agent. Once this has been spun up, appropriate routing information needs to be uploaded, mapping the `districtheating` label (or equivalent) to the query and update endpoints of the relevant blazegraph namespace (examples see below). Please consult the Access Agent documentation on how to do this.

#### Time-series client properties
The time-series client property file is required and it needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder. Please take note the namespace set in the db.url has to be the same as the one set in the PostgreSQL Databases. 

#### Routing setup for local Blazegraph
The query endpoint and update endpoint for routing information has to be set as, e.g.
```
http://blazegraph-access-agent:8080/blazegraph/namespace/ontoheatnet/sparql
```
If running within a stack, use e.g.
```
http://<STACK NAME>-blazegraph:8080/blazegraph/namespace/districtheating/sparql
```

#### Mapping files
The mapping files define how the heating network data is connected
to the knowledge graph (KG). Specifically, each JSON key in the heating network data represents a specific measure that needs to be represented by an IRI, if it should be saved in the database.

Furthermore, measures can be grouped into one time-series (will result in one time-series instance per group in the KG).
This should be done so that all measures in one group are recorded at the same time interval, and so they come from 
the same readings.

#### Building the Heating Network Agent

Heating Network Agent is set up to use the Maven repository. Credentials in single-word text files located like this are required:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

#### Stack Deployment

If you want to spin up this agent as part of a stack, instead of `docker-compose up -d`, do the following:
- Copy the contents of `config/client.properties_stack` into `config/client.properties`, inserting the name of your stack.
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into the `inputs/config` folder of the stack manager, adjusting the absolute path of the bind mounts as required.
- Start the stack manager as usual. This should start the container.

This agent is reachable at "heatnetwork-agent/performheatupdate" on localhost port 1080.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1080/district-heating-agent/performheatupdate with a JSON Object. In this agent, it contains the URL for sparql endpoint. Follow the sample request shown below.
```

POST http://localhost:1080/district-heating-agent/performheatupdate
Content-Type: application/json
{"endpoint":"http://host.docker.internal:48888/ontoheatnet"}
```

In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{
\"endpoint\":\"http://host.docker.internal:48888/ontoheatnet\"}" http://localhost:1080/district-heating-agent/performheatupdate
```

If running the agent within a stack:
```
curl -X POST --header "Content-Type: application/json" -d "{\"endpoint\":\"http://<STACK NAME>-access-agent:8080/districtheating\"}" http://localhost:3838/district-heating-agent/performheatupdate
```

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":["Static data has been updated.","Timeseries client initialized.","Timeseries Data has been updated."]}
```
