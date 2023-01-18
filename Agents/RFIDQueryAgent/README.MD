# RFID Query Agent

This agent is designed to query for the latest status of the RFID tags (In/Out) and determine whether the tagged object has been "Out" for longer
than expected. If the tagged object has been "Out" for longer than expected, An email will be sent to the relevant personnel via the [EmailSender class in the JPS Base Lib](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/email/EmailSender.java). The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) from the JPS_BASE_LIB to interact with both the KG and database.

## Usage 
This part of the README describes the usage of the agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client which maintains both instances in a knowledge graph and a Postgres database to store the data, these will be required to be set-up before.  

The [next section](#requirements) will explain the requirements to run the agent.
### Requirements
1) It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL. This can be either in the form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

2) It is required to have the [Email Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/EmailAgent) set up beforehand.

3) It is required to have some timeseries data that contains the status of the RFID tags (In/Out) already instantiated in the knowledge graph. One such agent that currently does that is the [RFID Update Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RFIDUpdateAgent).

### Property file
For running the agent, one property file is required:
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.

#### Time-series client properties
The time-series client properties file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

### Building the RFID Query Agent
The RFID Query Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).


Modify `client.properties` in the `config` folder accordingly. 

Modify the `Dockerfile` found in the same directory as this README. Change `EMAIL_AGENT_URL` according to where your Email Agent instance is located at.


To build and start the agent, open up the command prompt in the same directory as this `README`, run
```
docker-compose up -d
```
The agent is reachable at "rfid-query-agent/retrieve" on localhost port 1021.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1021/rfid-query-agent/retrieve with a correct JSON Object.
Follow the request shown below.
```
POST http://localhost:1021/rfid-query-agent/retrieve
Content-Type: application/json
{dataIRIs":"iri_1,iri_2,iri_3","hours":"10","timeSeriesClientProperties":"CLIENT_PROPERTIES"}
```
The JSONObject should contain the data IRIs of the RFID tags status, each IRI should be separated by a "," (see example below), the number of hours that the tagged object is allowed to be "Out" for and the environment variable pointing to where the `client.properties` is located at(The `Dockerfile` automatically does this for you).

In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"dataIRIs\":\"https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0,https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009726_status_9fdfa988-46c3-4402-bbcc-3d712234af6d\",\"hours\":\"1\",\"timeSeriesClientProperties\":\"CLIENT_PROPERTIES\"}" http://localhost:1021/rfid-query-agent/retrieve
```
If the agent runs successfully, you should see a returned JSON Object depending on the following scenarios:

1) If tagged object is "In".
```
{"message":"POST request has been sent successfully.","Result":["Input agent object initialized.","Time series client object initialized.","Queried for latest RFID tag status and checked timestamp threshold."]}
```

2) If tagged object is "Out" but within the allowed time period.
```
{"message":"POST request has been sent successfully.","Result":["Input agent object initialized.","Time series client object initialized.","Queried for latest RFID tag status and checked timestamp threshold."]}
```

3) If tagged object is "Out" for longer than allowed.
```
{"message":"POST request has been sent successfully.","Result":["Input agent object initialized.","Time series client object initialized.","Queried for latest RFID tag status and checked timestamp threshold.","Alert Email sent for <data IRI>"]}
```
