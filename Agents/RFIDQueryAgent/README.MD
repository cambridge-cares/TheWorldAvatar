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

4) If the tagged object is a chemical container that contain some chemicals, the tag, chemical container and the chemical it contains should be instantiated in the knowledge graph based on [OntoDevice](pending), [OntoLab](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontolab) and [OntoSpecies](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontospecies). An example of the instance can be found below:
```
<http://www.theworldavatar.com/kb/ontodevice/RFIDSensor_01> a ontodevice:RFIDSensor;
     ontodevice:observes <http://www.theworldavatar.com/kb/ontodevice/tag_01_status> .

<http://www.theworldavatar.com/kb/ontodevice/tag_01_status> a ontodevice:InOutStatus ;
   ontodevice:isPropertyOf <http://www.theworldavatar.com/kb/ontodevice/tag_01> ;
   ontodevice:isObservedBy <http://www.theworldavatar.com/kb/ontodevice/RFIDSensor_01> ;
   ontodevice:hasQualitativeValue <https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0> .

<https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0> a ontodevice:QualitativeValue .

<http://www.theworldavatar.com/kb/ontodevice/tag_01> a ontodevice:PassiveRFIDTag ;
   ontodevice:hasProperty <http://www.theworldavatar.com/kb/ontodevice/tag_01_status> ;
   ontodevice:isAttachedTo <http://www.theworldavatar.com/kb/ontolab/Bottle_01> .

<http://www.theworldavatar.com/kb/ontolab/Bottle_01> a ontolab:ChemicalContainer ;
   ontolab:isFilledWith <http://www.theworldavatar.com/kb/ontolab/Chemical_01> .

<http://www.theworldavatar.com/kb/ontolab/Chemical_01> a ontolab:Chemical ;
   ontolab:fills <http://www.theworldavatar.com/kb/ontolab/Bottle_01> ;
   ontoCAPE_CPS_Behavior:refersToMaterial <http://www.theworldavatar.com/kb/ontolab/Material_01> .

<http://www.theworldavatar.com/kb/ontolab/Material_01> a ontoCAPE_CPS_Behavior:Material ;
   ontoCAPE_Material:thermodynamicBehavior <http://www.theworldavatar.com/kb/ontolab/SolidPhase_01> .

<http://www.theworldavatar.com/kb/ontolab/SolidPhase_01> a ontoCAPE_Phase_System:SinglePhase ;
   ontoCAPE_System:isComposedOfSubsystem <http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01> .

<http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01> a ontoCAPE_Phase_System:PhaseComponent ;
   ontoCAPE_Phase_System:representsOccurenceOf <http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060> .

<http://www.theworldavatar.com/kb/ontospecies/Species_0a1489ff-c315-4607-93fc-b70b633bf060> a ontospecies:Species ;
   ontospecies:hasMolecularFormula <http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060> .

<http://www.theworldavatar.com/kb/ontospecies/MolecularFormula_0a1489ff-c315-4607-93fc-b70b633bf060> a ontospecies:MolecularFormula ;
   ontokin:hasElementNumber <http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br> .

<http://www.theworldavatar.com/kb/ontospecies/ElementNumber_0a1489ff-c315-4607-93fc-b70b633bf060_Br> a ontokin:ElementNumber ;
   ontokin:hasNumberOfElement 3 .
```

### Property file
For running the agent, one or two property files is required depending on whether your tagged object is a chemical container:
- One [property file for the time-series client](#time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for chemical species](#species-properties) defining how to access the sparql endpoints that contains the chemical species information.

#### Time-series client properties
The time-series client properties file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `client.properties` in the `config` folder.

#### Species properties
The species properties file needs to contain all endpoints to access the SPARQL endpoint of the knowledge graph that contains the chemical species information. It should contain the following keys:
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph

More information can be found in the example property file `species.properties` in the `config` folder.

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

If the tagged object is a chemical container that contains some chemical, Modify `species.properties` in the `config` folder accordingly. 

Modify the `Dockerfile` found in the same directory as this README. Change `EMAIL_AGENT_URL` according to where your Email Agent instance is located at.


To build and start the agent, open up the command prompt in the same directory as this `README`, run
```
docker-compose up -d
```
The agent is reachable at "rfid-query-agent/retrieve" on localhost port 1021.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1021/rfid-query-agent/retrieve with a correct JSON Object.
There are two formats for the requests:

If the tagged object does not contain a chemical:
```
POST http://localhost:1021/rfid-query-agent/retrieve
Content-Type: application/json
{dataIRIs":"iri_1,iri_2,iri_3","hours":"10","timeSeriesClientProperties":"CLIENT_PROPERTIES","containSpecies":"false"}
```
The JSONObject should contain the data IRIs of the RFID tags status, each IRI should be separated by a "," (see example below), the number of hours that the tagged object is allowed to be "Out" for, the environment variable pointing to where the `client.properties` is located at (Default should be "CLIENT_PROPERTIES" as the `Dockerfile` automatically does this for you) and whether the tagged object contains a chemical.

In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"dataIRIs\":\"https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0,https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009726_status_9fdfa988-46c3-4402-bbcc-3d712234af6d\",\"hours\":\"1\",\"timeSeriesClientProperties\":\"CLIENT_PROPERTIES\",\"containSpecies\":\"false\"}" http://localhost:1021/rfid-query-agent/retrieve
```

If the tagged object contains a chemical:
```
POST http://localhost:1021/rfid-query-agent/retrieve
Content-Type: application/json
{dataIRIs":"iri_1,iri_2,iri_3","hours":"10","timeSeriesClientProperties":"CLIENT_PROPERTIES","containSpecies":"true","speciesProperties":"SPECIES_PROPERTIES"}
```
The JSONObject should contain the data IRIs of the RFID tags status, each IRI should be separated by a "," (see example below), the number of hours that the tagged object is allowed to be "Out" for, the environment variable pointing to where the `client.properties` is located at (Default should be "CLIENT_PROPERTIES" as the `Dockerfile` automatically does this for you), whether the tagged object contains a chemical and the environment variable pointing to where the `species.properties` is located at (Default should be "SPECIES_PROPERTIES" as the `Dockerfile` automatically does this for you). 

In curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"dataIRIs\":\"https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0,https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009726_status_9fdfa988-46c3-4402-bbcc-3d712234af6d\",\"hours\":\"1\",\"timeSeriesClientProperties\":\"CLIENT_PROPERTIES\",\"containSpecies\":\"true\",\"speciesProperties\":\"SPECIES_PROPERTIES\"}" http://localhost:1021/rfid-query-agent/retrieve
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
