# RFID Query Agent

This agent is able to execute the following routes:
1) Check route. Query for the latest status of the RFID tags (In/Out) and determine whether the tagged object has been "Out" for longer
than allowed. If the tagged object has been "Out" for longer than allowed, an email will be sent to the relevant personnel via the [EmailSender class in the JPS Base Lib](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/email/EmailSender.java). The agent is able to intuitively determine whether the tagged object is a chemical container that contains a chemical species and retrieve the information of the chemical species if possible. More information can be found at the [Check route](#51-check-route) section.

2) Retrieve data route. Retrieve information relating to a tagged object IRI and return them in the form of a JSONObject. The agent is able to intuitively determine whether the tagged object is a chemical container that contains a chemical species and retrieve the information of the chemical species if possible. More information can be found at the [Retrieve data route](#52-retrieve-data-route) section.

3) Send notification route. Upon receiving the latest status of a tag, the agent will query for all the meta data relevant to the tag and it's tagged object, send out an email containing these information to the relevant personnels via the [EmailSender class in the JPS Base Lib](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/email/EmailSender.java). More information can be found at the [Send notification route](#53-send-notification-route) section.

The agent uses the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries) and [remote store client](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/query/RemoteStoreClient.java) from the JPS_BASE_LIB to interact with both the KG and database.


## 1. Usage 
This part of the README describes the usage of the agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. Since it uses the time-series client and remote store client to interact with the KG and database, the KG and database will be required to be set-up beforehand.  

The [next section](#2-requirements) will explain the requirements to run the agent.

## 2. Requirements
1) It is required to have access to a knowledge graph SPARQL endpoint and Postgres database. These can run on the same machine or need to be accessible from the host machine via a fixed URL. This can be either in the form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store or Postgres database.

2) It is required to have the [Email Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/EmailAgent) set up beforehand.

3) It is required to have some timeseries data that contains the status of the RFID tags (In/Out) already instantiated in the knowledge graph via the [time-series client](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/JPS_BASE_LIB/src/main/java/uk/ac/cam/cares/jps/base/timeseries).

4) If the tagged object is a chemical container that contain some chemicals; the tag, chemical container, chemical it contains, chemical species label and the species's GHS Hazard Statements(if the species does have GHS Hazard Statements) should be instantiated in the knowledge graph based on several ontologies: [OntoDevice](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontodevice), [OntoLab](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontolab), [OntoSpecies](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontospecies), [OntoCAPE](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontocape), [OntoReaction](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontoreaction) . An example of the instance can be found below:
```
<http://www.theworldavatar.com/kb/ontodevice/RFIDSensor_01> a ontodevice:RFIDSensor;
     ontodevice:observes <https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0> .

<https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0> a ontodevice:InOutState ;
     ontotimeseries:hasTimeSeries <https://www.theworldavatar.com/kg/ontotimeseries/Timeseries_d2929ba8-84f5-4fe0-94b2-850c4b40e10a> .

<http://www.theworldavatar.com/kb/ontodevice/tag_01> a ontodevice:PassiveRFIDTag ;
   saref:hasState <https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0> ;
   ontodevice:isAttachedTo <http://www.theworldavatar.com/kb/ontolab/Bottle_01> .

<http://www.theworldavatar.com/kb/ontolab/Bottle_01> a ontolab:ChemicalContainer ;
   rdfs:label "chemical container 01" ;
   ontolab:isFilledWith <http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01> .

<http://www.theworldavatar.com/kb/ontolab/ChemicalAmount_01> a ontolab:ChemicalAmount ;
   ontoCAPE_CPS_Behavior:refersToMaterial <http://www.theworldavatar.com/kb/ontolab/Chemical_01> .

<http://www.theworldavatar.com/kb/ontolab/Chemical_01> a ontoreaction:Chemical ;
   ontoCAPE_Material:thermodynamicBehavior <http://www.theworldavatar.com/kb/ontolab/Phase_01> .

<http://www.theworldavatar.com/kb/ontolab/Phase_01> a ontoCAPE_Phase_System:SinglePhase ;
   ontoCAPE_System:isComposedOfSubsystem <http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01> .

<http://www.theworldavatar.com/kb/ontolab/PhaseComponent_01> a ontoCAPE_Phase_System:PhaseComponent ;
   ontoCAPE_Phase_System:representsOccurenceOf <http://www.theworldavatar.com/kg/ontospecies/Species_92490f04-fc41-457a-baff-14480eae4d3e> .

<http://www.theworldavatar.com/kg/ontospecies/Species_92490f04-fc41-457a-baff-14480eae4d3e> a ontospecies:Species ;
   rdfs:label "KNO3" ;
   ontospecies:hasGHSHazardStatements <http://www.theworldavatar.com/kg/ontospecies/GHSHazardStatements_61ecccfe-2f04-44ac-a209-d6dc23e2dbe2> .

<http://www.theworldavatar.com/kg/ontospecies/GHSHazardStatements_61ecccfe-2f04-44ac-a209-d6dc23e2dbe2> a ontospecies:GHSHazardStatements ;
   rdfs:comment	"Causes skin irritation [Warning Skin corrosion/irritation]" ;
   rdfs:label	"H315" .

```

## 3. Property file
For running the agent, one or two property files is required depending on whether your tagged object is a chemical container:
- One [property file for the time-series client](#31-time-series-client-properties) defining how to access the database and SPARQL endpoint.
- One [property file for chemical species](#32-species-properties) defining how to access the SPARQL endpoints that contains the chemical species information.

### 3.1. Time-series client properties
The time-series client properties file needs to contain all credentials and endpoints to access the SPARQL endpoint of the knowledge graph and the Postgres database. It should contain the following keys:
- `db.url` the [JDBC URL](https://www.postgresql.org/docs/7.4/jdbc-use.html) for the Postgres database
- `db.user` the username to access the Postgres database
- `db.password` the password to access the Postgres database
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username needed to access the SPARQL endpoint, can be left empty if no credentials are required
- `sparql.password` the password needed to access the SPARQL endpoint, can be left empty if no credentials are required

More information can be found in the example property file `client.properties` in the `config` folder.

### 3.2. Species properties
The species properties file needs to contain all endpoints to access the SPARQL endpoint of the knowledge graph that contains the chemical species information. It should contain the following keys:
- `sparql.query.endpoint` the SPARQL endpoint to query the knowledge graph
- `sparql.update.endpoint` the SPARQL endpoint to update the knowledge graph
- `sparql.username` the username needed to access the SPARQL endpoint, can be left empty if no credentials are required
- `sparql.password` the password needed to access the SPARQL endpoint, can be left empty if no credentials are required

More information can be found in the example property file `species.properties` in the `config` folder.

## 4. Building the RFID Query Agent
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

The agent can be deployed via two ways, a standalone or as part of the stack.


### 4.1. Standalone
Modify the `Dockerfile` found in the same directory as this README. Change `EMAIL_AGENT_URL` according to where your Email Agent instance is located at. 

Open `docker-compose.yml` and under the volumes section, modify source according to where the `config` folder is located at. 

To build and run the agent as a standalone, open up the command prompt in the same directory as this `README`, run
```
docker-compose up -d
```
The agent is reachable at "rfid-query-agent/" on localhost port 1021.

### 4.2. Stack deployment
To build and run the agent in the stack, open up the command prompt in the same directory as this `README`, run
```
docker-compose build
```
Open the `rfid-query-agent.json` under `stack-manager-input-config`. Under the Env section, modify EMAIL_AGENT_URL according to where your Email Agent instance is located at. Under the Mounts section, modify Source according to where the `config` folder is located at.

Copy `rfid-query-agent.json` to the services folder under your stack-manager directory (By default it should be TheWorldAvatar/Deploy/stacks/dynamic/stack-manager/inputs/config/services/) and start up the stack.

The agent is reachable at "rfid-query-agent/" on localhost port 3838.

## 5. Run the agent
The agent can execute three different routes as described below:

### 5.1. Check route
To run the agent, a POST request must be sent to `http://IPaddress:portNumber/rfid-query-agent/check` with a correct JSON Object.

The request has this general format:
```
POST http://<IP address>:<portNumber>/rfid-query-agent/check
Content-Type: application/json
{
   dataIRIs":"<iri_1>,<iri_2>,<iri_3>",
   "hours":"<number of hours the tagged object is allowed to be "out" for>",
   "timeSeriesClientProperties":"CLIENT_PROPERTIES",
   "speciesProperties":"SPECIES_PROPERTIES"
}
```
The JSONObject should contain the data IRIs of the RFID tags status where each IRI is separated by a "," , the number of hours that the tagged objects is allowed to be "Out" for, the environment variable pointing to where the `client.properties` is located at (Default should be "CLIENT_PROPERTIES" as the `Dockerfile` automatically does this for you) and the environment variable pointing to where the `species.properties` is located at (Default should be "SPECIES_PROPERTIES" as the `Dockerfile` automatically does this for you). 

An example of the request in curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"dataIRIs\":\"https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009727_status_89c6626e-4eae-4e75-8d7e-502d5cf904b0,https://www.theworldavatar.com/kg/ontotimeseries/rfid_tag_00000000000000A000009726_status_9fdfa988-46c3-4402-bbcc-3d712234af6d\",\"hours\":\"6\",\"timeSeriesClientProperties\":\"CLIENT_PROPERTIES\",\"speciesProperties\":\"SPECIES_PROPERTIES\"}" http://<IP address>:<portNumber>/rfid-query-agent/check
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
{"message":"POST request has been sent successfully.","Result":["Input agent object initialized.","Time series client object initialized.","Queried for latest RFID tag status and checked timestamp threshold.","Alert Email sent for <iri_1>","Alert Email sent for <iri_2>"]}
```

If the tagged object is "Out" for longer than allowed, the email content sent will be different depending on the scenarios mentioned below. Some examples of the email content can be found below:

1) If tagged object is not a chemical container.
```
The tagged object has been removed for longer than 1 hours since 2023-03-08 07:03:19 AM UTC. The object has the following label asset 02 and tag ID 00000000000000A000009726 . 
```
2) If tagged object is a chemical container containing a chemical with GHS Hazard Statements.
```
tl;dr Container with KNO3 has been removed for longer than 1 hour.

The chemical container with the following information has been removed for longer than 1 hours since 2023-03-08 07:03:19 AM UTC. The container has the label chemical container 01 and tag ID 00000000000000A000009727. The container is storing a chemical with the label KNO3 which has the following GHS hazard statements.
H315: Causes skin irritation [Warning Skin corrosion/irritation]
H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
H335: May cause respiratory irritation [Warning Specific target organ toxicity, single exposure; Respiratory tract irritation]
H272: May intensify fire; oxidizer [Danger Oxidizing liquids; Oxidizing solids]
```
3) If tagged object is a chemical container containing a chemical without GHSHazardStatements.
```
tl;dr Container with Hydrogen has been removed for longer than 1 hour.

The chemical container with the following information has been removed for longer than 1 hours since 2023-03-08 07:03:19 AM UTC. The container has the label chemical container 01 and tag ID 00000000000000A000009727.The container is storing a chemical with the label Hydrogen.
```

### 5.2. Retrieve data route
To run the agent, a GET request must be sent to `http://IPaddress:portNumber/rfid-query-agent/retrieveData`.

The request has this general format:
```
GET http://<IP address>:<portNumber>/rfid-query-agent/retrieveData?timeSeriesClientProperties=CLIENT_PROPERTIES&speciesProperties=SPECIES_PROPERTIES&taggedObjectIRI=<tagged_object_iri>
```
The request should contain the tagged object IRI, the environment variable pointing to where the `client.properties` is located at (Default should be "CLIENT_PROPERTIES" as the `Dockerfile` automatically does this for you), and the environment variable pointing to where the `species.properties` is located at (Default should be "SPECIES_PROPERTIES" as the `Dockerfile` automatically does this for you). 

An example of the request in curl syntax
```
curl -X GET http://<IP address>:<portNumber>/rfid-query-agent/retrieveData?timeSeriesClientProperties=CLIENT_PROPERTIES&speciesProperties=SPECIES_PROPERTIES&taggedObjectIRI=http://www.theworldavatar.com/kg/lab/ChemicalContainer_2ac36998-e5cd-4021-b638-7776c394250c
```

The agent will attempt to query for the following information:
1) tagged object label
2) latest In/Out Status
3) Chemical species information such as (Molecular weight, molecular formula, GHS hazard statements etc)

A successful request will return a JSONObject similar to the one below:
```
{
    "Bottle Label": "explosive precursor container (potassium nitrate)",
    "Latest In/Out Status": "Out since 2023-05-24 06:36:30 AM UTC",
    "Molecular Weight": "101.103 g / mol",
    "GHS Hazard Statements": {
        "H319": "Causes serious eye irritation",
        "H272": "May intensify fire; oxidizer",
        "H335": "May cause respiratory irritation",
        "H315": "Causes skin irritation"
    },
    "Molecular Formula": "KNO3"
}
```

### 5.3. Send notification route
This route is designed to work with the [RFID Agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RFIDAgent). To run the agent, a POST request must be sent to `http://IPaddress:portNumber/rfid-query-agent/sendNotification` with a correct JSON Object.

The request has this general format:
```
POST http://<IP address>:<portNumber>/rfid-query-agent/sendNotification
Content-Type: application/json
{
    "dataIRI":"<data_iri>",
    "latestStatus":"<a string describing the latest status and corresponding timestamp>",
    "timeSeriesClientProperties":"CLIENT_PROPERTIES",
    "speciesProperties":"SPECIES_PROPERTIES"
}
```

The JSONObject should contain the data IRI of the RFID tag status, the latest status and corresponding timestamp, the environment variable pointing to where the `client.properties` is located at (Default should be "CLIENT_PROPERTIES" as the `Dockerfile` automatically does this for you), and the environment variable pointing to where the `species.properties` is located at (Default should be "SPECIES_PROPERTIES" as the `Dockerfile` automatically does this for you). 

An example of the request in curl syntax
```
curl -X POST --header "Content-Type: application/json" -d "{\"dataIRI\":\"http://www.theworldavatar.com/kb/ontotimeseries/rfid_tag_00000000000000A000009726_status_614a81fc-cdd2-46b8-8f72-c1632c109acb\",\"latestStatus\":\"Out since 2023-05-24 06:36:30 AM SGT\",\"timeSeriesClientProperties\":\"CLIENT_PROPERTIES\",\"speciesProperties\":\"SPECIES_PROPERTIES\"}" http://<IP address>:<portNumber>/rfid-query-agent/sendNotification
```

The agent will attempt to query for the following information:
1) tagged object label
2) Chemical species information such as (label, GHS hazard statements etc)

A successful request will return a JSONObject similar to the one below:
```
{"Result":"Alert Email sent for http://www.theworldavatar.com/kb/ontotimeseries/rfid_tag_00000000000000A000009726_status_614a81fc-cdd2-46b8-8f72-c1632c109acb"}
```

The email content sent out will be similar to the one below:
```
tl;dr Container with KNO3 is currently Out since 2023-05-24 06:36:30 AM SGT.

The container has the label explosive precursor container (potassium nitrate) and tag ID 00000000000000A000009727. The container is storing a chemical with the label KNO3 which has the following GHS hazard statements.
H315: Causes skin irritation [Warning Skin corrosion/irritation]
H319: Causes serious eye irritation [Warning Serious eye damage/eye irritation]
H335: May cause respiratory irritation [Warning Specific target organ toxicity, single exposure; Respiratory tract irritation]
H272: May intensify fire; oxidizer [Danger Oxidizing liquids; Oxidizing solids]
```