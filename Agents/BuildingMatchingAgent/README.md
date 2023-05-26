# Building Matching Agent

### Introduction
The Building Matching Agent is used to link a building instantiated in [OntoBuiltEnv](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontobuiltenv/OntoBuiltEnv.owl) to its corresponding instance instantiated in [OntoCityGML](https://github.com/cambridge-cares/TheWorldAvatar/blob/main/JPS_Ontology/ontology/ontocitygml/OntoCityGML.owl). The link is created by using UPRNs as the identifiers.

### Input
The agent accepts 3 input parameters in a JSONObject format with the keys: "ocgml", "obe" and "prefixIRI" where, "ocgml" is the endpoint containing buildings instantiated in OntoCityGML, "obe" is the endpoint containing buildings instantiated in OntoBuiltEnv and "prefixIRI" is the complete IRI of the OntoCityGML namespace (i.e. the IRI string used to prefix all OntoCityGml instances at creation). 

Example input:
```
{
"ocgml": "http://localhost:9999/blazegraph/namespace/kings-lynn/sparql", 
"obe": "http://localhost:9999/blazegraph/namespace/ontobuiltenv/sparql",
"prefixIRI": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/" 
}
```

### Output
The relationship/link between the OntoBuiltEnv and OntoCityGML buildings is written to the knowledge graph in the OntoBuilEnv namespace. <br/>
The newly added triples have the form <br/>
1. Subject - OntoBuiltEnv building instance 
2. Predicate - https:<span></span>//www<span></span>.theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation
3. Object - OntoCityGML building instance


### Requirements
The following prerequisites are required before running the Building Matching Agent
1. 2 SPARQL endpoints containing buildings instantiated in OntoCityGML and OntoBuiltEnv respectively
2. On the OntoCityGML namespace, run the [Thematic Surface Discovery Agent](https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/README.md#thematic-surface-discovery-agent-user-guide) in the restructure mode followed 
by the [UPRN agent](https://github.com/cambridge-cares/CitiesKG/tree/develop/agents#uprn-agent-user-guide). These steps are required to link a building in OntoCityGML to its corresponding UPRN(s).


### Building the <i>Building Matching Agent</i>
1. The Building Matching Agent is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central). You'll need to provide your credentials in single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

2. To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

3. The agent is reachable at "BuildingMatchingAgent/match" on localhost port 58085.


### Running the agent
To run the agent, a PUT request must be sent to http://localhost:58085/BuildingMatchingAgent/match with a correct JSON Object as described in Input.
An example request is shown below.

```
PUT http://localhost:58085/BuildingMatchingAgent/match
Content-Type: application/json
{
  "ocgml": "http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql",
  "obe": "http://127.0.0.1:9999/blazegraph/namespace/ontobuiltenv/sparql",
  "prefixIRI":"http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
}
```
