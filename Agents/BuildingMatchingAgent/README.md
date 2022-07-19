#Building Matching Agent

###Introduction
The building matching agent is used to link a building instantiated in OntoBuiltEnv to its corresponding instance instantiated in OntoCityGML. The link is created by using UPRNs as the identifiers.

###Input
The agent accepts 3 input parameters in a JSONObject format with the keys: "ocgml", "obe" and "prefixIRI" where,<br/>
"ocgml" is the endpoint containing buildings instantiated in OntoCityGML, "obe" is the endpoint containing buildings instantiated in OntoBuiltEnv and "prefixIRI" 
is the complete IRI of the OntoCityGML namespace. 

Example input: <br/>
{<br/>
&emsp; "ocgml": "http:<span></span>//host.docker.internal:48080/kings-lynn", <br/>
&emsp; "obe": "http:<span></span>//host.docker.internal:48080/ontobuiltenv", <br/>
&emsp; "prefixIRI": "http:<span></span>//127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/" <br/>
}

###Output
The relationship/link between the OntoBuiltEnv and OntoCityGML buildings is written to the knowledge graph in the OntoBuilEnv namespace. <br/>
The newly added triples have the form <br/>
1. Subject - OntoBuiltEnv building instance 
2. Predicate - https:<span></span>//www.<span></span>theworldavatar.com/kg/ontobuiltenv/hasOntoCityGMLRepresentation
3. Object - OntoCityGML building instance


###Requirements
The following prerequisites are required before running the Building Matching Agent
1. 2 SPARQL endpoints containing buildings instantiated in OntoCityGML and OntoBuiltEnv respectively
2. On the OntoCityGML namespace, run the [Thematic Surface Discovery Agent](https://github.com/cambridge-cares/CitiesKG/blob/develop/agents/README.md#thematic-surface-discovery-agent-user-guide) in the restructure mode followed 
by the [UPRN agent](https://github.com/cambridge-cares/CitiesKG/blob/uprn-agent/agents/src/main/resources/uprn_HTTPRequest.http). <br/>
This step is used to link a building in OntoCityGML to its corresponding UPRN(s).
3. Access Agent to be deployed locally via Docker to access your local blazegraph. 
   1. Set up a Docker environment as described in the [TWA Wiki - Docker: Environment](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment) page. 
   2. In the TWA repository clone, locate the properties file in _TheWorldAvatar/JPS_ACCESS_AGENT/access-agent/src/main/resources/accessagent.properties_, and replace `http://www.theworldavatar.com/blazegraph/namespace/ontokgrouter/sparql` with `http://host.docker.internal:9999/blazegraph/namespace/ontokgrouter/sparql`.
   If using a different port for Blazegraph, replace `9999` with your port number. 
   3. In your local Blazegraph, create a new namespace called 'ontokgrouter'. 
   4. For each endpoint to be accessed, 5 triples need to be added to the 'ontokgrouter' namespace. An example SPARQL update is shown, which inserts the triples required to access a namespace 'test' in a local Blazegraph on port 9999. Replace all occurrences of `test` with the name of the namespace and `9999` with your port number, and execute the SPARQL update.
   ```
   INSERT DATA {
   <http://host.docker.internal:9999/blazegraph/ontokgrouter/test>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasQueryEndpoint>	"http://host.docker.internal:9999/blazegraph/namespace/test/sparql".
   <http://host.docker.internal:9999/blazegraph/ontokgrouter/test>	<http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#hasUpdateEndpoint> "http://host.docker.internal:9999/blazegraph/namespace/test/sparql".
   <http://host.docker.internal:9999/blazegraph/ontokgrouter/test>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.theworldavatar.com/ontology/ontokgrouter/OntoKGRouter.owl#TargetResource>.
   <http://host.docker.internal:9999/blazegraph/ontokgrouter/test>	<http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/2002/07/owl#NamedIndividual>.
   <http://host.docker.internal:9999/blazegraph/ontokgrouter/test>	<http://www.w3.org/2000/01/rdf-schema#label> "test".
   }
   ```
   5. In this repository, locate the agents properties file in _agents/src/main/resources/config.properties_, and set the uri.route to `http://localhost:48080/access-agent/access/test`, replacing `test` with the name of the namespace to connect to. 
   6. Build and deploy the AccessAgent as described in the README of the [JPS_ACCESS_AGENT directory](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_ACCESS_AGENT) in the TWA repository.

###Building the <i>Building Matching Agent</i>
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
  "ocgml": "http://host.docker.internal:48080/kings-lynn",
  "obe": "http://host.docker.internal:48080/ontobuiltenv",
  "prefixIRI":"http://127.0.0.1:9999/blazegraph/namespace/kings-lynn/sparql/"
}
```