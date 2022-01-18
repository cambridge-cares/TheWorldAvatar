# Derivation Asynchronous Example

## Namespaces
There are a few namespaces used in this example:
```
PREFIX OntoDerivation: <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
PREFIX derivationBase: <https://www.derivationasynexample.com/triplestore/repository/>
PREFIX example: <http://derivation_asyn_example#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX time: <http://www.w3.org/2006/time#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
```

## Purpose
This example demonstrates the usage of the asynchronous function enabled by `AsynAgent` and `DerivationClient`/`DerivationSparql` class from `jps-base-lib`. The example here is set to follow the same (similar) context as in the [DerivationExample](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/DerivationExample). The general structure of this example follows the [Example: Java agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Deploy/examples/java_agent).

## Chain of asynchronous derivations
This example contains a chain of asynchronous derivations:
1. Random Number Generation Derivation: a list of (specified amount of) random numbers generated within the upper and lower limits
2. Maximum Value Derivation: the maximum value in the generated list of random numbers
3. Minimum Value Derivation: the minimum value in the generated list of random numbers
4. Difference Derivation: difference between the maximum and minimum values

Each derivation has an asynchronous agent associated with it that keeps monitoring its status. Based on four `rdf:type` of status (`PendingUpdate`, `Requested`, `InProgress`, and `Finished`), asynchronous agent performs tasks organised by the asynchronous operation mode as part of `DerivationClient`. All instances within this chain of derivation depend on three pure inputs: UpperLimit, LowerLimit, and NumberOfPoints. Once the request for update derivations is fired, each derivation will be marked as `PendingUpdate`, and each asynchronous agent will work out when to update the derivation it monitors. The information propagates through the knowledge graph and finally the whole knowledge graph is up-to-date.

A more visual illustration can be found below:

<p align="center">
    <img src="https://lucid.app/publicSegments/view/2f4a9840-5d5a-4273-b4a2-5f71d98f5987/image.png" width="750"/>
</p>

### Pure inputs
The pure inputs of the first (upstream) derivation in the chain are instances of `UpperLimit` (20), `LowerLimit` (3), and `NumberOfPoints` (6), with the value in brackets as their own default value in this example. Upon initialisation, the following triples are created in blazegraph:
```
# for UpperLimit instance
<upperlimit> <rdf:type> <example:UpperLimit>
<upperlimit> <rdf:type> <owl:NamedIndividual>
<upperlimit> <example:hasValue> <value1>
<value1> <rdf:type> <example:SvalarValue>
<value1> <example:numericalValue> 20
<upperlimit> <time:hasTime> <timestamp1>
<timestamp1> <time:inTimePosition> <unixtime1>
<unixtime1> <time:numericPosition> 123

# for LowerLimit instance
<lowerlimit> <rdf:type> <example:LowerLimit>
<lowerlimit> <rdf:type> <owl:NamedIndividual>
<lowerlimit> <example:hasValue> <value2>
<value2> <rdf:type> <example:SvalarValue>
<value2> <example:numericalValue> 3
<lowerlimit> <time:hasTime> <timestamp2>
<timestamp2> <time:inTimePosition> <unixtime2>
<unixtime2> <time:numericPosition> 123

# for NumberOfPoints instance
<numofpoints> <rdf:type> <example:NumberOfPoints>
<numofpoints> <rdf:type> <owl:NamedIndividual>
<numofpoints> <example:hasValue> <value3>
<value3> <rdf:type> <example:SvalarValue>
<value3> <example:numericalValue> 6
<numofpoints> <time:hasTime> <timestamp3>
<timestamp3> <time:inTimePosition> <unixtime3>
<unixtime3> <time:numericPosition> 123
```
Note that the `value1` `value2` `value3` instances contain data used by the first derivation (Random Number Generation Derivation) that `OntoDerivation:isDerivedFrom` these three inputs. The RNGAgent is responsible for monitoring the derivation and will generate an instance of `example:ListOfRandomPoints`. The `timestamp1` `timestamp2` `timestamp3` are used to check whether the derivation is out-of-date compared to the inputs.


### Random Number Generation Derivation
The property instance:
```
<listOfRandomPoints> <rdf:type> <example:ListOfRandomPoints>
<listOfRandomPoints> <example:hasPoint> <point> # there are multiple similar triples that each refers to one point in the list
<point> <example:hasValue> <valueOfPoint>
<valueOfPoint> <example:numericalValue> 123
```
The derivation instance:
```
<derivation_of_rng> <OntoDerivation:isDerivedFrom> <upperlimit>
<derivation_of_rng> <OntoDerivation:isDerivedFrom> <lowerlimit>
<derivation_of_rng> <OntoDerivation:isDerivedFrom> <numofpoints>
<listOfRandomPoints> <OntoDerivation:belongsTo> <derivation_of_rng>
<derivation_of_rng> <OntoDerivation:isDerivedUsing> <RNGAgent>
<derivation_of_rng> <OntoDerivation:hasStatus> <rng_status>
```
The agent for this instance, `RNGAgent`, when it satisfies `<rng_status> <rdf:type> <OntoDerivation:Requested>`, retrieves the inputs of this derivation:
```json
{"agent_input": {"example:UpperLimit": "<upperlimit>", "example:LowerLimit": "<lowerlimit>", "example:NumberOfPoints": "<numofpoints>"}}
```
generates `<numofpoints>` random points bounded by the `<upperlimit>` and `<lowerlimit>`, and connects them with a new instance of `example:ListOfRandomPoints`, all triples are written to the knowledge graph, e.g.
```
<new_listOfRandomPoints> <rdf:type> <example:ListOfRandomPoints>
<new_listOfRandomPoints> <example:hasPoint> <new_point> # again, there are multiple similar triples that each refers to a new generated point in the list
<new_point> <example:hasValue> <new_valueOfPoint>
<new_valueOfPoint> <example:numericalValue> 123
```
the old instance `<listOfRandomPoints>` will be deleted, the new instance will be connected with derivations, e.g.
```
<new_listOfRandomPoints> <OntoDerivation:belongsTo> <derivation_of_rng>
<derivation_of_max> <OntoDerivation:isDerivedFrom> <new_listOfRandomPoints>
```

### Maximum value
The property instance:
```
<max> <rdf:type> <example:MaxValue>
<max> <example:hasValue> <valueOfMax> 
<valueOfMax> <example:numericalValue> 123
```
The derivation instance:
```
<derivation_of_max> <OntoDerivation:isDerivedFrom> <new_listOfRandomPoints>
<max> <OntoDerivation:belongsTo> <derivation_of_max>
<derivation_of_max> <OntoDerivation:isDerivedUsing> <MaxValueAgent>
<derivation_of_max> <OntoDerivation:hasStatus> <max_status>
```
The agent for this instance, `MaxValueAgent`, when it satisfies `<max_status> <rdf:type> <OntoDerivation:Requested>`, retrieves the inputs of this derivation:
```json
{"agent_input": {"example:ListOfRandomPoints": "<new_listOfRandomPoints>"}}
```
queries the maximum value from the list of `<new_point>`, and writes a new instance, e.g.
```
<new_max> <rdf:type> <example:MaxValue>
<new_max> <example:hasValue> <newMaxValue>
<newMaxValue> <example:numericalValue> 123
```
the old instance `<max>` will be deleted, the new instance will be connected with derivations, e.g.
```
<new_max> <OntoDerivation:belongsTo> <derivation_of_max>
<derivation_of_diff> <OntoDerivation:isDerivedFrom> <new_max>
```

### Minimum value
The property instance:
```
<min> <rdf:type> <example:MinValue>
<min> <example:hasValue> <valueOfMin> 
<valueOfMin> <example:numericalValue> 123
```
The derivation instance:
```
<derivation_of_min> <OntoDerivation:isDerivedFrom> <new_listOfRandomPoints>
<min> <OntoDerivation:belongsTo> <derivation_of_min>
<derivation_of_min> <OntoDerivation:isDerivedUsing> <MinValueAgent>
<derivation_of_min> <OntoDerivation:hasStatus> <min_status>
```
The agent for this instance, `MinValueAgent`, when it satisfies `<min_status> <rdf:type> <OntoDerivation:Requested>`, retrieves the inputs of this derivation:
```json
{"agent_input": {"example:ListOfRandomPoints": "<new_listOfRandomPoints>"}}
```
queries the minimum value from the list of `<new_point>`, and writes a new instance, e.g.
```
<new_min> <rdf:type> <example:MinValue>
<new_min> <example:hasValue> <newMinValue>
<newMinValue> <example:numericalValue> 123
```
the old instance `<min>` will be deleted, the new instance will be connected with derivations, e.g.
```
<new_min> <OntoDerivation:belongsTo> <derivation_of_min>
<derivation_of_diff> <OntoDerivation:isDerivedFrom> <new_min>
```

### Difference
The difference instance is also identical, except that it has the rdf:type `example:Difference`.
```
<diff> <rdf:type> <example:Difference>
<diff> <example:hasValue> <valueOfDiff> 
<valueOfDiff> <example:numericalValue> 123
```
The derivation instance contains two inputs - the maximum value and minimum value instances.
```
<derivation_of_diff> <OntoDerivation:isDerivedFrom> <new_max>
<derivation_of_diff> <OntoDerivation:isDerivedFrom> <new_min>
<diff> <OntoDerivation:belongsTo> <derivation_of_diff>
<derivation_of_diff> <OntoDerivation:isDerivedUsing> <DifferenceAgent>
<derivation_of_diff> <OntoDerivation:hasStatus> <diff_status>
```
Again, when it satisfies `<diff_status> <rdf:type> <OntoDerivation:Requested>`, `DifferenceAgent` retrieves the inputs of this derivation:
```json
{"agent_input": {"example:MaxValue": "<new_max>", "example:MinValue": "<new_min>"}}
```
It then queries the values using the given IRIs and calculate the difference between the values. The agent creates a new instance
```
<new_diff> <rdf:type> <example:Difference>
<new_diff> <example:hasValue> <newDiffValue>
<newDiffValue> <example:numericalValue> 123
```
the old instance `<diff>` will be deleted, as the new instance is not input of another other derivations, only one connection will be made with derivation instance, e.g.
```
<new_diff> <OntoDerivation:belongsTo> <derivation_of_diff>
```

## Docker setup
This example uses a docker stack with two containers:

1. derivationasynexample (containing the following servlets)
    - Those extend JPSAgent:
      - DerivationAsynExample/InitialiseInstances
      - DerivationAsynExample/UpdateDerivations
      - DerivationAsynExample/InputAgent
    - Those extend AsynAgent:
      - DerivationAsynExample/RNGAgent
      - DerivationAsynExample/MaxValueAgent
      - DerivationAsynExample/MinValueAgent
      - DerivationAsynExample/DifferenceAgent
    
2. blazegraph

The example is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files in the `credentials` folder
```
  credentials/
      repo_username.txt
      repo_password.txt
      blazegraph_password
```

`repo_username.txt` should contain your github username, and `repo_password.txt` your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

`blazegraph_password` will set the password for the blazegraph containers. For simplicity, you may just provide an empty file.

Next, you will also need to populate the file `DerivationAsynExample/src/main/resources/agents.properties` with the passwords you set for your blazegraph containers, i.e.
```
kg.password=[YOUR_BLAZEGRAPH_PASSWORD]
```
If `blazegraph_password` is left empty, then you should leave the `agents.properties` file unchanged.

By all means, you should leave the other fields unchanged.

To build and start the service, you need to spin up the stack using the docker-compose.yml file provided. Before composing the docker container, please make sure you have already set up the [Docker Environment](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment) and obtained access to [Docker Image Registry](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry) - the latter is required to pull the blazegraph image. In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The default port numbers for the containers are (accesing from host machine, i.e. localhost):
| container | port number |
| --- | --- |
| derivationexample | 58085 |
| blazegraph | 8889 |

Once the docker stack is up and running, you should be able to access the blazegraph container at (http://localhost:8889/blazegraph).

## Test
To test the docker set up, there are integration tests written for this example - `uk.ac.cam.cares.derivation.asynexample.IntegrationTest`. The tests should pass if you already correctly setup the [Docker Environment](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment) and obtained access to [Docker Image Registry](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry). To run the tests, navigate to the folder containing the java project `DerivationAsynExample`, and run the command `mvn clean test`. As we are operating in asynchronous mode, it might take a couple of minutes (~3 mins) before the testing is finished.

## Logs
By default, logs are written to `/root/.jps/` within the docker container. To copy this file into your local directory, run the following command in the terminal
```
docker cp derivationasynexample:root/.jps/ .
```

## Initialisation
Assuming the stack is up and running, the instances can be initialised by running the following command:
```
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances
```
If it is successful, you should receive an HTTP response with the IRIs of the newly created instances, e.g.
```json
{"MaxValue Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_2b35a4f6-b84f-46ed-b8a9-445e65840af9","ListOfRandomPoints instance":"http://derivation_asyn_example#c0ada22f-8a9e-45ce-b2b6-786d6fbd610e","UpperLimit instance":"http://derivation_asyn_example#d4155720-9692-46ba-8cd5-7d982b7e342c","MinValue instance":"http://derivation_asyn_example#32cf77bf-008f-4538-9198-fd9ed2377e27","Difference instance":"http://derivation_asyn_example#2ea19a84-5508-4e48-bd97-f53ddde1059c","NumberOfPoints instance":"http://derivation_asyn_example#2f9149b2-05ed-440a-8762-767fee51d211","Difference Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11","LowerLimit instance":"http://derivation_asyn_example#801a906e-c4e6-4998-a7a1-1065b9cf41a5","MaxValue instance":"http://derivation_asyn_example#8345d2cf-fc1a-4b52-bc34-8dda61aaaa11","RandomNumberGeneration Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29","MinValue Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_3d38ae98-da52-44ca-8abf-dedcb5c21531"}
```
If this is not successful, it may be the case that the `derivationasynexample` container is still loading up, check the console of the container and ensure that the you see a line like this:
```
04-Dec-2021 16:50:38.060 INFO [main] org.apache.catalina.startup.Catalina.start Server startup in [112291] milliseconds
```

## Updating the derivations
The derivations in this example can be checked for update by running the command:
```
curl http://localhost:58085/DerivationAsynExample/UpdateDerivations
```
On a successful update request, you should receive an HTTP response, e.g.:
```json
{"status":"Checked derivation of difference <https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>, the update should be done in a few minutes"}
```
As here we are demonstrating asynchronous operation, you may use below SPARQL query in the blazegraph container (http://localhost:8889/blazegraph) to check the status during the course of update:
```
PREFIX OntoDerivation: <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
PREFIX time: <http://www.w3.org/2006/time#>
SELECT ?derivation ?devTime ?inputTime ?status ?status_type
WHERE {
  ?derivation a OntoDerivation:DerivationAsyn;
                time:hasTime/time:inTimePosition/time:numericPosition ?devTime .
  OPTIONAL {?derivation OntoDerivation:isDerivedFrom/time:hasTime/time:inTimePosition/time:numericPosition ?inputTime .}
  OPTIONAL {?derivation OntoDerivation:hasStatus ?status .
            ?status a ?status_type}
}
```
Once the update is finished, you may get results from SPARQL query similar to below:
| derivation | devTime | inputTime | status | status_type |
| --- | --- | --- | --- | --- |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638647788 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638647788 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638647788 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_2b35a4f6-b84f-46ed-b8a9-445e65840af9>` | 1638648307 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_3d38ae98-da52-44ca-8abf-dedcb5c21531>` | 1638648307 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>` | 1638648322 |

During the course of update, you may also see IRIs appear at the `status` and `status_type` columns, but there should NEVER be instances appear in `inputTime` for `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_2b35a4f6-b84f-46ed-b8a9-445e65840af9>`, `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_3d38ae98-da52-44ca-8abf-dedcb5c21531>`, and `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>` - as by design the inputs of these three derivation instances are all derived quantities from another derivation, thus NO timestamp are added.

## Input agent
If after the derivations are updated successfully and you would like to have another go, `InputAgent` provides such an option - the input instance of `NumberOfPoints` can be updated using it. Each time the agent is called, the value of the `numofpoints` is increased by 1. In addition, it also updates the unix time stamp of the instance in the triple-store (blazegraph).
```
curl http://localhost:58085/DerivationAsynExample/InputAgent
```
If the update is successful, you should receive an HTTP response, e.g.:
```json
{"Updated successfully":{"NumberOfPoints instance":"http://derivation_asyn_example#2f9149b2-05ed-440a-8762-767fee51d211"}}
```
If you now run the SPARQL query privided above again, you may find results look something like:
| derivation | devTime | inputTime | status | status_type |
| --- | --- | --- | --- | --- |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638647788 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638647788 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` | 1638648292 | 1638651402 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_2b35a4f6-b84f-46ed-b8a9-445e65840af9>` | 1638648307 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_3d38ae98-da52-44ca-8abf-dedcb5c21531>` | 1638648307 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>` | 1638648322 |

You can see that one of the `inputTime` of `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` is updated from 1638647788 to 1638651402 and this derivation is now out-of-date in theory. You may want to fire another request for an update and you should receive an HTTP response like:
```json
{"status":"Checked derivation of difference <https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>, the update should be done in a few minutes"}
```
By firing the same SPARQL query as provided above, you may monitor the status during the course of update again. The `InputAgent` can be called and the `UpdateDerivations` can be requested as many times as you would like.
