# Derivation Asynchronous Example

## Namespaces

There are a few namespaces used in this example:

```sparql
PREFIX OntoDerivation: <https://www.theworldavatar.com/kg/ontoderivation/>
PREFIX derivationBase: <https://www.derivationasynexample.com/triplestore/repository/>
PREFIX example: <http://derivation_asyn_example#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX time: <http://www.w3.org/2006/time#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
```

## Purpose

This example demonstrates the usage of the asynchronous function and mixed type directed acyclic graph (DAG) enabled by `DerivationAgent`/`DerivationInputs`/`DerivationOutputs` and `DerivationClient`/`DerivationSparql` class from `jps-base-lib`. The example here is set to follow the same (similar) context as in the [DerivationExample](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/DerivationExample). The general structure of this example follows the [Example: Java agent](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Deploy/examples/java_agent).

## Directed Acyclic Graph (DAG) of derivations

This example contains a DAG of derivations:

1. Random Number Generation Derivation: a list of (specified amount of) random numbers generated within the upper and lower limits
2. Maximum Value Derivation: the maximum value in the generated list of random numbers
3. Minimum Value Derivation: the minimum value in the generated list of random numbers
4. Difference Derivation: difference between the maximum and minimum values

Each derivation has a derivation agent associated with it that responsible for keeping the derivation up-to-date. Depending on the type of derivation created, the derivation agent either monitors the derivation status (`DerivationAsyn`) or conduct calculation upon receiving HTTP requests (`Derivation` and `DerivationWithTimeSeries`). For asynchronous derivation, derivation agent performs tasks based on three `rdf:type` of status (`Requested`, `InProgress`, and `Finished`). All instances within this DAG of derivation depend on three pure inputs: UpperLimit, LowerLimit, and NumberOfPoints. This example demonstrates two modes of asynchronous derivation operations: (1) new information generation; and (2) existing information update. The example starts with creating asynchronous derivation that marked as `Requested` at their creations, each agent performs calculation as soon as its immediate upstream derivation is up-to-date. The information propagates through the knowledge graph and finally the whole knowledge graph is completed. If the pure inputs got updated after that, developer can fire requests for update derivations, all involved derivations will be marked as `Requested`, and each asynchronous agent will work out when to update the derivation it monitors - the update of existing information propagates through the knowledge graph again.

A more visual illustration can be found below:

![Alt text](DerivationAsynExampleMode1.svg?raw=true)

![Alt text](DerivationAsynExampleMode2.svg?raw=true)

### Six cases

To also demonstrate the new added capability of unified DerivationAgent updating the mixed type DAGs, here we provide different cases that can be handled by one sets of derivation agents. For the four derivations aforementioned, each one will be created as either synchronous derivation or asynchronous derivation at its creation. The synchronous derivation will be created with up-to-date information, whereas the asynchronous derivations will be created in the mode of generating new information (i.e. NO outputs marked at its creation). As the framework assumes async derivation can depend on sync derivation but NOT the other way around, there are in total six cases as demonstrated in the below table, where empty entries are all async derivations omitted for simplicity.

|  | RNG | MaxVal | MinVal | Diff |
| --- | --- | --- | --- | --- |
| 1 | Sync | Sync | Sync | Sync |
| 2 | Sync | Sync | Sync | Async |
| 3 | Sync | Async | Sync |  |
| 4 | Sync | Sync | Async |  |
| 5 | Sync | Async | Async |  |
| 6 | Async |  |  |  |

In these cases, the derivation agent will work out when to generate the information if it detects async derivations are to be handled. Once the whole knowledge graph is completed, developer may change the pure inputs and request update of existing information. More details on how to invoke these please refer to below sections. **NOTE: Example case 5 should now be working with the fixes to address the concurrent HTTP request issue. However, it should be noted that a better approach is still desired to prevent the calculation at all. For more details, please refer to this [issue](https://github.com/cambridge-cares/TheWorldAvatar/issues/184).**

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
{"agent_input": {"example:UpperLimit": ["<upperlimit>"], "example:LowerLimit": ["<lowerlimit>"], "example:NumberOfPoints": ["<numofpoints>"]}}
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
{"agent_input": {"example:ListOfRandomPoints": ["<new_listOfRandomPoints>"]}}
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
{"agent_input": {"example:ListOfRandomPoints": ["<new_listOfRandomPoints>"]}}
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
{"agent_input": {"example:MaxValue": ["<new_max>"], "example:MinValue": ["<new_min>"]}}
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
      - DerivationAsynExample/InitialiseInstances (six available API_PATTERN)
      - DerivationAsynExample/UpdateDerivations
      - DerivationAsynExample/InputAgent
    - Those extend DerivationAgent:
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
To test the full asynchronous operation (case 6), there are integration tests written for this example - `uk.ac.cam.cares.derivation.asynexample.IntegrationTest`. The tests should pass if you already correctly setup the [Docker Environment](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment) and obtained access to [Docker Image Registry](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry).

To test the complete docker set up (where all six cases are provided), another sets of integration tests are provided - `uk.ac.cam.cares.derivation.asynexample.DockerIntegrationTest`. The tests should pass if you have correctly spin up the docker images provided in the `TheWorldAvatar/Agents/DerivationAsynExample/docker-compose.yml`. 

To run both tests, navigate to the folder containing the java project `DerivationAsynExample`, and run the command `mvn clean test`. As we are operating in asynchronous mode for most of the examples, it might take a couple of minutes (~6 mins) before the testing is finished.

## Logs
By default, logs are written to `/root/.jps/` within the docker container. To copy this file into your local directory, run the following command in the terminal
```
docker cp derivationasynexample:root/.jps/ .
```

## Initialisation
If you would like to test the example on a more interactive basis, you can spin up the docker container to execute initialisation and update by yourself. Assuming the stack is up and running, the instances can be initialised by running any of the following commands, each corresponding to one of the example cases above:
```
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_1
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_2
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_3
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_4
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_5
curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_6
```
Take `curl http://localhost:58085/DerivationAsynExample/InitialiseInstances_6` as an example, if it is successful, you should receive an HTTP response with the IRIs of the newly created instances, e.g.
```json
{"MaxValue Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_7843ed41-a60e-4fdb-94eb-450164b6ed93","UpperLimit instance":"http://derivation_asyn_example#6f6ab87d-7aa7-4d5a-96c0-6b06cdf8a738","NumberOfPoints instance":"http://derivation_asyn_example#00341b5d-4882-425b-b4c7-8c57b94c2ef9","Difference Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_ecb4acda-b4e8-499c-b1ed-ec6a9e9d1c47","LowerLimit instance":"http://derivation_asyn_example#a56f2544-c365-40d0-a3b0-4af8d794cab0","RandomNumberGeneration Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2","MinValue Derivation":"https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_28aea05d-4801-4f63-a498-a61287e664ee"}
```
If this is not successful, it may be the case that the `derivationasynexample` container is still loading up, check the console of the container and ensure that the you see a line like this:
```
04-Dec-2021 16:50:38.060 INFO [main] org.apache.catalina.startup.Catalina.start Server startup in [112291] milliseconds
```

## Automated information generation

As the derivations are marked as `Requested` at their creations, the new information will be generated automatically. As here we are demonstrating full asynchronous operation (case 6), you may use below SPARQL query in the blazegraph container (http://localhost:8889/blazegraph) to check the status during the course of update:

```sparql
PREFIX OntoDerivation: <https://www.theworldavatar.com/kg/ontoderivation/>
PREFIX time: <http://www.w3.org/2006/time#>
SELECT ?derivation ?devTime ?inputTime ?status ?status_type
WHERE {
  VALUES ?derivationType {OntoDerivation:DerivationAsyn OntoDerivation:Derivation OntoDerivation:DerivationWithTimeSeries}
  ?derivation a ?derivationType;
              time:hasTime/time:inTimePosition/time:numericPosition ?devTime .
  OPTIONAL {?derivation OntoDerivation:isDerivedFrom/time:hasTime/time:inTimePosition/time:numericPosition ?inputTime .}
  OPTIONAL {?derivation OntoDerivation:hasStatus ?status .
            ?status a ?status_type}
}
```

Once the new information is generated is finished, you may get results from SPARQL query similar to below:
| derivation | devTime | inputTime | status | status_type |
| --- | --- | --- | --- | --- |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312229 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312229 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312229 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_7843ed41-a60e-4fdb-94eb-450164b6ed93>` | 1650312245 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_28aea05d-4801-4f63-a498-a61287e664ee>` | 1650312245 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_ecb4acda-b4e8-499c-b1ed-ec6a9e9d1c47>` | 1650312255 |

During the course of update, you may also see IRIs appear at the `status` and `status_type` columns, and there may be instances `0` appear in `inputTime` for `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_7843ed41-a60e-4fdb-94eb-450164b6ed93>`, `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_28aea05d-4801-4f63-a498-a61287e664ee>`, and `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_ecb4acda-b4e8-499c-b1ed-ec6a9e9d1c47>` - as by design these async derivations are directly connected with its upstream derivation via `<isDerivedFrom>` before the outputs of the upstream derivations are generated, thus the inputs of these three derivation instances are all in fact their upstream derivations, thus timestamp with `0` are added.

## Updating existing information
If after the outputs of all derivations are generated successfully and you would like to try the mode that updates existing information, `InputAgent` provides such an option - the input instance of `NumberOfPoints` can be updated using it. Each time the agent is called, the value of the `numofpoints` is increased by 1. In addition, it also updates the unix time stamp of the instance in the triple-store (blazegraph).

```
curl http://localhost:58085/DerivationAsynExample/InputAgent
```

If the update of `numofpoints` value is successful, you should receive an HTTP response, e.g.:

```json
{"Updated successfully":{"NumberOfPoints instance":"http://derivation_asyn_example#00341b5d-4882-425b-b4c7-8c57b94c2ef9"}}
```

If you now run the SPARQL query privided above again, you may find results look something like:
| derivation | devTime | inputTime | status | status_type |
| --- | --- | --- | --- | --- |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312229 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312229 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_069b4d2b-bbd5-4cae-a2cf-7c8647f242c2>` | 1650312235 | 1650312878 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_7843ed41-a60e-4fdb-94eb-450164b6ed93>` | 1650312245 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_28aea05d-4801-4f63-a498-a61287e664ee>` | 1650312245 |
| `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_ecb4acda-b4e8-499c-b1ed-ec6a9e9d1c47>` | 1650312255 |

You can see that one of the `inputTime` of `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` is updated from 1650312229 to 1650312878 and this derivation is now out-of-date in theory. You may want to fire a request for an update by running the command:

```
curl http://localhost:58085/DerivationAsynExample/UpdateDerivations
```

and you should receive an HTTP response on a successful update request, e.g.:

```json
{"status":"Checked derivation of difference <https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_ecb4acda-b4e8-499c-b1ed-ec6a9e9d1c47>, the update should be done in a few minutes"}
```

By firing the same SPARQL query as provided above, you may monitor the status during the course of update again. The `InputAgent` can be called and the `UpdateDerivations` can be requested as many times as you would like.
