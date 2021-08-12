# Derivation Example

## Purpose
This example demonstrates the usage of the DerivationClient class from jps-base-lib.

## Setup
This example uses a docker stack with three containers:

1. derivationexample (containing the following servlets)
    - DerivationExample/InitialiseInstances
    - DerivationExample/InputAgent
    - DerivationExample/MinValueAgent
    - DerivationExample/MaxValueAgent
    - DerivationExample/DifferenceAgent
    - DerivationExample/AverageAgent
2. postgres
3. blazegraph

The example is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files in the `credentials` folder
```
  credentials/
      repo_username.txt
      repo_password.txt
      blazegraph_password
      postgres_password
```

`repo_username.txt` should contain your github username, and `repo_password.txt` your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

`blazegraph_password` and `postgres_password` will set the password for the postgres and blazegraph containers.

Next, you will also need to populate the file `DerivationExample/src/main/resources/credentials.properties` with the passwords you set for your blazegraph and postgres containers, i.e.
```
db.password = [YOUR_POSTGRES_PASSWORD]
kg.password = [YOUR_BLAZEGRAPH_PASSWORD]
```
You should leave the other fields unchanged.

To build and start the service, you need to spin up the stack using the docker-compose.yml file provided.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The default port numbers for the containers are:
| container | port number |
| --- | --- |
| derivationexample | 8081 |
| blazegraph | 8889 |
| postgres | 7432 |
|  |  |

Once the docker stack is up and running, you should be able to access the blazegraph at (http://localhost:8889/blazegraph). If you are using pgAdmin, you can monitor the postgres container by creating a new server that connects to `localhost` with port number `7432`.

## A derivation instance
An instance consisting of more than one entity is wrapped around a derivation instance using the `belongsTo` property, in the form of:
```
: https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#
msm : http://www.theworldavatar.com/ontology/ontoagent/MSM.owl#
time : http://www.w3.org/2006/time#
<entity1> <:belongsTo> <Derivation>
<entity2> <:belongsTo> <Derivation>
```
There is no limit on the number of entities per derivation instance.

Inputs for a derivation is marked using the `isDerivedFrom` property, there is no limit on the number of inputs:
```
<Derivation> <:isDerivedFrom> <input1>
<Derivation> <:isDerivedFrom> <input2>
```
And lastly, each derivation requires an agent attached to it and the agent is marked using the `isDerivedUsing` property:
```
<Derivation> <:isDerivedUsing> <Agent>
<Agent> <msm:hasOperation> <Operation>
<Operation> <msm:hasHttpUrl> <URL>
```
Each agent is assumed to be a servlet and must have a URL. Developers must ensure that the entity calling the `DerivationClient` can access the agent at the URL provided.

Upon initialisation, each derivation has a unix timestamp attached using the W3C standard:
```
<Derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 123
```
Methods are provided in the `DerivationClient` to add and query these instances.

There are two types of derivations
1. Derivation without time series, initialised using `createDerivation`
  The instances wrapped around the derivation instance are deleted and replaced each time the agent makes an update.

2. Derivation with time series, initialised using `createDerivationWithTimeSeries`
  It is assumed that the agents acting on these derivations add row(s) to the table(s), the entities under these derivations are not deleted and replaced.

## Agents
It is assumed that all agents extend the JPSAgent class, where the inputs and outputs consist a JSON object.
### Derivation without time series
The HTTP request made by the DerivationClient will be a JSON object with the `"agent_input"` key, agents should use the public parameter `DerivationClient.AGENT_INPUT_KEY`. Within this JSON object is a JSON array consisting a list of IRIs marked using `isDerivedFrom` for the derivation being updated.
```
HTTP request: {"agent_input": [iri_in1, iri_in2, iri_in3, ...]}
```
Upon receiving these inputs, the agent is expected to write new instances in the knowledge graph, the IRIs of the new instances should be included in the HTTP response with the `"agent_output"` key, provided by the parameter `DerivationClient.AGENT_OUTPUT_KEY`.
```
HTTP response: {"agent_output": [iri_out1, iri_out2, iri_out3, ...]}
```

## Instances for this example
There are two key demonstrations in this example, the input instance is shared between the two examples. Assuming the stack is up and running, the instances can be initialised by running the following command:
```
curl http://localhost:8081/DerivationExample/InitialiseInstances
```

### Input
The input is an instance containing a time series. Upon initialisation, the following triples are created in blazegraph:
```
<Input> <rdf:type> <http://derived_example#InputData>
<Input> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> <timeseries>
<timeseries> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasRDB> "jdbc:postgresql://postgres/postgres"
```
This instance is linked to a table in RDB containing a time column and a column with random numbers:
| time | column1 |
| --- | --- |
| 2021-08-11 13:43:24.282104+00 | 123 |
| 2021-08-11 13:43:40.778827+00 | 456 |
|  |  |

### Derivations without time series
There are three key instances:
1. Maximum value - This derivation is the maximum value of the input
2. Minimum value - This derivation is the minimum value of the input
3. Difference - This is the difference between the maximum and minimum value, i.e. maximum - minimum.

Each of the above instances is an instance with two entities, in this case, &lt;property&gt; and &lt;propertyvalue&gt;:
```
<property> <rdf:type> <http://derived_example#[PROPERTY]>
<property> <hasValue> <propertyvalue>
<propertyvalue> <numericalValue> 123
```

A derivation instance is created for each of the instances