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

Once the docker stack is up and running, you should be able to access the blazegraph container at (http://localhost:8889/blazegraph). If you are using pgAdmin, you can monitor the postgres container by creating a new server that connects to `localhost` with port number `7432`.

## Test
To test the docker set up, there are integration tests written for this example - `uk.ac.cam.cares.derivation.example.IntegrationTest`. The tests should pass if the stack is up and running with the default port numbers.

## Logs
By default, logs are written to `/root/.jps/` within the docker container. To copy this file into your local directory, run the following command in the terminal
```
docker cp derivationexample:root/.jps/ .
```

## Initialisation
There are two key demonstrations in this example, the input instance is shared between the two examples. Assuming the stack is up and running, the instances can be initialised by running the following command:
```
curl http://localhost:8081/DerivationExample/InitialiseInstances
```
If it is successful, you should receive a HTTP response with the IRIs of the newly created instances, e.g.
```json
{"min value":"http://derivation_example#2b5f97c0-9e90-4eee-8c75-b193a83e2269","derivation of average":"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derived365dbb1a-89c1-49f4-9e00-546f85feb0c2","input":"http://derivation_example#bdba8ae0-51f5-4447-8d4b-1c4c05f8347f","average":"http://derivation_example#e7966f33-a01d-4ee5-a12e-825a96a10060","derivation of difference":"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derived9f192ef4-83a7-4c24-b8cf-3f3d9fc7f441","derivation of min value":"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derived66ccb5a6-6b29-43da-a68c-3e8f3f89010c","difference":"http://derivation_example#01a767ee-d048-4a89-b3e1-6def55f3410d","max value":"http://derivation_example#b8b26247-883d-4243-b19b-d9b08d13cd18","derivation of max value":"https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derived2e8a4d01-bd19-4811-abdb-dee6b21e102a"}
```
If this is not successful, it may be the case that the `derivationexample` container is still loading up, check the console of the container and ensure that the you see a line like this 
```
16-Aug-2021 17:03:05.847 INFO [main] org.apache.catalina.startup.Catalina.start Server startup in [3196] milliseconds
```

## Updating the derivations
The derivations in this example can be updated by running the command:
```
curl http://localhost:8081/DerivationExample/UpdateDerivations
```
On a successful update, you should receive a HTTP response, e.g.:
```json
{"status":"Updated derivation of difference <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derived2341b3e3-598f-46a1-900d-5506d0906c60> and derivation of average <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#derivedd41b0c5d-b442-4a14-b0a0-14ad99c03ecc>"}
```

## Input
The input is an instance containing a time series. Upon initialisation, the following triples are created in blazegraph:
```
<input> <rdf:type> <http://derived_example#InputData>
<input> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> <timeseries>
<timeseries> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasRDB> "jdbc:postgresql://postgres/postgres"
<input> <time:hasTime> <timestamp>
<timestamp> <time:inTimePosition> <unixtime>
<unixtime> <time:numericPosition> 123
```
Note that the `timeseries` instance contains data used by the derivations that depend on this input, and `timestamp` is used to check whether the derivation is out-of-date compared to the input.

This input instance is linked to a table in RDB containing a time column and a column with random numbers:
| time | column1 |
| --- | --- |
| 2021-08-11 13:43:24.282104+00 | 123 |
| 2021-08-11 13:43:40.778827+00 | 456 |

This input instance can be updated using an input agent. Each time the agent is called, a random number is added to the table in RDB. In addition, it also updates the unix time stamp of the instance in the triple-store (blazegraph).
```
curl http://localhost:8081/DerivationExample/InputAgent
```
If the update is successful, you should receive a HTTP response, e.g.:
```json
{"status":"Updated <http://derivation_example#bdba8ae0-51f5-4447-8d4b-1c4c05f8347f>"}
```

## Derivation with time series
### Average
This derivation contains averages calculated from the input and stored in a time series table, e.g.
```
<average> <rdf:type> <http://derived_example#Average>
<average> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasTimeSeries> <timeseries>
<timeseries> <http://www.theworldavatar.com/ontology/ontotimeseries/OntoTimeSeries.owl#hasRDB> "jdbc:postgresql://postgres/postgres"
```
| time | column1 |
| --- | --- |
| 2021-08-11 13:43:24.282104+00 | average1 |
| 2021-08-11 13:43:40.778827+00 | average2 |

Derivation of average:
```
<average> <:belongsTo> <derived_average>
<derived_average> <:isDerivedFrom> <input>
<derived_average> <:isDerivedUsing> <AverageAgent>
```
This instance is updated using `AverageAgent`. This agent queries the average value from the input and records the value in the timeseries table.

## Derivations without time series
This example contains 3 derivations: 
1. Maximum value: Maximum value in the input time series table
2. Minimum value: Minimum value in the input time series table
3. Difference: Difference between the minimum and maximum values

### Maximum value
The property instance:
```
<max> <rdf:type> <http://derived_example#MaxValue>
<max> <http://derived_example#hasValue> <valueOfMax> 
<valueOfMax> <http://derived_example#numericalValue> 123
```
The derivation instance:
```
<derivation_of_max> <:isDerivedFrom> <input>
<max> <:belongsTo> <derivation_of_max>
<valueOfMax> <:belongsTo> <derivation_of_max>
<derivation_of_max> <:isDerivedUsing> <MaxValueAgent>
```
The agent for this instance, `MaxValueAgent`, receives HTTP responses in the form of:
```
{"agent_input": [input]}
```
queries the maximum value from the given input using the TimeSeriesClient, and writes a new instance, e.g.
```
<new_max> <rdf:type> <http://derived_example#MaxValue>
<new_max> <http://derived_example#hasValue> <newMaxValue>
<newMaxValue> <http://derived_example#numericalValue> 123
```
And returns a HTTP response in the form of:
```json
{"agent_output": [new_max, newMaxValue]}
```

### Minimum value
This instance is almost identical with the maximum value instance, except that it has the rdf:type `<http://derived_example#MinValue>`. 
The property instance:
```
<min> <rdf:type> <http://derived_example#MinValue>
<min> <http://derived_example#hasValue> <valueOfMin> 
<valueOfMin> <http://derived_example#numericalValue> 123
```
The derivation instance:
```
<derivation_of_min> <:isDerivedFrom> <input>
<min> <:belongsTo> <derivation_of_min>
<valueOfMin> <:belongsTo> <derivation_of_min>
<derivation_of_min> <:isDerivedUsing> <MinValueAgent>
```
The agent for this derivation, `MinValueAgent`, queries the minimum value from the given input using the TimeSeriesClient, and writes a new instance similarly to the `MaxValueAgent`.

### Difference
The difference instance is also identical, except that it has the rdf:type `<http://derived_example#Difference>`.
```
<diff> <rdf:type> <http://derived_example#Difference>
<diff> <http://derived_example#hasValue> <valueOfDiff> 
<valueOfDiff> <http://derived_example#numericalValue> 123
```

The derivation instance contains two inputs - the minimum value and maximum value instances. 
```
<derivation_of_diff> <:isDerivedFrom> <min>
<derivation_of_diff> <:isDerivedFrom> <max>
<diff> <:belongsTo> <derivation_of_diff>
<valueOfDiff> <:belongsTo> <derivation_of_diff>
<derivation_of_diff> <:isDerivedUsing> <DifferenceAgent>
```
The `DifferenceAgent` receives HTTP requests in the form of:
```json
{"agent_input": [min,max]}
```
It then queries the values using the given IRIs and calculate the difference between the values. The agent creates a new instance
```
<new_diff> <rdf:type> <http://derived_example#Difference>
<new_diff> <http://derived_example#hasValue> <newDiffValue>
<newDiffValue> <http://derived_example#numericalValue> 123
```
and returns a HTTP response in the form of:
```json
{"agent_output": [new_diff,newDiffValue]}
```

