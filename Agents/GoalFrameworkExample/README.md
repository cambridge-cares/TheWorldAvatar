# Goal Framework Example (Bin Related)

## Purpose
This exercise is a modification from [DerivationExample (Synchronous)](../DerivationExample/) whereby the example demonstrates the usage of the derivation framework (DerivationInputs, DerivationOutputs, DerivationAgent, DerivationClient, and DerivationSparql class) from jps-base-lib. The purpose of this exercise to implement a simple example for BNL Use Case. 

## Setup
This example uses a docker stack with three containers:

1. derivationexample (containing the following servlets)
    - DerivationExample/InitialiseInstances
    - DerivationExample/InputAgent
    - DerivationExample/SumValueAgent
    - DerivationExample/TruckCountingAgent
2. postgres
3. blazegraph

The example is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide your credentials in single-word text files in the `credentials` folder
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

> NOTE: if the `postgres_data` volume was initialised for previous docker containers, it is required to remove it using: `docker volume rm postgres_data`

To build and start the service, you need to spin up the stack using the docker-compose.yml file provided.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The default port numbers for the containers are:
| container | port number |
| --- | --- |
| derivationexamplebnl | 8081 |
| blazegraph | 8889 |
| postgres | 7432 |

Once the docker stack is up and running, you should be able to access the blazegraph container at (http://localhost:8889/blazegraph). If you are using pgAdmin, you can monitor the postgres container by creating a new server that connects to `localhost` with port number `7432`.

## Logs
By default, logs are written to `/root/.jps/` within the docker container. To copy this file into your local directory, run the following command in the terminal
```
docker cp derivationexamplebnl:root/.jps/ .
```

## Initialisation
A more visual illustration can be found below:

![Alt text](https://lucid.app/publicSegments/view/2ca9228e-b5cf-4ec4-8bd0-4a65dfadba51/image.jpeg)

Assuming the stack is up and running, the instances can be initialised by running the following command:
```
curl http://localhost:8081/DerivationExample/InitialiseInstances
```
If it is successful, you should receive a HTTP response with the IRIs of the newly created instances, e.g.
```json
{"input":"http://bnl_example#7f758542-fca1-4973-968c-0060b975335c","derivation of truck value":"http://derivationexample.com/triplestore/repository/Derivation_8d976d70-b231-4a02-8fd2-8140e33e7e14","truck value":"http://bnl_example#
5792cb5e-7256-48a3-a871-fa07d96d8cde","sum value":"http://bnl_example#93bcd78a-11b8-48e6-ae23-785c1053b8ab","derivation of sum value":"http://derivationexample.com/triplestore/repository/Derivation_7085e7fd-f34d-47c7-9028-8f5accffcd89"}

```
If this is not successful, it may be the case that the `derivationexamplebnl` container is still loading up, check the console of the container and ensure that the you see a line like this 
```
16-Aug-2021 17:03:05.847 INFO [main] org.apache.catalina.startup.Catalina.start Server startup in [3196] milliseconds
```

## Updating the derivations
The derivations in this example can be updated by running the command:
```
curl http://localhost:8081/DerivationExample/UpdateDerivations
```

## Input
The input is an instance containing a time series. Upon initialisation, the following triples are created in blazegraph:
```
<input> <rdf:type> <http://bnl_example#InputData>
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

## Derivations without time series
This example contains 3 derivations: 
1. Sum value: Sum value in the input time series table
2. Truck vlaue: Count the trucks needed divide by 

### Sum value
Sums up the total value of the inputs.

The property instance
```
<sum> <rdf:type> <http://bnl_example#SumValue>
<sum> <http://bnl_example#hasValue> <valueOfSum> 
<valueOfSum> <http://bnl_example#numericalValue> 123
```

The derivation instance
```
<derivation_of_sum> <:isDerivedFrom> <input>
<sum> <:belongsTo> <derivation_of_sum>
<valueOfSum> <:belongsTo> <derivation_of_sum>
<derivation_of_sum> <:isDerivedUsing> <SumValueAgent>
```

The agent for this instance `SumValueAgent`, receives HTTP responses in the form of
```
{"downstream_derivation":{"http://bnl_example#4cbbf53c-7614-4425-914b-4822d5499526":["http://derivationexample.com/triplestore/repository/Derivation_8d976d70-b231-4a02-8fd2-8140e33e7e14"],"http://bnl_example#b20c5c37-eb42-457d-9fbb-1b147ecb272a":["http://derivationexample.com/triplestore/repository/Derivation_8d976d70-b231-4a02-8fd2-8140e33e7e14"]},"method":"GET","sync_new_info":false,"requestUrl":"http://derivationexamplebnl:8080/DerivationExample/SumValueAgent","derivation_rdftype":"https://www.theworldavatar.com/kg/ontoderivation/Derivation","derivation":"http://derivationexample.com/triplestore/repository/Derivation_7085e7fd-f34d-47c7-9028-8f5accffcd89","belongsTo":{"http://bnl_example#4cbbf53c-7614-4425-914b-4822d5499526":"http://bnl_example#ScalarValue","http://bnl_example#b20c5c37-eb42-457d-9fbb-1b147ecb272a":"http://bnl_example#SumValue"},"agent_input":{"http://bnl_example#InputData":["http://bnl_example#7f758542-fca1-4973-968c-0060b975335c"]}}
```
sums the value from the input using TimeSeriesClient and writes a new instnace, e.g.,
```
<new_sum> <rdf:type> <http://derived_example#SumValue>
<new_sum> <http://derived_example#hasValue> <newSumValue>
<newSumValue> <http://derived_example#numericalValue> 123
```

### Truck Counting
The truck counting is similar, except that it has the rdf:type `<http://bnl_example#TruckValue>`

The derivation instance contains one inputs - the sum value. It retrieves the sum value and divides by 500 kg per truck, output a string indicating how many trucks are needed to carry the sum value
```
3.0 trucks are needed to carry the weight.
```


