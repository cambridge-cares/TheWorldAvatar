# Derivation Asynchronous Example

## Purpose
This example demonstrates the usage of the asynchronous function enabled by AsynAgent and DerivationClient/DerivationSparql class from jps-base-lib. The example here is set to follow the same (similar) context as in the [DerivationExample](https://github.com/cambridge-cares/TheWorldAvatar/tree/develop/Agents/DerivationExample).

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

## Setup
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
If `blazegraph_password` is left empty, then you should leave the `DerivationAsynExample/src/main/resources/agents.properties` unchanged. 

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

<!-- ## Test
To test the docker set up, there are integration tests written for this example - `uk.ac.cam.cares.derivation.example.IntegrationTest`. The tests should pass if the stack is up and running with the default port numbers. To run the tests, navigate to the folder containing the java project `DerivationExample`, and run the command `mvn clean test`. -->

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

## Inputs
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
Note that the `value1` `value2` `value3` instances contain data used by the first derivation (Random Number Generation Derivation) that `OntoDerivation:isDerivedFrom` these three inputs. The RNGAgent is responsible for monitoring the derivation and will generate an instance of 
and `timestamp1` `timestamp2` `timestamp3` are used to check whether the derivation is out-of-date compared to the inputs.

This input instance of `NumberOfPoints` can be updated using an input agent. Each time the agent is called, the value of the `numofpoints` is increased by 1. In addition, it also updates the unix time stamp of the instance in the triple-store (blazegraph).
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

You can see that one of the `inputTime` of `<https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_0f064364-f3b5-4e2e-b628-dd840112bd29>` is updated from 1638647788 to 1638651402 and this derivation is in theory out-of-date. You may want to fire another request for an update and you should receive an HTTP response like:
```json
{"status":"Checked derivation of difference <https://www.derivationasynexample.com/triplestore/repository/derivedAsyn_d299313f-f5d7-4380-a616-b854498d1e11>, the update should be done in a few minutes"}
```

## Asynchronous derivations
This example contains 4 derivations:
1. Random Number Generation Derivation: a list of random numbers generated within the upper and lower limits
2. Maximum Value Derivation: the maximum value in the generated list of random numbers
3. Minimum Value Derivation: the minimum value in the generated list of random numbers
4. Difference Derivation: difference between the maximum and minimum values

### Random Number Generation Derivation
The property instance:


<!-- 

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
And returns an HTTP response in the form of:
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
and returns an HTTP response in the form of:
```json
{"agent_output": [new_diff,newDiffValue]}
``` -->


<!-- # Example: Java agent

## What this image is for

The purpose of this image is to show how Docker can be used to deploy an agent as a Java servlet running in a Tomcat server.
It should be useful as template if your servlet extends the uk.ac.cam.cares.jps.base.agent.JPSAgent class.

## How it works

The image uses a [multi-stage build](https://docs.docker.com/develop/develop-images/multistage-build) to:

  1. Generate a .war file from some Java code with Maven dependencies.
  2. Deploy the .war file on a Tomcat server.

The advantages of this two-stage approach are:
  * The final image doesn't include files that aren't needed at runtime, i.e. those required only for building the .war file.
  * Credentials can be supplied to Maven easily, with no risk of exposing them in the final image.

## Using the template

The template is set up to use the Maven repository at https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/ (in addition to Maven central).
You'll need to provide  your credentials in single-word text files located like this:
```
./docker/
    credentials/
        repo_username.txt
        repo_password.txt
```

repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token), which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, you simply need to spin up a container from the image.
In Visual Studio Code, ensure the Docker extension is installed, then right-click docker-compose.yml and select 'Compose Up'.
Alternatively, from the command line, and in the same directory as this README, run

```
docker-compose up -d
```

The agent is reachable on localhost port 58085 by default (you can change this in docker-compose.yml).
To test it, you can you use 'curl' to send it a request on the command line (taking care to escape the ampersand in the argument list):

```
result=$(curl -s http://localhost:58085/java_agent_eg/api/v1?a=2\&b=3) && echo $result
```

Here, 'a' and 'b' are the two variables to be summed; the result is returned in a third variable, 'c', in json format:

```
{"c":[5]}
```

## Adapting the template

To deploy your agent:

* Make a copy of this whole directory
* Replace java_agent/pom.xml and the contents of java_agent/src and java_agent/WEB-INF with your own files

Note that, if you rename the 'java_agent' subdirectory, you'll need to change the 'COPY' command in the second build stage of the Dockerfile.
Similarly, if your pom.xml generates the .war file in a location other than ./output/, the same command will need to be updated. -->