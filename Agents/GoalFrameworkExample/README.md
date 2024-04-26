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



## Shortcuts for developing this agent
Docker shortcuts for spinning down and developing the container
```
docker-compose down
docker rmi goalframeworkexample:1.0.0
docker volume remove postgres_data
docker volume remove blazegraph_data

docker-compose up -d
```

```
curl http://localhost:8081/GoalFrameworkExample/InitialiseInstances
curl http://localhost:8081/GoalFrameworkExample/UpdateDerivations
curl http://localhost:8081/GoalFrameworkExample/InputAgent?input=truck


```