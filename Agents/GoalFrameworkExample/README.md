# Goal Framework Example (Bin Related)

## Purpose
This exercise is an application of Derived Information Framework and Goal Framework.


## Setup
This example uses a docker stack with three containers:

1. derivationexample (containing the following servlets)
    - GoalFrameworkExample/InitialiseInstances
    - GoalFrameworkExample/InputAgent
    - GoalFrameworkExample/BinEmptyingAgent
    - GoalFrameworkExample/TruckEmptyingAgent
    - GoalFrameworkExample/Actuator
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
| GoalFrameworkExample | 8081 |
| blazegraph | 8889 |
| postgres | 7432 |

Once the docker stack is up and running, you should be able to access the blazegraph container at (http://localhost:8889/blazegraph). If you are using pgAdmin, you can monitor the postgres container by creating a new server that connects to `localhost` with port number `7432`.

## Logs
By default, logs are written to `/root/.jps/` within the docker container. To copy this file into your local directory, run the following command in the terminal
```
docker cp goalframeworkexample:root/.jps/ .
```

## Initialisation
A more visual illustration can be found below:
1) Goal Framework
![Alt text](https://lucid.app/publicSegments/view/2db91170-c758-4c48-895b-54f94ef39185/image.png)

2) Goal Framework Example
![Alt text](https://lucid.app/publicSegments/view/cc671f70-a694-4eef-b737-e4efab0d7d01/image.png)
Assuming the stack is up and running, the instances can be initialised by running the following command:
```
curl http://localhost:8081/GoalFrameworkExample/InitialiseInstances
```

## Modifying the input instances
Request Parameters
`input`: Specify either `truck` or `bin` to indicate the type of input instances you want to modify.
`value`: An integer representing the value to be added into the timeseries.

This command adds a new timestamp and value into the specific timeseries instance of either the bin, or the truck.
```
curl "http://localhost:8081/GoalFrameworkExample/InputAgent?input=bin&value=400"
```

## Updating the derivations
This command activates the update derivations to check whether the inputs of `truck` and `bin` are outdated and subsequently call the linked agents. 
```
curl http://localhost:8081/GoalFrameworkExample/UpdateDerivations
```

## Shortcuts for developing this agent
Docker shortcuts for spinning down and developing the container
```
## Shortcut commands for debugging and rebuilding purposes
## Spinning down the stack; Removing the image; Removing the data volumes (i.e., postgres_data, blazegraph_data); Rebuilding the image
docker-compose down
docker rmi goalframeworkexample:1.0.0
docker volume remove postgres_data
docker volume remove blazegraph_data

docker-compose up -d
```