# Sewerage Network Agent
This agent is for maintaining data and the corresponding instances in the knowledge graph (KG) regarding the sewerage network located in a midsize town in Germany. Its purpose is to instantiate instances of the sewerage network. 

## Usage 
This part of the README describes the usage of the sewerage network agent. The module itself can be packaged into an executable war, deployed as a web servlet on tomcat. Sending the appropriate request to the correct URL will initiate the agent. 

The next section explains the requirements to run the agent.

### Requirements
It is required to have access to a knowledge graph SPARQL endpoint. A namespace "ontosewage" is required to be set up in the blazegraph workbench. Access-agent-dev-stack is also required in the Docker container for uploading the data triples into the blazegraph.

This can be either in form of a Docker container or natively running on a machine. It is not in the scope of this README to explain the set-up of a knowledge graph triple store. 

#### Building the Sewerage Network Agent

Sewerage Network Agent is set up to use the Maven repository. Credentials in single-word text files located like this are required:
```
./credentials/
    repo_username.txt
    repo_password.txt
```
repo_username.txt should contain your github username, and repo_password.txt your github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

To build and start the agent, open up the command prompt in the same directory as this README, run
```
docker-compose up -d
```

This agent is reachable at "seweragenetwork-agent/performsewageupdate" on localhost port 1080.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1080/seweragenetwork-agent/performsewageupdate with a JSON Object. In this agent, a random JSON Object will do. Follow the sample request shown below.
```

POST http://localhost:1080/seweragenetwork-agent/performsewageupdate
Content-Type: application/json
{"SewerageNetworkAgent\":"DataInstantiation"}
```

In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{
\"SewerageNetworkAgent\":\"DataInstantiation\"}"  http://localhost:1080/seweragenetwork-agent/performsewageupdate
```

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":"Data has been instantiated."}
```

#### Import the CSV data file into the agent
The number of columns inside the imported CSV data files should not exceed 7500, otherwise it will throw an index-out-of-bound error. Therefore, it is recommended to separate the whole data into few CSV files to instantiate the sewage network. Please refer to set the required environment variables section in the Dockerfile for importing different CSV files into the agent. All the CSV files have to be put under config folder. It is able to take in all the CSV files and instantiate them all at once.