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

#### Stack Deployment

If you want to spin up this agent as part of a stack, instead of `docker-compose up -d`, do the following:
- Build the image via `docker-compose build`. Do not start the container.
- Copy the `json` file from the `stack-manager-input-config` folder into the `inputs/config` folder of the stack manager.
- Start the stack manager as usual. This should start the container.

#### Run the agent
To run the agent, a POST request must be sent to http://localhost:1080/sewage-network-agent/performsewageupdate with a JSON Object. In this agent, it contains the URL for sparql endpoint. Follow the sample request shown below.
```

POST http://localhost:1080/sewage-network-agent/performsewageupdate
Content-Type: application/json
{"endpoint":"http://host.docker.internal:48888/ontosewage"}
```

In curl syntax:
```
curl -X POST --header "Content-Type: application/json" -d "{
\"endpoint\":\"http://host.docker.internal:48888/ontosewage\"}"  http://localhost:1080/sewage-network-agent/performsewageupdate
```

If the agent runs successfully, you should see a returned JSON Object that is similar to the one shown below.
```
{"Result":"Data has been instantiated."}
```

#### Import the CSV data file into the agent
A dockerised python code is available under the DataEngineering folder to generate all the required CSV data files. The number of columns inside the imported CSV data files should not exceed 7500, otherwise it will throw an index-out-of-bound error. Therefore, it is recommended to separate the whole data into few CSV files to instantiate the sewage network. Please refer to set the required environment variables section in the Dockerfile for importing different CSV files into the agent. All the CSV files have to be put under the resources folder. It is able to take in all the CSV files and instantiate them all at once. 
