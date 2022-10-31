# Ifc2OntoBim Agent

This agent converts IFC files into TTL files defined by the OntoBIM ontology.

## Instructions
### 1. Requirements
#### 1.1 IFC File
This agent is designed to work with IFC2x3 TC1 schema. IFC4 schema are not yet included. The scope coverage is also 
non-exhaustive at this point, including only the relevant concepts.

Some excluded concepts include:
 - Property Sets
 - Niche Geometries
 - Material Information

#### 1.2 Technical Requirements
- Java 11
- Apache Maven 3.8+
- Docker
- IFCtoRDF
  - Download the `IFCtoRDF-0.4-shaded.jar` file from [here](https://github.com/pipauwel/IFCtoRDF)
  - This should be placed in the working directory, when executing the agent. 
  - Although there is a maven dependency, there are errors in the execution process

### 2. Building the Agent
The agent is designed for execution through a Docker container. Other deployment workflows are beyond the scope of this document.

#### 2.1 Preparation
This agent is set up to use this [Maven repository](https://maven.pkg.github.com/cambridge-cares/TheWorldAvatar/) (in addition to Maven central).
You'll need to provide  your credentials in a single-word text files located like this:
```
./credentials/
    repo_username.txt
    repo_password.txt
```

repo_username.txt should contain your Github username. repo_password.txt should contain your Github [personal access token](https://docs.github.com/en/github/authenticating-to-github/creating-a-personal-access-token),
which must have a 'scope' that [allows you to publish and install packages](https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-apache-maven-registry#authenticating-to-github-packages).

#### 2.2 Docker Deployment
Deploy the agent and its dependencies by running the following code in the command prompt at the `<root>` directory:
```
docker-compose up -d 
```

#### 2.3 Running the Agent
##### 2.3.1 Precursor
Place your IFC file into the `<root>/data/` directory. This is directly linked to the relevant directory in a Docker container.
The agent is able to convert multiple IFC files at once. However, it is unable to upload them into separate endpoints or namespaces 
at one go. 

##### 2.3.2 POST Request Parameters
The agent currently accepts two parameters. 
1. Base URI - Mandatory

This sets the base URI for all instances. Examples of valid URIs include `http://www.theworldavatar.com/ifc/` and  `https://www.theworldavatar.com/bim#`.

A default URI of `http://www.theworldavatar.com/ifc/resources_16UUID/` is also available. Please access this with a request of `"uri":"default"`.

##### 2.3.3 POST Request
Run the agent by sending a POST request with the required JSON Object to `http://localhost:3025/ifc2ontobim-agent/retrieve`. A sample request is as follows:
```
POST http://localhost:3025/ifc2ontobim-agent/retrieve
Content-Type: application/json
{"uri":"http://www.theworldavatar.com/ifc/building/"}

// Written in curl syntax (as one line)
curl -X POST --header "Content-Type: application/json" -d "{'uri':'http://www.theworldavatar.com/ifc/building/'}" localhost:3025/ifc2ontobim-agent/retrieve 
```

If the agent ran successfully, a JSON Object would be returned as follows:
```
{"Result":["File.ttl has been successfully converted!","All ttl files have been generated in OntoBIM. Please check the directory."]}
```

#### 2.3 Post-Build
The generated TTL files can be retrieved at the `<root>/data/` directory.