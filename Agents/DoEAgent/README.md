# Design of Experiment (DoE) Agent
The folder contains the source, resource, and Docker setup files for the DoE Agent, based on a template provide as `TheWorldAvatar/Deploy/examples/python_agent`. As a first step, the python package `Summit` (https://gosummit.readthedocs.io/en/latest/index.html) is utilised to act as the DoE Agent. The TSEMO algorithm (J Glob Optim 2018, 71, 407–438 https://doi.org/10.1007/s10898-018-0609-2) provided in the `Summit` package is used to suggeste the next experiments. 

## Purpose
The DoE Agent is designed to automate the design of experiment exercise. It does so by querying the information about design of experiment from the knowledge graph, retrieving the experiment data from the knowledge graph, making suggestions about the next experiments, and populating those suggestions back to the knowledge graph. 

## Building the Docker image
Requirements:

* The Python code of this Agent requires the py4jps library, this is currently downloaded from the external PyPi website. However, a specific version of `JpsBaseLib` is used, built from the `JPS_BASE_LIB` in branch https://github.com/cambridge-cares/TheWorldAvatar/tree/135-dev-expand-derivation-framework/JPS_BASE_LIB/ to use the most updated `uk.ac.cam.cares.jps.base.derivation.DerivationClient` class. Given that (1) you are at correct branch (`135-dev-expand-derivation-framework`), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```
    cd TheWorldAvatar/JPS_BASE_LIB
    mvn clean install -DskipTests
    ```
    Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them in the `TheWorldAvatar/Agents/DoEAgent/`. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:
    ```
    # Re-install the version of JPS_BASE_LIB that is been developing
    # (sinse the newly added code is not in the release version of py4jps)
    # TO BE REMOVED WHEN MERGE TO DEVELOP
    RUN jpsrm uninstall JpsBaseLib
    RUN mkdir /jpstemp
    COPY jps-base-lib.jar ./jpstemp/jps-base-lib.jar
    COPY lib ./jpstemp/lib
    RUN jpsrm install JpsBaseLib ./jpstemp/
    ``` 
    Once the changes of the DerivationClient class are merged back to develop branch and wrapped in the latest `py4jps` release, these lines in the Dockerfile can be commented out when building the Docker Image.
* The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT` within the `TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/doeagent_properties.py` file. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build using the following commands:
```
cd TheWorldAvatar/Agents/DoEAgent/
docker-compose -f "docker-compose.yml" up -d --build
```
Or, simply right click `docker-compose.yml` file and select `Compose Up` option in Visual Studio Code.

## How to use it
### HTTP servlet
DoE Agent itself is an HTTP servlet which is able to take the design of experiment requests at `http://localhost:7000/doe/summit/suggest?` (the address `http://localhost:7000` can be changed depends on where you deploy the container). The request string should be expressed as the URL encoded format of a JSON string that is similar to below:
```json
{
   "agent_input":{
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy":"https://theworldavatar.com/kb/ontodoe/DoE_1/Strategy_1",
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain":"https://theworldavatar.com/kb/ontodoe/DoE_1/Domain_1",
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse":[
         "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_1",
         "https://theworldavatar.com/kb/ontodoe/DoE_1/SystemResponse_2"
      ],
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData":"https://theworldavatar.com/kb/ontodoe/DoE_1/HistoricalData_1"
   }
}
```
and the response is the IRI of an instance of `OntoDoE:NewExperiment` indicating the suggestions.

### Asynchronous derivation operation
DoE Agent can also work with the derivation framework in asychronous mode. Once the DoE Agent is deployed, it periodically (every 120 seconds) checks the derivation that `isDerivedUsing` itself (parameter `DOEAGENT_ONTOAGENT_SERVICE` in `TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/doeagent_properties.py`) and start any `Requested` jobs.

For an example, developer can SPARQL Update all triples stated in below three files to the knowledge graph endpoints used by the DoE Agent:
```
TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/doe.ttl
TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/rxn_data.ttl
TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/Service__DoE.ttl
```
Then the developer can access `http://localhost:7000/example` in a browser. This will trigger the creation of a derivation in the knowledge graph and checks if it is out-of-date. As the derivation is initialised with a timestamp of 0 and the inputs are marked with a timestamp of current time, the derivation is outdated and will be marked as `Requested`. The update will be taken care of by DoE Agent and a response will be given stating the IRI of the suggested instance of `OntoDoE:NewExperiment`.
