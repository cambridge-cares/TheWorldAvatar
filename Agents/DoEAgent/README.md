# Design of Experiment (DoE) Agent
The folder contains the source, resource, and Docker setup files for the DoE Agent, based on a template provide as `TheWorldAvatar/Deploy/examples/python_agent`. As a first step, the python package `Summit` (https://gosummit.readthedocs.io/en/latest/index.html) is utilised to act as the DoE Agent. The TSEMO algorithm (J Glob Optim 2018, 71, 407–438 https://doi.org/10.1007/s10898-018-0609-2) provided in the `Summit` package is used to suggeste the next experiments. 

## Purpose
The DoE Agent is designed to automate the design of experiment exercise. It does so by querying the information about design of experiment from the knowledge graph, retrieving the experiment data from the knowledge graph, making suggestions about the next experiments, and populating those suggestions back to the knowledge graph. 

## Building the Docker image
Requirements:

* The Python code of this Agent requires the `pyasyncagent` library, which can be downloaded from the external PyPi website https://pypi.org/project/pyasyncagent/. It should be noted that `pyasyncagent` library relies on the `py4jps` package to utilise the functions provided in `jps-base-lib`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```cmd
    git pull
    git checkout <your_branch_with_new_functions>
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
    At the moment, above lines are commented out in the Dockerfile. One may bring them back if a specific version of `jps-base-lib` is required and provided.
* Configurations for the agent are provided in `TheWorldAvatar/Agents/DoEAgent/src/conf/doeagent_properties.json` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT`, with the credentials specified using `KG_USERNAME` and `KG_PASSWORD`. (REMEMBER NEVER COMMIT THESE INFORMATION TO GIT! A BETTER WAY OF ENCODING CREDENTIALS WILL BE PROVIDED ASAP.) The OntoAgent:Service IRI of the agent is specified using `ONTOAGENT_SERVICE`. The periodically time interval to monitor asynchronous derivation is specified by `PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build using the following commands:
```cmd
cd TheWorldAvatar/Agents/DoEAgent/
docker-compose -f "docker-compose.yml" up -d --build
```
Or, simply right click `docker-compose.yml` file and select `Compose Up` option in Visual Studio Code.

## How to use it
### HTTP servlet
After code refactoring (adopting `pyasyncagent`), the ability to serve HTTP requests has been removed in the most recent iteration of the DoE Agent - as the intention of DoE Agent is asynchronous operation, it is (strongly) discouraged to expose it as an HTTP servlet.

If you wish to try out a previous version where DoE Agent itself is an HTTP servlet, please travel back to [this point](https://github.com/cambridge-cares/TheWorldAvatar/tree/8d3daf5628228ad8cacdaa051a63a79a509932aa/Agents/DoEAgent) in the history. In that iteration, DoE Agent is able to take the design of experiment requests at `http://localhost:7000/doe/summit/suggest?` (the address `http://localhost:7000` can be changed depends on where you deploy the container). Developer also need to upload relevant ttl files (provided in `TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/` at that point in history) to the triple store where the agent is monitoring. The request string should be expressed as the URL encoded format of a JSON string that is similar to below:
```json
{
   "agent_input":{
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Strategy":"https://www.example.com/triplestore/ontodoe/DoE_1/Strategy_1",
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#Domain":"https://www.example.com/triplestore/ontodoe/DoE_1/Domain_1",
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#SystemResponse":[
         "https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_1",
         "https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_2"
      ],
      "https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#HistoricalData":"https://www.example.com/triplestore/ontodoe/DoE_1/HistoricalData_1"
   }
}
```
and the response is the IRI of an instance of `OntoDoE:NewExperiment` indicating the suggestions.

### Asynchronous derivation operation
In the latest iteration, DoE Agent only works with the derivation framework in asychronous mode. Once the DoE Agent is deployed, it periodically (every 120 seconds, defined by `PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE` in `TheWorldAvatar/Agents/DoEAgent/src/conf/doeagent_properties.json`) and acts based on the status associated with that derivation.

A simple script `TheWorldAvatar/Agents/DoEAgent/dev_docker.py` is provided as an example to demonstrate the operations. It operates the same triple store specified in the `TheWorldAvatar/Agents/DoEAgent/src/conf/doeagent_properties.json`. Therefore, it can be used to test if the DoE Agent deployed is functional as expected. **ALERT: developer should ONLY execute this script when the agent is operating on a triple store that has NO VALUABLE DATA in it - ALL TRIPLES will be DELETED once the script is executed.**

Once the script is executed, it first DELETES ALL TRIPLES in the specified SPARQL endpoint, it then SPARQL update all triples stated in below three files to the same endpoint:
```
TheWorldAvatar/Agents/DoEAgent/src/test/resources/doe.ttl
TheWorldAvatar/Agents/DoEAgent/src/test/resources/rxn_data.ttl
TheWorldAvatar/Agents/DoEAgent/src/test/resources/Service__DoE.ttl
```

The script then writes below derivation related triples:
```
# derivation is an asynchronous derivation
<derivation> <rdf:type> <OntoDerivation:DerivationAsyn>

# derivation inputs and outputs
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/Strategy_1>
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/Domain_1>
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_1>
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_2>
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/HistoricalData_1>
<https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_1> <OntoDerivation:belongsTo> <derivation>

# agent related
<derivation> <OntoDerivation:isDerivedUsing> <http://www.theworldavatar.com/resource/agents/Service__DoE#Service>

# timestamp of derivation
<derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 0
```

The timestamp of derivation inputs are also added in the similar fashion as derivation, but their `<time:numericPosition>` will be marked as the cureent time - the derivation is out-of-date and the script finally request an update of the derivation.

Once the developer has confirmed NO valuable data are present in the triple store, one may run the script with below commands (You may experience a "SyntaxError: invalid syntax" - this is done intentionally to provide an extra layer of safeguard, please follow the instruction on console and please do NOT commit any changes you made to fix this SyntaxError, it will help to protect other developers):
```cmd
cd /your_absolute_path_to/TheWorldAvatar/Agents/DoEAgent
python dev_docker.py
```

If everything is working as expected, an output on console should be expected similar to the one below (this might take a few minutes) (**Please make a note of the IRI in the response as `<createdDerivationInstance>`, you will need this for querying later**):
```
2022-01-24 12:41:35,081 (STDOUT) Initialised successfully, created derivation instance: http://kg.cmclinnovations.com:81/testontorxn/derivedAsyn_5c513164-b820-422b-a6d3-5facc4108ac7
```
As the derivation is initialised with a timestamp of 0 and the inputs are marked with a timestamp of current time, the derivation is outdated and will be marked as `PendingUpdate`. The update will be taken care of by DoE Agent and the IRI of the suggested instance of `OntoDoE:NewExperiment` will be generated and uploaded into the knowledge graph. This can be verified by querying {`?new_exp` `OntoDerivation:belongsTo` `<createdDerivationInstance>`}:
```
PREFIX OntoDerivation:     <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
PREFIX OntoDoE:            <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontodoe/OntoDoE.owl#>

SELECT ?ontodoe_new_exp ?ontorxn_new_exp
WHERE {
  ?ontodoe_new_exp OntoDerivation:belongsTo <http://kg.cmclinnovations.com:81/testontorxn/derivedAsyn_5c513164-b820-422b-a6d3-5facc4108ac7> .
  OPTIONAL {?ontodoe_new_exp OntoDoE:refersTo ?ontorxn_new_exp}
}
```
If the update was successful (this might take a few minutes), the results of above query will be changed from:
  | ontodoe_new_exp | ontorxn_new_exp |
  | --------------- | --------------- |
  | `<https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_1>` | |

to something similar to below in a few minutes:

  | ontodoe_new_exp | ontorxn_new_exp |
  | --------------- | --------------- |
  | `<https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_f8b195c9-d030-4468-a221-be3b75127c0d>` | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_0bcdabd7-223c-414c-964d-41d96d01146d>` |
  | `<https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_f8b195c9-d030-4468-a221-be3b75127c0d>` | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_93e0c2cd-4739-4a03-bfd2-23987a31a5d2>` |
  | `<https://www.example.com/triplestore/ontodoe/DoE_1/NewExperiment_f8b195c9-d030-4468-a221-be3b75127c0d>` | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_ca1c2a65-6521-47ac-ae1d-4ffb3c394cb1>` |

where the IRIs in `ontorxn_new_exp` column indicate the new suggested instances of `OntoRxn:ReactionVariation`.
