# Design of Experiment (DoE) Agent
The folder contains the source, resource, and Docker setup files for the DoE Agent, following the suggestions on a template provide as `TheWorldAvatar/JPS_BASE_LIB/python_derivation_agent/README.md`. As a first step, the python package `Summit` (https://gosummit.readthedocs.io/en/latest/index.html) is utilised to act as the DoE Agent. The TSEMO algorithm (J Glob Optim 2018, 71, 407–438 https://doi.org/10.1007/s10898-018-0609-2) provided in the `Summit` package is used to suggeste the next experiments.

## Purpose
The DoE Agent is designed to automate the design of experiment exercise. It does so by querying the information about design of experiment from the knowledge graph, retrieving the experiment data from the knowledge graph, making suggestions about the next experiments, and populating those suggestions back to the knowledge graph.

## Building the Docker image
Requirements:

* The Python code of this Agent requires the `pyderivationagent` library, which can be downloaded from the external PyPI website (https://pypi.org/project/pyderivationagent/). It should be noted that `pyderivationagent` library relies on the `py4jps` package to utilise the functions provided in `jps-base-lib`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
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
* Example of configurations for the agent are provided in `TheWorldAvatar/Agents/DoEAgent/agent.doe.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT`, with the credentials specified using `KG_USERNAME` and `KG_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.doe.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The OntoAgent:Service IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations, however, given the nature of the DoE Agent, this is NOT RECOMMENDED. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build via docker container, one example of which is:

`(Linux)`
```sh
cd TheWorldAvatar/Agents/DoEAgent/
docker-compose -f "docker-compose.test.yml" up -d --build
```
Or, simply right click `docker-compose.test.yml` file and select `Compose Up` option in Visual Studio Code.

For proper deployment, one may need to make a copy of `docker-compose.test.yml` and populate the correct settings for deployment.

## How to use it
### HTTP servlet (NOT RECOMMENDED)
After code refactoring (adopting `pyasyncagent`), the ability to serve HTTP requests has been removed in that iteration of the DoE Agent, however, this has been brought back to life after adopting `pyderivationagent` - nonetheless, as the intention of DoE Agent is asynchronous operation, it is (strongly) discouraged to invoke it via HTTP request by ONESELF, in the situation that synchronous derivation been created for DoE agent, all operations will be handled by the derivation framework ON ITS OWN.

If you wish to try out a previous version where DoE Agent itself is an HTTP servlet, please travel back to [this point](https://github.com/cambridge-cares/TheWorldAvatar/tree/8d3daf5628228ad8cacdaa051a63a79a509932aa/Agents/DoEAgent) in the history. In that iteration, DoE Agent is able to take the design of experiment requests at `http://localhost:7000/doe/summit/suggest?` (the address `http://localhost:7000` can be changed depends on where you deploy the container). Developer also need to upload relevant ttl files (provided in `TheWorldAvatar/Agents/DoEAgent/summit_agent/resources/` at that point in history) to the triple store where the agent is monitoring. The request string should be expressed as the URL encoded format of a JSON string that is similar to below:
```json
{
   "agent_input":{
      "https://www.theworldavatar.com/kg/ontodoe/Strategy":"https://www.example.com/triplestore/ontodoe/DoE_1/Strategy_1",
      "https://www.theworldavatar.com/kg/ontodoe/Domain":"https://www.example.com/triplestore/ontodoe/DoE_1/Domain_1",
      "https://www.theworldavatar.com/kg/ontodoe/SystemResponse":[
         "https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_1",
         "https://www.example.com/triplestore/ontodoe/DoE_1/SystemResponse_2"
      ],
      "https://www.theworldavatar.com/kg/ontodoe/HistoricalData":"https://www.example.com/triplestore/ontodoe/DoE_1/HistoricalData_1"
   }
}
```
and the response is the IRI of an instance of `OntoDoE:NewExperiment` indicating the suggestions.

### Asynchronous derivation operation
In the latest iteration, DoE Agent works with the derivation framework in both asychronous and synchronous mode. However, as the intention is to provide asynchronous derivation operation, we focus on the async side in this document. Once the DoE Agent is deployed, it periodically (every 120 seconds, defined by `DERIVATION_PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE_IRI` in `TheWorldAvatar/Agents/DoEAgent/agent.doe.env.example`) and acts based on the status associated with that derivation.

A set of dockerised integration tests `TheWorldAvatar/Agents/DoEAgent/doeagent/tests` is provided as examples to demonstrate the operations. It operates on the triple store specified in the `TheWorldAvatar/Agents/DoEAgent/doeagent/tests/agent.doe.env.test` when the docker stack is spun up. Therefore, it can be used to test if the DoE Agent deployed is functional as expected.

Once the test is executed, it first DELETES ALL TRIPLES in the specified SPARQL endpoint (as the endpoint is specifically for testing purpose, one do not need to worry about if any valuable got deleted), it then SPARQL update all triples stated in below ttl files to the same endpoint (`/path/to/chemistry_and_robots/resources/` varies depend on where the `chemistry_and_robots` package is installed):
```
/path/to/chemistry_and_robots/resources/sample_data/doe.ttl
/path/to/chemistry_and_robots/resources/sample_data/rxn_data.ttl
/path/to/chemistry_and_robots/resources/sample_data/dummy_lab.ttl'
/path/to/chemistry_and_robots/resources/ontoagent/Service__DoE.ttl
```

The script then writes below derivation related triples:
```
# derivation is an asynchronous derivation, and marked as Requested at its creation
<derivation> <rdf:type> <OntoDerivation:DerivationAsyn>
<derivation> <OntoDerivation:hasStatus> <status>
<status> <rdf:type> <OntoDerivation:Requested>

# derivation inputs, no outputs were added as here we are requesting for generating new information
<derivation> <OntoDerivation:isDerivedFrom> <https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1>

# agent related
<derivation> <OntoDerivation:isDerivedUsing> <https://www.theworldavatar.com/kg/agents/Service__DoE#Service>

# timestamp of derivation
<derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 0
```

The timestamp of derivation inputs are also added in the similar fashion as derivation, but their `<time:numericPosition>` will be marked as the cureent time - the derivation is out-of-date and `Requested` thus the agent will automatically go update the derivation.

The dockerised integration test can be invoked via below commands:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/DoEAgent
pytest -s --docker-compose=./docker-compose.test.yml --reruns 5 --reruns-delay 5
```

If everything is working as expected, an output on console should be expected similar to the one below (this might take a few minutes) (**Please make a note of the IRI in the response as `<createdDerivationInstance>`, you will need this for querying later**):

```
2022-05-22 14:05:34,367 (STDOUT) Initialised successfully, created asynchronous derivation instance: http://www.asyncagent.com/triplestore/repository/derivedAsyn_44785428-1128-4068-9d08-c4005a8d5fb6
```

As the derivation is initialised as `Requested` with a timestamp of 0 and the inputs are marked with a timestamp of current time, the derivation is outdated and will be started automatically. The update will be taken care of by DoE Agent and the IRI of the suggested instance of `OntoRxn:ReactionVariation` will be generated and uploaded into the knowledge graph. This can be verified by querying {`?new_exp` `OntoDerivation:belongsTo` `<createdDerivationInstance>`}:
```
PREFIX OntoDerivation:     <https://www.theworldavatar.com/kg/ontoderivation/>
PREFIX OntoDoE:            <https://www.theworldavatar.com/kg/ontodoe/>

SELECT ?ontorxn_rxn_exp
WHERE {
  ?ontorxn_rxn_exp OntoDerivation:belongsTo <http://www.asyncagent.com/triplestore/repository/derivedAsyn_44785428-1128-4068-9d08-c4005a8d5fb6> .
}
```
If the update was successful (this might take a few minutes, you may want to change that to a smaller value for testing purpose), the results of above query will be changed from:
  | ontorxn_rxn_exp |
  | --------------- |
  |  |

to something similar to below in a few minutes:

  | ontorxn_rxn_exp |
  | --------------- |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_35bb4494-9498-47b3-bdf7-eee8dce3edd6>` |

where the IRIs in `ontorxn_rxn_exp` column indicate the new suggested instances of `OntoRxn:ReactionVariation`.

This will also be reflected in the console as an output similar to below will be printed:
```
2022-05-22 14:05:54,427 (STDOUT) New experiment suggested successfully, suggested experiment instance: https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_35bb4494-9498-47b3-bdf7-eee8dce3edd6
```


## Upload docker image to GitHub

Developers who add new features to the `DoEAgent` handle the distribution of the docker image on GitHub. If you want to add new features that suit your project and release the docker image independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/DoEAgent
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/DoEAgent/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/doe_agent:x.x.x
```

**NOTE: the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload.**

### Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/DoEAgent
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number following the same procedure as in the stable version release.

# Author

Jiaru Bai (jb2197@cam.ac.uk)
