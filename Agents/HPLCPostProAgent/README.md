# HPLC Post Processing (HPLCPostPro) Agent
The folder contains the source, resource, and Docker setup files for the DoE Agent, following the suggestions on a template provide as `TheWorldAvatar/JPS_BASE_LIB/python_derivation_agent/README.md`.

## Purpose
The HPLC PostPro Agent is designed to automate the post-processing of the results collected from the HPLC analysis for the automated flow chemistry experiment. It does so by querying the information about the generated HPLC report, the reaction experiment, and the digital twin of the hardware that was used to conduct the experiment, calculating the interested performance indicators, and finally populating the processed results back to the knowledge graph.

## Building the Docker image
Requirements:

* The Python code of this Agent requires the `pyderivationagent` library, which can be downloaded from the external PyPI website (https://pypi.org/project/pyderivationagent/). It should be noted that `pyderivationagent` library relies on the `py4jps` package to utilise the functions provided in `jps-base-lib`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```cmd
    git pull
    git checkout <your_branch_with_new_functions>
    cd /your_absolute_path_to/TheWorldAvatar/JPS_BASE_LIB
    mvn clean install -DskipTests
    ```
    Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them in the `TheWorldAvatar/Agents/HPLCPostProAgent/`. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:
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
* Example of configurations for the agent are provided in `TheWorldAvatar/Agents/HPLCPostProAgent/agent.hplc.postpro.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT` for triple store, and `FILESERVER_URL` for the file server. The credentials for knowledge graph endpoints, i.e. triple store and file server, should be provided in the same file using `KG_USERNAME`, `KG_PASSWORD`, `FILE_SERVER_USERNAME`, `FILE_SERVER_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.hplc.postpro.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The OntoAgent:Service IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations, however, given the nature of the post processing Agent, this is NOT RECOMMENDED. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build via docker container, one example of which is:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/HPLCPostProAgent/
docker-compose -f "docker-compose.test.yml" up -d --build
```
Or, simply right click `docker-compose.test.yml` file and select `Compose Up` option in Visual Studio Code.

For proper deployment, one may need to make a copy of `docker-compose.test.yml` and populate the correct settings for deployment.

## How to use it
### HTTP servlet
As the agent adopts `pyderivationagent`, the agent serving HTTP requests to handle synchronous derivations in an automated fashion, nonetheless, as the intention of HPLCPostPro Agent is asynchronous operation, it is (strongly) discouraged to invoke it via HTTP request by ONESELF, in the situation that synchronous derivation been created for HPLCPostPro agent, all operations will be handled by the derivation framework ON ITS OWN. An HTTP servlet provided in this agent is its instructional page `http://localhost:7000/` (the address depends on where you deploy the container), i.e. you will see a message when accessing the above address if the agent is deployed successfully:
```
This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.
For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/HPLCPostProAgent#readme
```

### Asynchronous derivation operation
As intended, HPLCPostPro Agent works with the derivation framework in asychronous mode. Once the HPLCPostPro Agent is deployed, it periodically (every 120 seconds, defined by `DERIVATION_PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE_IRI` in `TheWorldAvatar/Agents/HPLCPostProAgent/agent.hplc.postpro.env.example`) and acts based on the status associated with that derivation.

A set of dockerised integration tests `TheWorldAvatar/Agents/HPLCPostProAgent/hplcpostproagent/tests` is provided as examples to demonstrate the operations. It operates on the triple store specified in the `TheWorldAvatar/Agents/HPLCPostProAgent/hplcpostproagent/tests/agent.hplc.postpro.env.test` when the docker stack is spun up. Therefore, it can be used to test if the HPLCPostPro Agent deployed is functional as expected. **NOTE: spinning up the containers in this image requires access to the docker.cmclinnovations.com registry from the machine the test is run on. For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry**

Also note that below two lines in `TheWorldAvatar/Agents/HPLCPostProAgent/docker-compose.test.yml` were commented out:
```yml
ports:
 - 7000:5000
```
instead, below line was used, which essentially uses the network of the host machine when deploying Docker image so that the deployed agent will be able to access the triple store and file server deployed at localhost (you can bring above two lines back to life and comment out the `network_mode` line if you would like to specify a remote triple store/file server in the environment file):
```yml
network_mode: host
```

Once the test is executed, it first DELETES ALL TRIPLES in the specified SPARQL endpoint (as the endpoint is specifically for testing purpose, one do not need to worry about if any valuable got deleted), it then SPARQL update all triples stated in below ttl files to the same endpoint (`/path/to/chemistry_and_robots/resources/` varies depend on where the `chemistry_and_robots` package is installed):
```
/path/to/chemistry_and_robots/resources/ontoagent/Service__PostProc.ttl
/path/to/chemistry_and_robots/resources/sample_data/new_exp_data.ttl
/path/to/chemistry_and_robots/resources/sample_data/duplicate_ontorxn.ttl
/path/to/chemistry_and_robots/resources/sample_data/dummy_lab.ttl
/path/to/chemistry_and_robots/resources/sample_data/rxn_data.ttl
/path/to/chemistry_and_robots/resources/sample_data/dummy_post_proc.ttl
```
and upload below sample HPLC raw report to the file server specified (the script also writes some triples for recording purpose to the triple store):
```
/path/to/chemistry_and_robots/resources/sample_data/raw_hplc_report_xls.xls
```

If the upload to file server is successfully, a series of information will be output to console and an IRI will be generated as the generated OntoHPLC:HPLCReport instance, for example `<https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_73e0b361-9960-454c-9d7a-11f3adb240cc>` in below log messages:
```
2022-05-25 12:53:14,297 (STDOUT) HPLC raw report (/home/jb2197/wsl_code/TheWorldAvatar/Agents/HPLCPostProAgent/hplcpostproagent/tests/downloaded_files_for_test/0308a669-2fe3-4e92-a6fb-14e2a66be694.xls) was uploaded to fileserver <http://localhost:48086/FileServer/> at 1653479594.189448 with remote file path at: http://localhost:48086/FileServer/0308a669-2fe3-4e92-a6fb-14e2a66be694.xls 
2022-05-25 12:53:14,298 (STDOUT) The initialised HPLCReport IRI is: <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_73e0b361-9960-454c-9d7a-11f3adb240cc>; the initialised HPLCJob IRI is: <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCJob_06b8692c-fc9a-49b6-87e9-8f3c31306179>
2022-05-25 12:53:14,400 (STDOUT) The identified ReactionExperiment for HPLCReport <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_73e0b361-9960-454c-9d7a-11f3adb240cc> (remote path: http://localhost:48086/FileServer/0308a669-2fe3-4e92-a6fb-14e2a66be694.xls) is: <https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70>
2022-05-25 12:53:14,400 (STDOUT) The HPLCReport <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_73e0b361-9960-454c-9d7a-11f3adb240cc> (remote path: http://localhost:48086/FileServer/0308a669-2fe3-4e92-a6fb-14e2a66be694.xls) was generated using HPLCMethod <https://example.com/blazegraph/namespace/testlab/dummy_lab/HPLCMethod_Dummy>
2022-05-25 12:53:14,508 (STDOUT) HPLCReport <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_73e0b361-9960-454c-9d7a-11f3adb240cc> is connected to ChemicalAmount <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/ChemicalAmount_1_1>
```

The IRI of OntoHPLC:HPLCReport will then be used as the derivation inputs by the script to write below derivation related triples:
```
# derivation is an asynchronous derivation, and marked as Requested at its creation
<derivation> <rdf:type> <OntoDerivation:DerivationAsyn>
<derivation> <OntoDerivation:hasStatus> <status>
<status> <rdf:type> <OntoDerivation:Requested>

# derivation inputs, no outputs were added as here we are requesting for generating new information
<derivation> <OntoDerivation:isDerivedFrom> <https://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416>

# agent related
<derivation> <OntoDerivation:isDerivedUsing> <https://www.theworldavatar.com/kg/agents/Service__PostProc/Service>

# timestamp of derivation
<derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 0
```

The timestamp of derivation inputs are also added in the similar fashion as derivation, but their `<time:numericPosition>` will be marked as the cureent time - the derivation is out-of-date and the script finally request an update of the derivation.

Before running the test, make sure you have docker-compose plugin for pytest installed in your virtual environment, which can be done via executing below command:

`(Linux)`
```sh
pip install pytest-docker-compose
pip install pytest-rerunfailures
```

The dockerised integration test can be invoked via below commands:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/HPLCPostProAgent
pytest -s --docker-compose=./docker-compose.test.yml --reruns 5 --reruns-delay 5
```

All the logging information will be printed in console and you will be notified about the results of the test cases. If everything is working as expected, you will now see an output on console should be expected similar to the one below (this might take a few minutes) (**Please make a note of the IRI in the response as `<createdDerivationInstance>`, you will need this for querying later**):

```
2022-05-25 12:53:14,785 (STDOUT) Initialised successfully, created derivation instance: <http://www.asyncagent.com/triplestore/repository/derivedAsyn_4bcc05b7-ad07-4de5-b3b1-2543bc45659a>
```

As the derivation is initialised as `Requested` with a timestamp of 0 and the inputs are marked with a timestamp of current time, the derivation is outdated and will be started automatically. The update will be taken care of by HPLCPostPro Agent and the IRI of the suggested instance of `OntoRxn:PerformanceIndicator` will be generated and uploaded into the knowledge graph. The script will also check the timestamp of the created derivation to determine if the update is finished (i.e. a non-zero timestamp), during that process, you may see output messages like:

```
2022-05-25 12:53:17,845 (STDOUT) The current timestamp for the derivation <http://www.asyncagent.com/triplestore/repository/derivedAsyn_4bcc05b7-ad07-4de5-b3b1-2543bc45659a> is 0
```

After the derivation is updated, you will see outputs message like:

```
2022-05-25 12:53:23,955 (STDOUT) The current timestamp for the derivation <http://www.asyncagent.com/triplestore/repository/derivedAsyn_4bcc05b7-ad07-4de5-b3b1-2543bc45659a> is 1653479596
```

Meanwhile, the testing script checks the progress by querying `{?performance_indicator OntoDerivation:belongsTo <createdDerivationInstance>; rdf:type/rdfs:subClassOf OntoRxn:PerformanceIndicator}`, i.e.:

```sparql
PREFIX OntoDerivation:     <https://www.theworldavatar.com/kg/ontoderivation/>
PREFIX OntoRxn:            <https://www.theworldavatar.com/kg/ontoreaction/>
PREFIX rdf:                <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs:               <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?performance_indicator
WHERE {
  ?performance_indicator OntoDerivation:belongsTo <http://www.asyncagent.com/triplestore/repository/derivedAsyn_cc66814c-615a-46b4-a062-d74978604c3e>;
                         rdf:type/rdfs:subClassOf* OntoRxn:PerformanceIndicator.
}
```

If the update was successful, the results of above query will be changed from below in logging:

```
2022-05-25 12:53:17,866 (STDOUT) Generated performance indicator: []
```

to something similar to below in a few minutes:

```
2022-05-25 12:53:23,999 (STDOUT) Generated performance indicator: [{'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/Conversion_967deb19-b7bc-4b5b-a236-13df15a0d2ea'}, {'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/EcoScore_ed7d5c04-3388-4b34-9795-0685e2ece916'}, {'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/EnvironmentalFactor_a03c52ca-9404-4b71-9ef3-ba19e78c23a0'}, {'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/RunMaterialCost_9e97205c-7b59-497c-b5bc-187c212e53c4'}, {'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/SpaceTimeYield_0d5b724a-d31a-4200-af31-ae574d14be50'}, {'performance_indicator': 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/Yield_7d236fe9-89ce-4fc1-b12e-e427b73b9bba'}]
```

where the IRIs in `performance_indicator` indicate the computed instances of `OntoRxn:PerformanceIndicator`.

Once the update is done, the script pulls the data back and conducts a few checks to verify the update, if all checks are passed, you will see an output message as:
```
2022-05-25 12:53:24,650 (STDOUT) All checks passed.
```

If you would like to contribute to new features for the HPLCPostPro Agent, you may use the same integration test to make sure the new features added do NOT break the original function.

## Upload docker image to GitHub

Developers who add new features to the `HPLCPostProAgent` handle the distribution of the docker image on GitHub. If you want to add new features that suit your project and release the docker image independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/HPLCPostProAgent
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/HPLCPostProAgent/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/hplc_postpro_agent:x.x.x
```

**NOTE: the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload.**

### Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/HPLCPostProAgent
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number following the same procedure as in the stable version release.

# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
