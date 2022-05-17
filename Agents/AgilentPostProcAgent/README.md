# Post Processing (PostProc) Agent
The folder contains the source, tests and Docker setup files for the PostProc Agent, based on a template provide as `TheWorldAvatar/Deploy/examples/python_agent`.

## Purpose
The PostProc Agent is designed to automate the post-processing of the results collected from the HPLC analysis for the automated flow chemistry experiment. It does so by querying the information about the generated HPLC report, the reaction experiment, and the digital twin of the hardware that was used to conduct the experiment, calculating the interested performance indicators, and finally populating the processed results back to the knowledge graph.

## Building the Docker image
Requirements:

* The Python code of this Agent requires the `pyasyncagent` library, which can be downloaded from the external PyPi website https://pypi.org/project/pyasyncagent/. It should be noted that `pyasyncagent` library relies on the `py4jps` package to utilise the functions provided in `jps-base-lib`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```cmd
    git pull
    git checkout <your_branch_with_new_functions>
    cd /your_absolute_path_to/TheWorldAvatar/JPS_BASE_LIB
    mvn clean install -DskipTests
    ```
    Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them in the `TheWorldAvatar/Agents/PostProcAgent/`. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:
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
* Configurations for the agent are provided in `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/agent_properties.json` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT` for triple store, and `FILESERVER_URL` for the file server. The credentials for knowledge graph endpoints, i.e. triple store and file server, should be provided in `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/credentials` folder, specifically, `blazegraph_username`, `blazegraph_password`, `fileserver_username`, and `fileserver_password`. Note that these files are listed in `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/credentials/.gitignore`, so please do check that you created the credential files with the correct name as they should NEVER be committed to git under any circumstance. The `OntoAgent:Service` IRI of the agent is specified using `ONTOAGENT_SERVICE`. The periodically time interval to monitor asynchronous derivation is specified by `PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build using the following commands:
```cmd
cd /your_absolute_path_to/TheWorldAvatar/Agents/PostProcAgent/
docker-compose -f "docker-compose.yml" up -d --build
```
Or, simply right click `docker-compose.yml` file and select `Compose Up` option in Visual Studio Code.

## How to use it
### HTTP servlet
As the agent adopts `pyasyncagent`, the agent does NOT support serving HTTP requests - the intention of PostProc Agent is asynchronous operation. The only HTTP servlet aspect provided in this agent is its instructional page `http://localhost:7000/` (the address depends on where you deploy the container), i.e. you will see a message when accessing the above address if the agent is deployed successfully:
```
This is an asynchronous agent that capable of post-processing experiment raw data generated from lab equipment.
For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/134-dev-lab-equipment-digital-twin/Agents/PostProcAgent#readme
```

### Asynchronous derivation operation
As intended, PostProc Agent works with the derivation framework in asychronous mode. Once the PostProc Agent is deployed, it periodically (every 120 seconds, defined by `PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE` in `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/agent_properties.json`) and acts based on the status associated with that derivation.

A simple script `TheWorldAvatar/Agents/PostProcAgent/dev_docker.py` is provided as an example to demonstrate the operations. It operates on the same triple store specified in the `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/agent_properties.json`. Therefore, it can be used to test if the PostProc Agent deployed is functional as expected. **ALERT: developer should ONLY execute this script when the agent is operating on a triple store that has NO VALUABLE DATA in it - ALL TRIPLES will be DELETED once the script is executed.**

Before executing the script, the developer first need to decide the triple store and file server to be used for the demonstration. It can be either endpoints already deployed on the Internet or one may choose to deploy them by oneself at localhost, which can be done easily by building the image provided in `TheWorldAvatar/Agents/PostProcAgent/postprocagent/tests/docker-compose.yml`. Please remember also to populate the credentials in the folder `TheWorldAvatar/Agents/PostProcAgent/postprocagent/conf/credentials` using the exact information provided in the folder `TheWorldAvatar/Agents/PostProcAgent/postprocagent/tests/dummy_services_secrets`. **NOTE: spinning up the containers in this image requires access to the docker.cmclinnovations.com registry from the machine the test is run on. For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry**

If any endpoints deployed at localhost is used (either triple store or file server), the developer need to comment out below two lines in `TheWorldAvatar/Agents/PostProcAgent/docker-compose.yml` that were used for ports mapping:
```yml
ports:
 - 7000:5000
```
and use below line instead, which essentially uses the network of the host machine when deploying Docker image so that the deployed agent will be able to access the triple store and file server deployed at localhost (you can safely ignore these changes if you are accessing a remote triple store/file server for testing):
```yml
network_mode: host
```

Once the developer has confirmed NO valuable data are present in the triple store, one may run the script with below commands (You may experience a "SyntaxError: invalid syntax" - this is done intentionally to provide an extra layer of safeguard, please follow the instruction on console and please do NOT commit any changes you made to fix this SyntaxError, it will help to protect other developers):
```cmd
cd /your_absolute_path_to/TheWorldAvatar/Agents/PostProcAgent
python dev_docker.py
```

Once the script is executed, it first **DELETES ALL TRIPLES** in the specified SPARQL endpoint, it then SPARQL update all triples stated in below turtle files to the same endpoint:
```
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/ontoagent/Service__PostProc.ttl
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/new_exp_data.ttl
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/duplicate_ontorxn.ttl
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/dummy_lab.ttl
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/rxn_data.ttl
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/dummy_post_proc.ttl
```
and upload below sample HPLC raw report to the file server specified (the script also writes some triples for recording purpose to the triple store):
```
TheWorldAvatar/Agents/utils/chemistry-and-robots/chemistry_and_robots/resources/sample_data/raw_hplc_report_xls.xls
```

If the upload to file server is successfully, a series of information will be output to console and an IRI will be generated as the generated OntoHPLC:HPLCReport instance, for example `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416>` in below log messages:
```
2022-03-22 19:54:27,803 (STDOUT) HPLC raw report (7cbdd1df-cc44-439a-8dca-a95cb059ce9b.xls) was uploaded to fileserver <http://localhost:48086/FileServer/> with a response statue code 200 at 1647978867.603452
2022-03-22 19:54:27,805 (STDOUT) The remote file path of the new uploaded HPLCReport is: <http://localhost:48086/FileServer/7cbdd1df-cc44-439a-8dca-a95cb059ce9b.xls>
2022-03-22 19:54:27,805 (STDOUT) The initialised HPLCReport IRI is: <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416>; the initialised HPLCJob IRI is: <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCJob_256841ed-ad33-4d55-907f-490a9d05847c>
2022-03-22 19:54:38,021 (STDOUT) The identified ReactionExperiment for HPLCReport <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416> (remote path: http://localhost:48086/FileServer/7cbdd1df-cc44-439a-8dca-a95cb059ce9b.xls) is: <https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70>
2022-03-22 19:54:38,022 (STDOUT) The HPLCReport <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416> (remote path: http://localhost:48086/FileServer/7cbdd1df-cc44-439a-8dca-a95cb059ce9b.xls) was generated using HPLCMethod <http://example.com/blazegraph/namespace/testlab/dummy_lab/HPLCMethod_Dummy>
2022-03-22 19:54:38,994 (STDOUT) HPLCReport <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416> is connected to ChemicalSolution <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/ChemicalSolution_1_1>
```

The IRI of OntoHPLC:HPLCReport will then be used as the derivation inputs by the script to write below derivation related triples:
```
# derivation is an asynchronous derivation
<derivation> <rdf:type> <OntoDerivation:DerivationAsyn>

# derivation inputs and outputs
<derivation> <OntoDerivation:isDerivedFrom> <http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLCReport_d3806b3d-821f-4891-859b-4a96522de416>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_yield_1> <OntoDerivation:belongsTo> <derivation>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_conversion_1> <OntoDerivation:belongsTo> <derivation>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_eco_score_1> <OntoDerivation:belongsTo> <derivation>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_e_factor_1> <OntoDerivation:belongsTo> <derivation>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_sty_1> <OntoDerivation:belongsTo> <derivation>
<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_cost_1> <OntoDerivation:belongsTo> <derivation>

# agent related
<derivation> <OntoDerivation:isDerivedUsing> <http://www.theworldavatar.com/resource/agents/Service__PostProc#Service>

# timestamp of derivation
<derivation> <time:hasTime> <time>
<time> <rdf:type> <time:Instant>
<time> <time:inTimePosition> <unix_time>
<unix_time> <rdf:type> <time:TimePosition>
<unix_time> <time:hasTRS> <http://dbpedia.org/resource/Unix_time>
<unix_time> <time:numericPosition> 0
```

The timestamp of derivation inputs are also added in the similar fashion as derivation, but their `<time:numericPosition>` will be marked as the cureent time - the derivation is out-of-date and the script finally request an update of the derivation.

If everything is working as expected, you will now see an output on console should be expected similar to the one below (this might take a few minutes) (**Please make a note of the IRI in the response as `<createdDerivationInstance>`, you will need this for querying later**):
```
2022-03-22 19:54:46,832 (STDOUT) Initialised successfully, created derivation instance: <http://localhost:8080/ontolab/derivedAsyn_b70e671e-09c7-4121-842f-1c1c7801bb06>
```
As the derivation is initialised with a timestamp of 0 and the inputs are marked with a timestamp of current time, the derivation is outdated and will be marked as `Requested`. The update will be taken care of by PostProc Agent and the IRI of the suggested instance of `OntoRxn:PerformanceIndicator` will be generated and uploaded into the knowledge graph. The script will also check the timestamp of the created derivation to determine if the update is finished (i.e. a non-zero timestamp), during that process, you may see output messages like:
```
2022-03-22 19:55:55,110 (STDOUT) The current timestamp for the derivation <http://localhost:8080/ontolab/derivedAsyn_b70e671e-09c7-4121-842f-1c1c7801bb06> is 0
```
Meanwhile, you can check the progress by querying {`?performance_indicator` `OntoDerivation:belongsTo` `<createdDerivationInstance>`}:
```
PREFIX OntoDerivation:     <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontoderivation/OntoDerivation.owl#>
PREFIX OntoRxn:            <https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_Ontology/ontology/ontorxn/OntoRxn.owl#>

SELECT ?performance_indicator
WHERE {
  ?performance_indicator OntoDerivation:belongsTo <http://localhost:8080/ontolab/derivedAsyn_b70e671e-09c7-4121-842f-1c1c7801bb06> .
}
```
If the update was successful (this might take a few minutes as the `PERIODIC_TIMESCALE` was set as 120 seconds, you may want to change that to a smaller value for testing purpose), the results of above query will be changed from:
  | performance_indicator |
  | --------------------- |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_conversion_1>` |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_cost_1>` |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_e_factor_1>` |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_eco_score_1>` |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_sty_1>` |
  | `<http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/placeholder_yield_1>` |

to something similar to below in a few minutes:

  | performance_indicator |
  | --------------------- |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/Conversion_967deb19-b7bc-4b5b-a236-13df15a0d2ea>` |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/EcoScore_ed7d5c04-3388-4b34-9795-0685e2ece916>` |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/EnvironmentalFactor_a03c52ca-9404-4b71-9ef3-ba19e78c23a0>` |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/RunMaterialCost_9e97205c-7b59-497c-b5bc-187c212e53c4>` |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/SpaceTimeYield_0d5b724a-d31a-4200-af31-ae574d14be50>` |
  | `<https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/Yield_7d236fe9-89ce-4fc1-b12e-e427b73b9bba>` |

where the IRIs in `performance_indicator` column indicate the computed instances of `OntoRxn:PerformanceIndicator`.

Once the update is done, the script pulls the data back and conducts a few checks to verify the update, if all checks are passed, you will see an output message as:
```
2022-03-22 20:02:16,416 (STDOUT) All checks passed.
```

### Integration tests
If you would like to contribute to new features for the PostProc Agent, there are also an integration test to make sure the new features added do NOT break the original function. Before running the test, make sure you have docker-compose plugin for pytest installed in your virtual environment, which can be done via executing below command:
```cmd
pip install pytest-docker-compose
```

With Docker Desktop opened, you can run the test via:
```cmd
cd /your_absolute_path_to/TheWorldAvatar/Agents/PostProcAgent
pytest -s --docker-compose=./postprocagent/tests/docker-compose.yml
```

All the logging information will be printed in console and you will be notified about the results of the test cases.
