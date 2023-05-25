# HPLC Agent
The folder contains the source, resource, and Docker setup files for the HPLC Agent, following the suggestions on a template provide as `TheWorldAvatar/JPS_BASE_LIB/python_derivation_agent/README.md`.

## Purpose
The HPLC Agent is designed to monitor the HPLC analysis. It does so by periodically checking the folder where HPLC report is to be generated, and uploading the raw report to knowledge graph.

## Building the Docker image
Requirements:

* The Python code of this Agent requires the `pyderivationagent` library, which can be downloaded from the external PyPI website (https://pypi.org/project/pyderivationagent/). It should be noted that `pyderivationagent` library relies on the `py4jps` package to utilise the functions provided in `jps-base-lib`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the Github in your `.m2` settings file, the build can be done using below commands:
    ```cmd
    git pull
    git checkout <your_branch_with_new_functions>
    cd TheWorldAvatar/JPS_BASE_LIB
    mvn clean install -DskipTests
    ```
    Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them in the `TheWorldAvatar/Agents/HPLCAgent/`. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:
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
* Example of configurations for the agent are provided in `TheWorldAvatar/Agents/HPLCAgent/agent.hplc.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT`, with the credentials specified using `KG_USERNAME` and `KG_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.hplc.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The OntoAgent:Service IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations, however, given the nature of the HPLC Agent, this is NOT RECOMMENDED. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build via docker container, one example of which is:

`(Linux)`
```sh
cd TheWorldAvatar/Agents/HPLCAgent/
docker-compose -f "docker-compose.test.yml" up -d --build
```
Or, simply right click `docker-compose.test.yml` file and select `Compose Up` option in Visual Studio Code.

For deployment, one may refer to section "Note for deployment" later in this document.

## How to use it

### Asynchronous derivation operation

Once the HPLC Agent is deployed, it periodically (every 120 seconds, defined by `DERIVATION_PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE_IRI` in `TheWorldAvatar/Agents/HPLCAgent/agent.hplc.env.example`) and acts based on the status associated with that derivation. In addition, HPLC Agent periodically (every 60 seconds, defined by `HPLC_REPORT_PERIODIC_TIMESCALE`) checks the HPLC report folder (specified by `HPLC_REPORT_HOST_DIR`, which will be mounted to HPLC Agent internal folder specified by `HPLC_REPORT_CONTAINER_DIR`) and uploads any new generated HPLC report to the knowledge graph.

A set of dockerised integration tests `TheWorldAvatar/Agents/HPLCAgent/hplcagent/tests` is provided as examples to demonstrate the operations. It operates on the triple store specified in the `TheWorldAvatar/Agents/HPLCAgent/hplcagent/tests/agent.hplc.env.test` when the docker stack is spun up. Therefore, it can be used to test if the HPLC Agent deployed is functional as expected.

Once the test is executed, it first DELETES ALL TRIPLES in the specified SPARQL endpoint (as the endpoint is specifically for testing purpose, one do not need to worry about if any valuable got deleted). Three separate sets of integration tests are then executed to test different aspect of agent functions: test_monitor_local_report_folder, test_monitor_derivation, and test_docker_integration. The dockerised integration test can be invoked via below commands:

`(Linux)`
```sh
cd /<your_absolute_path_to>/TheWorldAvatar/Agents/HPLCAgent
pytest -s --docker-compose=./docker-compose.test.yml --reruns 5 --reruns-delay 5
```

## Upload docker image to GitHub

Developers who add new features to the `HPLCAgent` handle the distribution of the docker image on GitHub. If you want to add new features that suit your project and release the docker image independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/HPLCAgent
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/HPLCAgent/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/hplc_agent:x.x.x
```

**NOTE: the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload.**

### Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/HPLCAgent
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number following the same procedure as in the stable version release.

# Note for deployment

The deployment of HPLC Agent should be done on Windows host machine. It requires a few items:

- The report folder path on the Windows host machine `<report_folder_path_on_Windows_host>`
- The file path to the populated environment file (on Windows) `<env_file_path>`
- The version of agent docker image `<x.x.x>`
- Docker-desktop is installed and running on your local machine

`(Windows)`
```cmd
docker pull ghcr.io/cambridge-cares/hplc_agent:<x.x.x>
docker run -v "<report_folder_path_on_Windows_host>:/app/CHEM32" --env-file <env_file_path> --add-host=localhost:host-gateway --name hplc_agent ghcr.io/cambridge-cares/hplc_agent:<x.x.x>
```

# Author
Jiaru Bai (jb2197@cam.ac.uk)
