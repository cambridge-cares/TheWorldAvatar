# Derivation Agent Python Example

## Purpose
This agent folder is intended as a template that one can copy and adapt to turn their own Python code into a derivation agent. The example requires [`pyderivationagent`](https://pypi.org/project/pyderivationagent/)>=1.4.1.

This document covers four stages: development, test, package & publish, and deployment. For each stage, a step-by-step instruction is provided. Before continuing with this tutorial, it is recommended to read the [documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent) of `pyderivationagent` and all the relevant links.

If you identified anything that can be improved to make it easier for newcomers, please feel free to open a [pull request](https://github.com/cambridge-cares/TheWorldAvatar/pulls) or get in touch with the maintainer of the package.


&nbsp;
## Environment setup

For development and testing reasons, follow below instructions to get started.

### Virtual environment setup

It is highly recommended to install `pyderivationagent` packages in a [virtual environment (python>=3.8)](https://docs.python.org/3/tutorial/venv.html). The following steps can be taken to build a virtual environment:

`(Windows)`

```cmd
$ python -m venv <venv_name>
$ <venv_name>\Scripts\activate.bat
(<venv_name>) $
```

`(Linux)`
```sh
$ python3 -m venv <venv_name>
$ source <venv_name>/bin/activate
(<venv_name>) $
```

The above commands will create and activate the virtual environment `<venv_name>` in the current directory.

### Package installation

The following command can be used to install all required packages.

`(Linux)`
```bash
(<venv_name>) $ python -m pip install --upgrade pip
# Install all required packages, incl. pyderivationagent, pytest etc.
# NOTE instead of the loosely constrained versions defined in the setup.py
#   here packages in the requirements.txt with the pinned version are installed
# This ensures the user getting a tested version of the agent
# However, one can skip this line and execute the next command directly
#   where pip will pick up the versions automatically
(<venv_name>) $ python -m pip install -r requirements.txt
# Install the agent package itself for development purpose
# If the previous command executed, pip will skip all the "install_requires"
#   as "Requirement already satisfied"
(<venv_name>) $ python -m pip install -e .[dev]
```

As `pyderivationagent` library relies on the `py4jps` package, Java 11 is required. For Windows, it is recommended to obtain OpenJDK 11 from [here](https://developers.redhat.com/products/openjdk/download) and follow the [instructions](https://access.redhat.com/documentation/en-us/openjdk/11/html-single/installing_and_using_openjdk_11_for_windows/index). For linux environment, one can install via:

`(Linux)`
```sh
$ sudo apt update
$ sudo apt install openjdk-11-jdk-headless
```


&nbsp;
## Development

The development of a derivation agent is strongly related to the design of ontologies for your application. It is thus advised to first consolidate the concepts/relationships of the data flow in your application before advancing to agent development. Below are the changes needed to adapt this template for your application. You may find more information in the in-line documentation of the code.

1. Data model
   - Put all concept and relationship IRIs in `iris.py`, you may import `pyderivationagent.data_model.iris` to re-use common IRIs used in TheWorldAvatar project.
   - (If you wish to use [`pydantic`](https://github.com/pydantic/pydantic) for data validation) Put all data classes for your application in `onto.py`, this should in line with the concepts and relationships defined in `iris.py`.
2. Sparql client
   - Provide your own sparql client class by extending `pyderivationagent.kg_operations.sparql_client.PySparqlClient`. Also, put all your SPARQL query/update functions in the class.
3. Additional configuration (ignore this if you only need basic configuration from `pyderivationagent.conf.AgentConfig`)
   - Extend `pyderivationagent.conf.AgentConfig` class and put all the additional parameters in all capital letters.
   - Provide your own wrapper function of `pyderivationagent.conf.config_generic()` (e.g. `config_your_agent`) to return instance of your custom config class.
4. Agent class
   - Familiarise yourself with available agent parameters by going through the `pyderivationagent.DerivationAgent.__init__()` function. They should more or less match the fields in `pyderivationagent.conf.AgentConfig`.
   - Override `__init__` method with `self` and `**kwargs` as input arguments, then
     - (If applicable) Add additional config parameters to input arguments (between `self` and `**kwargs`).
     - Call `super().__init__(**kwargs)` as the first line.
     - Initialise `self.sparql_client` using the `self.get_sparql_client(sparql_client_cls)` method, with the input being the class of your custom sparql client.
   - Override `agent_input_concepts(self)` and `agent_output_concepts(self)` methods to return a list of full IRIs for the OntoAgent Input/Output signature of the agent.
   - Override `process_request_parameters` method which contains the agent logic converting derivation inputs to outputs. In the example agent, there are four blocks in this method, it is recommended to follow the same design for your own agent.
   - (Optional) Provide a function to return an instructional message at the agent flask app root.
5. Entry point for dockerised agent
   - Retrieve configuration object by calling either `pyderivationagent.conf.config_derivation_agent()` or your specific configuration method (e.g. `config_your_agent()`).
   - Create an agent instance for your agent class. You can access configuration parameters from the configuration object just retrieved.
   - Add instructional root page to agent's flask app route if applicable. You may add more pages for other information.
   - You can keep all the rest the same as the example.


&nbsp;
## Test

The derivation agent modifies the knowledge graph automatically, it is therefore recommended to run integration test before deploying it for production. The recommended setup is to have both the local agent integration test and dockerised agent integration test. The former spins up a triple store and creates an agent instance in memory. Whereas the latter spins up a docker container of agent, which itself further spins up a triple store and run pytest within the agent docker container.

> **WARNING** Everything in the example test was developed and tested working fine in WSL2. However, the test will ran into issue `ERROR: for blazegraph  Cannot create container for service blazegraph: invalid mount config for type "bind": bind source path does not exist: /tests/dummy_services_secrets/blazegraph_passwd.txt` when spinning up dockerised test in Windows host directly. It seems when running in Windows, the credential files were added executable permission when copying `.tests` folder into the docker container. However, at the time of writing, removing the executable permission doesn't seem to resolve the issue. Given that the recommended way of developing docker containers on Windows, according to [The World Avatar wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment#windows), is through WSL2, this test example will remain as it is for the moment. For those prefer to develop in Windows host directly, please refer to [PropertyValueEstimationAgent](https://github.com/cambridge-cares/TheWorldAvatar/tree/606def74ec3b921e3c451767b78458db3a2fa3d5/Agents/PropertyValueEstimationAgent) as a concrete working example.

### Local agent integration test
This example is provided in `docker-compose-testcontainers.yml` file. Other relevant files are provided in the `tests` folder.

1. `dummy_services_secrets` folder: credential for blazegraph container used in test, and potentially auth json file for email services (for more information on this, please refer to the official documentation of [`pyderivationagent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent))
2. `test_triples` folder: test triples for derivation inputs (example ABox), and example TBox where relevant concepts and relationships are defined
3. `agent.env.test` file: agent configuration parameters
4. `conftest.py` for pytest: all pytest fixtures and other utility functions
5. `test_example_agent.py`
   - `test_example_agent.py::test_example_triples`: test if all prepared triples are valid
   - `test_example_agent.py::test_monitor_derivations`: test if derivation agent performs derivation update as expected, the `local_agent_test` parameter controls if the agent performing the update is instantiating in memory (local agent integration test, for quick debugging) or deployed in docker container (dockerised agent integration test, to mimic the production environment), the detailed documentation are provided along with the codes

To run the test, one can execute below commands. To see live logs, one may want to add `-s` flag when calling pytest.

`(Linux)`
```sh
cd TheWorldAvatar/Agents/DerivationAgentPythonExample/
python -m pytest --docker-compose=./docker-compose-testcontainers.yml
```

### Dockerised agent integration test

The dockerised tests use one Docker container to initialise the Derivation agent and run pytest, and use Docker in Docker to spin up the required Blazegraph instances:

`(Linux)`
```sh
# Build and run Dockerised agent test
docker compose -f "docker-compose-test-dockerised.yml" up -d --build
```

To run the dockerised tests in Debug mode, `docker-compose-test-dockerised-debug.yml` and a suitable `.vscode/launch.json` configuration `Python: Test Dockerised Debug (WSL2)` have been provided. It should be noted that once the configuration is selected, one need to **press `F5` TWICE** to start debugging:
- Pressing it for the first time to compose up the agent docker, this will also start the pytest (which spins up a blazegraph container). However, due to the flag `--wait-for-client` in the `Dockerfile`, the docker will wait for a debugger to be attached to it, i.e. the dockerised agent will NOT start and therefore NOT picking up derivation markup created in the test script. If you look for logs inside the docker containers, you may see the docker just hanging there with the last line of logging being something similar to `2022-11-24 08:34:43,250 (STDOUT) Initialised successfully, created asynchronous derivation instance: http://www.example.com/triplestore/repository/DerivationAsyn_e7c61229-4691-4b4a-88e0-a7bca7246a0d`
- Then press for the second time to attach the debugger to the container. The dockerised agent will now start to process derivations. The logs in the container will continue and you may see something similar to `* Serving Flask app 'app/derivationagentpythonexample:create_app()' (lazy loading) * Environment: production   WARNING: This is a development server. Do not use it in a production deployment.   Use a production WSGI server instead. * Debug mode: on`. Remember to add a few breakpoints in the agent logic to see debug taking effect.

Once the test is finished, the docker container will be composed down automatically.

For developers new to `Run and Debug` configurations, please refer to these official documentations:
- [Debug Python within a container](https://code.visualstudio.com/docs/containers/debug-python)
- [Customize the Docker extension](https://code.visualstudio.com/docs/containers/reference)
- [Use Docker Compose: Debug](https://code.visualstudio.com/docs/containers/docker-compose#_debug)
- [Debugpy](https://github.com/microsoft/debugpy)

For developers interested to see more example of possible configurations, including those relevant to the usage of [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager), please refer to [`PropertyValueEstimationAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent).

### Develop tests for new agents

To develop new agents, it is recommended to follow the same test structure provided in this example.

It should be noted that `pyderivationagent` utilise the functions provided in `jps-base-lib` via `py4jps`. Therefore, in case one need to use new functions in a version of `jps-base-lib` that is NOT yet released as part of `py4jps`, developer may build it by oneself from the `JPS_BASE_LIB` in the branch where the new functions are developed. Given that (1) you are at correct branch `<your_branch_with_new_functions>` (the one contains your new functions), (2) maven is correctly installed on your machine, and most importantly, (3) you provided the correct credentials to access the GitHub in your `.m2` settings file (read more [here](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Packages)), the build can be done using below commands:

```cmd
git pull
git checkout <your_branch_with_new_functions>
cd TheWorldAvatar/JPS_BASE_LIB
mvn clean install -DskipTests
```

Developers then need to copy the `jps-base-lib.jar` file and folder `lib/` generated in folder `TheWorldAvatar/JPS_BASE_LIB/target/` and paste them into the `_temp` folder of your agent project folder. The update of the `JpsBaseLib` package in `py4jps` is taken care of by below lines of code in the Dockerfile:

```Dockerfile
# Re-install the version of JPS_BASE_LIB that is been developing
# (this will be required if the new features are not merged back to main)
RUN jpsrm uninstall JpsBaseLib
RUN mkdir /jpstemp
COPY /_temp/jps-base-lib.jar ./jpstemp/jps-base-lib.jar
COPY /_temp/lib ./jpstemp/lib
RUN jpsrm install JpsBaseLib ./jpstemp/
```

At the moment, above lines are commented out in the Dockerfile. One may bring them back if a specific version of `jps-base-lib` is required and provided.


&nbsp;
## Package & Publish

Once you have a working version of agent, it is recommended to wrap the agent as a python package and publish it for production use. For this purpose, `setup.py` is provided as an example. You may refer to its in line documentation for more information. The recommended way of publishing the agent is to upload a tested docker image to GitHub.

> **NOTE** The design of separating `setup.py` and `requirements.txt` in this example is an effort to avoid dependency conflict. The pinned versions of dependencies in the `requirements.txt` are the same versions that passed the integration tests. This file is called in the `Dockerfile` for publishing the production docker image.

> **NOTE** By design, requirements files and setup script are for different purposes. The former tends to be as specific as possible for reproducibility and production, whereas the latter normally gives a wide range to not limit the choice of packages in development. If you let the `pip` decide the version of packages, there's a chance it pulls a version of a package that potentially breaks the other packages. For more information, see https://packaging.python.org/en/latest/discussions/install-requires-vs-requirements/


The recommended way of publishing the agent is to upload a tested docker image to GitHub. In this example, the docker image is pushed to [Cambridge CARES GitHub package](https://github.com/orgs/cambridge-cares/packages) under the name of `ghcr.io/cambridge-cares/`. Currently the release procedure is semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/DerivationAgentPythonExample
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/DerivationAgentPythonExample/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/derivation_agent_python_example:x.x.x
```

**NOTE: (1) in case you ran into `bash: ./upload_docker_image_to_github.sh: Permission denied` error, you may want to run `chmod +x ./upload_docker_image_to_github.sh` and commit the corresponding change to git; (2) the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload, see [here](https://docs.github.com/en/packages/learn-github-packages/configuring-a-packages-access-control-and-visibility#configuring-visibility-of-container-images-for-an-organization).**

### Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/DerivationAgentPythonExample
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number and change the package visibility to public following the same procedure as in the stable version release.

### Modify for your agent

To release your agent, you may want to update information in `Dockerfile` and `docker-compose.github.yml` (see in-line documentation in those files), also update the `AUTHOR` and `AGENT_NAME` variables in the the `upload_docker_image_to_github.sh` script accordingly.


&nbsp;
## Deployment

Example of configurations for the agent are provided in `agent.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT`, with the credentials specified using `KG_USERNAME` and `KG_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The `OntoAgent:Service` IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations. To help monitoring the agent running status, an email notification feature is also provided and can be set up via `EMAIL_RECIPIENT`, `EMAIL_SUBJECT_PREFIX`, `EMAIL_USERNAME`, `EMAIL_AUTH_JSON_PATH` and `EMAIL_START_END_ASYNC_DERIVATIONS`. More details are provided in the [documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent) of the `pyderivationagent` package. Developers needs to ensure that this file is correctly updated before deploying the Docker Image.

Once the env file is prepared, the docker image can be deployed via:

`(Linux)`
```sh
docker run --env-file <env_file_path> --name derivation_agent_python_example ghcr.io/cambridge-cares/derivation_agent_python_example:<x.x.x>
```


&nbsp;
## Adapt agent to work with stack
> **NOTE** This agent example will be updated to incorporate Stack in the next iteration.

This agent example has been adapted to work with a Docker stack spun up by the [Stack Manager](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/dynamic/stack-manager) for a real use-case. For more information, please refer to [`PropertyValueEstimationAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/PropertyValueEstimationAgent).


&nbsp;
# Author

Jiaru Bai (jb2197@cam.ac.uk)
