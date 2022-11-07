# Derivation Agent Python Example


## Purpose
This agent folder is intended as a template that one can copy and adapt to turn their own Python code into a derivation agent. The example requires [`pyderivationagent`](https://pypi.org/project/pyderivationagent/)>=1.3.0.

This document covers four stages: development, test, publish and deployment. For each stage, a step-by-step instruction is provided. Before continuing with this tutorial, it is recommended to read the [documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent) of `pyderivationagent` and all the relevant links.

If you identified anything that can be improved to make it easier for newcomers, please feel free to open a [pull request](https://github.com/cambridge-cares/TheWorldAvatar/pulls) or get in touch with the maintainer of the package.


## Environment setup

For development and testing reasons, follow below instructions to get started.

### Virtual environment setup

It is highly recommended to install `pyderivationagent` packages in a [virtual environment](https://docs.python.org/3/tutorial/venv.html). The following steps can be taken to build a virtual environment:

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

The following command can be used to install the `pyderivationagent` package and `agentlogging` package. This is a workaround as PyPI does NOT allow `install_requires` direct links, so we could NOT add package `agentlogging` from `'agentlogging @ git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils'` as dependency of `pyderivationagent`. Therefore, in order to make the derivation agent working, we need to install `agentlogging` manually. A long term solution could be that we publish `agentlogging` in PyPI as well.

```sh
(<venv_name>) $ pip install pyderivationagent
(<venv_name>) $ pip install "git+https://github.com/cambridge-cares/TheWorldAvatar@main#subdirectory=Agents/utils/python-utils"
```

As `pyderivationagent` library relies on the `py4jps` package, Java 11 is required. For Windows, it is recommended to obtain OpenJDK 11 from [here](https://developers.redhat.com/products/openjdk/download) and follow the [instructions](https://access.redhat.com/documentation/en-us/openjdk/11/html-single/installing_and_using_openjdk_11_for_windows/index). For linux environment, one can install via:

`(Linux)`
```sh
$ sudo apt update
$ sudo apt install openjdk-11-jdk-headless
```


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


## Test

The derivation agent modifies the knowledge graph automatically, it is therefore recommended to run integration test before deploying it for production. The recommended setup is to have both the triple store and the agent spun up in the same docker stack. Such an example is `docker-compose.test.yml` file. Other relevant files are provided in the `tests` folder.

1. `dummy_services_secrets` folder: credential for blazegraph container used in test
2. `test_triples` folder: test triples for derivation inputs
3. `agent.env.test` file: agent configuration parameters
4. `conftest.py` for pytest: all pytest fixtures and other utility functions
5. `test_example_agent.py`
   - `test_example_agent.py::test_example_triples`: test if all prepared triples are valid
   - `test_example_agent.py::test_monitor_derivations`: test if derivation agent performs derivation update as expected, the `local_agent_test` parameter controls if the agent performing the update is instantiating in memory (for quick debugging) or deployed in docker container (to mimic the production environment), the detailed documentation are provided along with the codes

To run the test, one can execute below commands. To see live logs, one may want to add `-s` flag when calling pytest.

`(Linux)`
```sh
cd TheWorldAvatar/Agents/DerivationAgentPythonExample/
pytest --docker-compose=./docker-compose.test.yml
```

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


## Publish

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


## Deployment

Example of configurations for the agent are provided in `agent.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT`, with the credentials specified using `KG_USERNAME` and `KG_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The `OntoAgent:Service` IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations. To help monitoring the agent running status, an email notification feature is also provided and can be set up via `EMAIL_RECIPIENT`, `EMAIL_SUBJECT_PREFIX`, `EMAIL_USERNAME`, `EMAIL_AUTH_JSON_PATH` and `EMAIL_START_END_ASYNC_DERIVATIONS`. More details are provided in the [documentation](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent) of the `pyderivationagent` package. Developers needs to ensure that this file is correctly updated before deploying the Docker Image.

Once the env file is prepared, the docker image can be deployed via:

`(Linux)`
```sh
docker run --env-file <env_file_path> --name derivation_agent_python_example ghcr.io/cambridge-cares/derivation_agent_python_example:<x.x.x>
```


# Author

Jiaru Bai (jb2197@cam.ac.uk)
