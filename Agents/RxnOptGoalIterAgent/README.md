# Reaction Optimisation Goal Iteration (ROGI) Agent
The folder contains the source, resource, and Docker setup files for the Reaction Optimisation Goal Iteration (ROGI) Agent, following the suggestions on a template provide as `TheWorldAvatar/JPS_BASE_LIB/python_derivation_agent/README.md`.


## Purpose
The Reaction Optimisation Goal Iteration (ROGI) Agent is designed to perform iterations of reaction experiment as part of goal-driven reaction optimisation exercise. It operates based on concepts defined in [`OntoGoal`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontogoal) and orchestrates [`DoE Agent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DoEAgent), [`VapourtecSchedule Agent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecScheduleAgent), [`Vapourtec Agent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecAgent), [`HPLC Agent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCAgent), and [`HPLCPostPro Agent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCPostProAgent) to complete one iteration.


## Building the Docker image
Requirements:
* Example of configurations for the agent are provided in `TheWorldAvatar/Agents/RxnOptGoalIterAgent/agent.goal.iter.env.example` file. The knowledge graph endpoints used by this agent are specified using `SPARQL_QUERY_ENDPOINT` and `SPARQL_UPDATE_ENDPOINT` for triple store, and `FILESERVER_URL` for the file server. The credentials for knowledge graph endpoints, i.e. triple store and file server, should be provided in the same file using `KG_USERNAME`, `KG_PASSWORD`, `FILE_SERVER_USERNAME`, `FILE_SERVER_PASSWORD`. To avoid commit these information to git at deployment, developer may make a copy of this example file as `agent.goal.iter.env`. As `*.env` entry already exist in `.gitignore`, this new created file will be omitted. Any credentials encoded are safe. The OntoAgent:Service IRI of the agent is specified using `ONTOAGENT_SERVICE_IRI`. The periodically time interval to monitor asynchronous derivation is specified by `DERIVATION_PERIODIC_TIMESCALE`. One may also provide `DERIVATION_INSTANCE_BASE_URL` to be used by DerivationClient when creating derivations related instances. `ONTOAGENT_OPERATION_HTTP_URL` can be used to specify the URL of the agent that listens the request for updating synchronous derivations, however, given the nature of the post processing Agent, this is NOT RECOMMENDED. Developers needs to ensure that this file is correctly updated before building the Docker Image.

Once the requirements have been addressed, the Image can be build via docker container, one example of which is:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/RxnOptGoalIterAgent/
docker-compose -f "docker-compose.test.yml" up -d --build
```
Or, simply right click `docker-compose.test.yml` file and select `Compose Up` option in Visual Studio Code.


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


## How to use it

The ROGI Agent is intended to use the `asychronous mode` of the Derivation Framework to detect changes in instantiated OntoGoal properties (e.g. `goal`, `best resutl`, and `restriction`) to automatically request the design/schedule/execution/analyses of a reaction experiment instances in the KG. As the agent adopts the `pyderivationagent`, it also serves HTTP requests to handle synchronous derivations. However, it is (strongly) discouraged to invoke such HTTP request by ONESELF.

After successful agent start-up, an instructional page shall become available at the root (i.e. `/`) of the port specified in the docker compose file. The exact address depends on where the agent container is deployed (i.e. localhost, remote VM, ...), but takes a form like `http://165.232.172.16:7060/`.

### Asynchronous derivation operation
Once the Agent is deployed, it periodically (defined by `DERIVATION_PERIODIC_TIMESCALE`) checks the derivation that `isDerivedUsing` itself (parameter `ONTOAGENT_SERVICE_IRI`) and acts based on the status associated with that derivation. Over time, you may see more information about a reaction experiment is added to the knowledge graph gradually.

### Prior derivation markup

For the Agent to detect outdated information, a proper mark up of the relevant derivation inputs (i.e. *pure* inputs) is required. (Please note, that another pre-requisite for detecting derivation inputs is the registration of the agent in the KG, i.e. `REGISTER_AGENT=true` in the env file.) The following methods from the `pyderivationagent` package shall be used to mark up derivation inputs within the KG (for illustration purposes only):
```bash
# Retrieve derivation client from derivation agent
deriv_client = agent.derivation_client
# Alternatively create new derivation client using:
# deriv_client = pyderivationagent.PyDerivationClient(...)

# Using pyderivationagent>=1.3.0, the timestamp for pure inputs will be added automatically when marking up the derivations
# Hence, no need to add them separately (just for reference here)
#deriv_client.addTimeInstance(inputIRI)

# Update time stamp to all pure input instances (i.e. inputIRIs)
deriv_client.updateTimestamp(inputIRI)

# Create (flat!) list of all pure inputs (i.e. inputIRIs)
deriv_inputs = [goal_set_IRI, lab_IRI, chem_rxn_IRI, rxn_exp_IRI1, rxn_exp_IRI2, ...]

# Create derivation markup in KG
deriv_iri = deriv_client.createAsyncDerivationForNewInfo(agent.agentIRI, deriv_inputs)
```


## Agent integration test

As this derivation agent modifies the knowledge graph automatically, it is recommended to run integration tests before deploying it for production. A few integration tests are provided in the `tests` repository.

The dockerised integration test can be invoked via below commands:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/RxnOptGoalIterAgent
pytest -s --docker-compose=./docker-compose.test.yml --reruns 5 --reruns-delay 5
```

If you would like to contribute to new features for the ROGI Agent, you may use the same integration test to make sure the new features added do NOT break the original function.


## Upload docker image to GitHub

Developers who add new features to the `RxnOptGoalIter Agent` handle the distribution of the docker image on GitHub. If you want to add new features that suit your project and release the docker image independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/RxnOptGoalIterAgent
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/RxnOptGoalIterAgent/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/rxn_opt_goal_iter_agent:x.x.x
```

**NOTE: the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload.**

### Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/RxnOptGoalIterAgent
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number following the same procedure as in the stable version release.


# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
