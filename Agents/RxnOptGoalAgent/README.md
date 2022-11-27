# Reaction Optimisation Goal (ROG) Agent
The folder contains the source, resource, and Docker setup files for the Reaction Optimisation Goal (ROG) Agent.


&nbsp;
## 1. Purpose
The Reaction Optimisation Goal (ROG) Agent is designed to take goal requests, monitor the progress in goal iterations, make decisions based on the latest results, visualise progress in goal iterations, and notify users about the status change throughout the process. It does so by translating the goal request to actionable ontological representations based on concepts defined in [`OntoGoal`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_Ontology/ontology/ontogoal). These expressions will then be picked up by [`RxnOptGoalIterAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalIterAgent) to orchestrate the actual performance of the reaction experiment.


&nbsp;
## 2. Build docker image
As an example, a development image together with its companion knowledge graph is provided in this folder and can be build via docker compose:

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/RxnOptGoalAgent/
docker-compose -f "docker-compose-dev.yml" up -d --build
```
Or, simply right click `docker-compose-dev.yml` file and select `Compose Up` option in Visual Studio Code.

To build and deploying a production Docker image, several key environment variables need to be set. For details about available parameters, one may refer to the docstring of `RxnOptGoalAgentConfig` class defined in `RxnOptGoalAgent/rxnoptgoalagent/conf/rxn_opt_goal_agent_conf.py`. Specifically, it should be noted that the `GOAL_ONTOAGENT_SERVICE_IRI` and `GOAL_ITER_AGENT_IRI` refer to **different** agent, where the former is the IRI for the goal agent itself and the latter being the `OntoAgent:Service` IRI of the [`RxnOptGoalIterAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/RxnOptGoalIterAgent) which the goal iteration job is delegated to. An example env file is provided as `RxnOptGoalAgent/agent.goal.env.example`. Developer may want to make a copy of such file and ammend values accordingly.

Once the requirements have been addressed, the Image already published on GitHub can be spun up via command line, one example of which is:

`(Linux)`
```sh
docker run -d -p 5000:5000 --env-file <agent_env_file> --add-host=host.docker.internal:host-gateway --name rog_agent ghcr.io/cambridge-cares/rxn_opt_goal_agent:<x.x.x>
```

> **NOTE** A [well-known issue](https://github.com/docker/for-win/issues/8861) for deploying docker container in Windows machine is the docker network connection time outs to host over time. Based on [this comment](https://github.com/docker/for-win/issues/8861#issuecomment-1305175614), a workaround for now is to set `vpnKitMaxPortIdleTime` from `300` to `0` in `%APPDATA%\Docker\settings.json` file and restart Docker Desktop. If `%APPDATA%` environment variable is not set up in your machine, you may want to change the view in `C drive` to show hidden items and look for path like `C:\Users\<your_user_name>\AppData\Roaming\Docker\settings.json`.


&nbsp;
## 3. How to use it

Once deployed, the ROG Agent offers a few web pages for interacting with the agent ecosystem at the backend. For illustration purpose, the agent is assumed to be deployed at `http://localhost:5000` and only route URL is provided for the remaining section. To use the agent in production, you may contact the administrator of the project repo for the deployed base URL.

### 3.1 Default page - `/`
A welcome page is provided at the root of agent and look like something below:

```
Welcome to the RxnOptGoalAgent!
This is a goal agent that capable of persure a reaction optimisation goal.
For more information, please visit https://github.com/cambridge-cares/TheWorldAvatar/tree/160-dev-rxn-opt-goal-agent/Agents/RxnOptGoalAgent#readme
```

### 3.2 Goal request - `/goal`
This page offers the core function of the ROG agent, i.e. goal request. Once loaded, the user is required to provide information for these aspects:
- `Chemical Reaction`: select the IRI of the chemical reaction to be optimised
- `First Goal`: specify one of the available performance indicators of the reaction experiment as the objective function for optimisation
- `Second Goal`: specify the second objective
- `Reaction Optimisation Goal Plan`: select the reaction optimisation plan to be used from the dropdown list
- `Restrictions`: specify the amount of reactions allowed and the deadline to stop all reactions
- `Laboratories`: tick the laboratories to be included for the optimisation

Once all the inputs are filled, the user can click the `Submit` button to fire the goal request. The agent then processes the provided information at route `/goal_specification`. An example request can be expressed in HTTP POST format with data as below:
```json
{
    "chem_rxn": "https://www.example.com/triplestore/testlab/chem_rxn/ChemRxn_1",
    "cycleAllowance": 6,
    "deadline": "2022-11-26T10:00:00",
    "first_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#Yield",
    "first_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresGreaterThan",
    "first_goal_num_val": 99,
    "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
    "rxn_opt_goal_plan": "http://www.theworldavatar.com/resource/plans/RxnOpt/rxnoptplan",
    "second_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#RunMaterialCost",
    "second_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresLessThan",
    "second_goal_num_val": 0.001,
    "second_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/poundSterlingPerKilogram",
    "labs": [
        "http://example.com/blazegraph/namespace/testlab/lab1/Laboratory_Dummy",
        "http://example.com/blazegraph/namespace/testlab/lab2/Laboratory_Dummy"
    ]
}
```

At the backend, ROG agent creates an instance of [`OntoGoal:GoalSet`](https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#GoalSet) that to be pursued by the ROGI agent. This is done via instantiating derivation instances of ROGI agent, which further creates other derivations to perform the reaction experiment. The ROG agent also adds a periodical job to monitor the created ROGI derivations, which will later determine if to enter the next iteration (i.e. request for new experiment) or end goal iteration depends on if the goal is met or any of the restrictions is no longer satisfied. In a moment, a response similar to one below will appear:

```json
{
    "Created a RxnOptGoalIter (ROGI) Derivation": [
        "http://www.theworldavatar.com/triplestore/repository/DerivationAsyn_a649298f-4af2-438e-8eec-dedfd235e213",
        "http://www.theworldavatar.com/triplestore/repository/DerivationAsyn_a2753927-0e73-4756-aa92-77aa43e5b5ce"
    ],
    "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#GoalSet": "http://www.theworldavatar.com/triplestore/repository/GoalSet_93547e14-ec9a-409b-97e1-7b47ce0a8146"
}
```

You may want to take a note of the created GoalSet IRI for later visualising the goal iteration progress.

### 3.3 Visualise goal iteration progress - `/goal/result`
A webpage is also provided to visualise the progress of goal iteration. The route URL will plot the progress if a GoalSet is currently active. However, a message like below will be returned if no results were obtained yet.

```
The current active GoalSet IRI http://www.theworldavatar.com/triplestore/repository/GoalSet_93547e14-ec9a-409b-97e1-7b47ce0a8146 is not iterated for the first time yet. Please wait for the first iteration to finish and try again.
```

If no instance of GoalSet is active, something similar to below will be returned with the default visualisation route.

```
No GoalSet IRI is provided. Nor is any GoalSet currently running. Please provide a GoalSet IRI in the URL, e.g. http://localhost:5000/goal/result?goal_set=http://www.theworldavatar.com/GoalSet/GoalSet_1
```

As explained in the above message, besides the current active GoalSet, one can always provide a previous active GoalSet IRI in the HTTP request to retrieve its results.

### 3.4 Reactivate existing GoalSet - `/goal/reactivate`
In case the goal iteration for a particular GoalSet is terminated, e.g. the restriction is no longer satisfied, or failure in hardware/software, one can use this page to reactivate the GoalSet after the issue is resolved. Once loaded, the user is required to provide information for these aspects:
- `Goal Set`: select the GoalSet to reactivate from the dropdown list
- `Restrictions`: input the new restrictions, one can leave any of them empty if the previous values is to be preserved

Please note that at the moment all labs from the previous optimisation campaign will be involved, should you need more flexible options, please contact the administrator of this project repo.

If the GoalSet is reactivated successfully, you will see something like below:

```json
{
    "status": "success",
    "message": "GoalSet <http://www.theworldavatar.com/triplestore/repository/GoalSet_93547e14-ec9a-409b-97e1-7b47ce0a8146> is reactivated."
}
```


&nbsp;
## 4. Agent integration test
As this agent activates a bunch of derivation agents that interacting with the knowledge graph and the real world automatically, extra care should be taken before any deployment. A few integration tests are provided in the `tests` repository to make sure everything is working as expected: in silico local test, in silico dockerised test, physical local test, and physical dockerised test. While all fixtures and utility functions are provided in the `conftest.py` file. Furthermore, a few relevant folders are provided in the tests folder.
- `dummy_services_secrets` folder: credential files for basic auth of the knowledge graph
- `env_files` folder: env files for all agents involved in the integration tests
- `test_triples` folder: test triples for dummy data

To perform the integration test with agent instances created locally in memory, one first need to set up the virtual envrionment following the below section.

### 4.1 Environment setup from scratch
As the operation of ROG Agent is supported by all other agents that actually orchestrate and perform the experiment, to develop and test the whole ecosystem, one also need to include those agents in the environment setup. In light of this, all agents across different project folders are sharing the same virtual environment, making Anaconda a more suitable candidate compared to the python [`venv`](https://docs.python.org/3.8/library/venv.html) as normally suggested for [`Derivation Agents`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DerivationAgentPythonExample). As per recommendation of [The World Avatar wiki](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment#windows), WSL2 and VS Code are used in this work. For a complete setup from scratch, please follow the below steps:

- Set up WSL2/VS Code/Docker, follow the steps [here](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment).
- Install below recommended VS Code extensions:
  - Remote - WSL
  - Docker
  - Exetension Pack for Java
  - Python Extension Pack
  - Markdown All in One
  - Turtle Language Server
- Get access to docker image registry, follow the steps [here](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry).
- Install build-essential in WSL2 (otherwise `GPy` will fail to build due to missing `gcc`)
    ```sh
    $ sudo apt update && sudo apt install build-essential
    ```
- Install Java 11 in WSL2 (this is required for using `py4jps` package)
    ```sh
    $ sudo apt update
    $ sudo apt install openjdk-11-jdk
    ```
- Install anaconda in WSL2, follow the steps [here](https://gist.github.com/kauffmanes/5e74916617f9993bc3479f401dfec7da).
- Set up conda environment for python packages
    ```sh
    $ conda create --name <venv_name> python=3.8
    $ conda activate <venv_name>
    ```
- Install python packages for ROG Agent
    ```sh
    # First install pythonnet which is required to communicate with FlowCommander
    (<venv_name>) $ conda install -c conda-forge pythonnet
    # Install docker and docker-compose that are required for interacting with docker containers
    (<venv_name>) $ python -m pip install docker docker-compose
    # cd into RxnOptGoalAgent folder and install its dependencies first
    # The pinned version in the `requirements.txt` ensures you get a tested working version
    # Remember to change <absolute_path_to> dependes on where you cloned the TWA repo
    (<venv_name>) $ cd /<absolute_path_to>/TheWorldAvatar/Agents/RxnOptGoalAgent
    (<venv_name>) $ python -m pip install -r requirements.txt
    # Then install the `rxnoptgoalagent` package as editable version for development
    # You may see a lot `Requirement already satisfied` as they are mostly covered already
    (<venv_name>) $ python -m pip install -e .[dev]
    ```
- Install python packages for all other agents
    ```sh
    # Assume now you are in the folder of `/TheWorldAvatar/Agents/RxnOptGoalAgent`
    # Start with `RxnOptGoalIterAgent` and move to all the rest to install editable version
    # You may see a lot `Requirement already satisfied` as they are mostly covered already
    (<venv_name>) $ cd ../RxnOptGoalIterAgent && python -m pip install -e .[dev]
    (<venv_name>) $ cd ../DoEAgent && python -m pip install -e .[dev]
    (<venv_name>) $ cd ../VapourtecScheduleAgent && python -m pip install -e .[dev]
    (<venv_name>) $ cd ../VapourtecAgent && python -m pip install -e .[dev]
    (<venv_name>) $ cd ../HPLCAgent && python -m pip install -e .[dev]
    (<venv_name>) $ cd ../HPLCPostProAgent && python -m pip install -e .[dev]
    ```

### 4.2 In silico test
Depends on if the agent instances are created in memory or deployed in docker containers, the tests are attached with an identifier `LOCAL` and `DOCKERISED` at the end of their function names.

#### 4.2.1 Local test
Local tests are separated in below files and focus on different aspect of the operation:
- `test_example_triples.py`: test if all prepared test triples are valid
- `test_goal_html.py`: test if the ROG Agent creates the ROGI derivation and adds periodical job upon receiving a goal request
- `test_rxn_goal_driven.py`:
  - `test_rxn_rogi_LOCAL` - ROGI agent orchestrates agents to perform experiment given derivation markup (leaving ROG agent out)
  - `test_rxn_goal_request_LOCAL` - all agents run one iteration upon a goal request
  - `test_rxn_goal_iterations_LOCAL` - all agents run multiple iterations upon a goal request
- `test_two_setup_in_silico.py`: test if two-setup can work in silico

To run local tests, one can perform below command (add `-s` to see live logs):

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/RxnOptGoalAgent/
# Run a specific test
pytest tests/test_rxn_goal_driven.py::test_rxn_rogi_LOCAL
# Run a test module
pytest tests/test_goal_html.py
# Run all local tests in one-go, note that this can take a while
pytest -k "LOCAL"
```

#### 4.2.2 Dockerised test
Dockerised test is provided in `test_rxn_opt_dockerised.py`, where all seven agents and knowledge graph (triple store and file server) are deployed in the same docker stack. Before running the test, developer should follow the below steps to get things ready:
- Depends on which lab you would like to test, change `env_file` of `vapourtec_agent` and `hplc_agent` in `docker-compose-test-dockerised.yml`, please refer to `docker-compose-test-dockerised.yml` for more details **(NOTE the lab setting in vapourtec_agent and hplc_agent MUST be the same)**
- Open `FlowCommander` instance on the Windows host machine

To run dockerised tests, one can perform below command (add `-s` to see live logs):

`(Linux)`
```sh
cd /your_absolute_path_to/TheWorldAvatar/Agents/RxnOptGoalAgent/
pytest tests/test_rxn_opt_dockerised.py --docker-compose=./docker-compose-test-dockerised.yml
```

### 4.3 Physical test
#### 4.3.1 Local test
The local integration test using physical equipment is provided in `test_rxn_lab_physical.py`. To run physical test in the lab, please follow below steps:
1. (**ONLY IF** you would like to receive email notifications about the agents operations) Set up email configuration in relevant `tests/env_files/*.env.test`, for details, see [here](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent#set-up-email-notification-for-exceptions)
2. Manually spin up docker containers in `tests/docker-compose.test.kg.yml` (this design prevents the test triples being deleted by the teardown function)
3. Open FlowCommander in Windows host machine, load the correct experiment file (`.fcexp`) - you may contact the maintainer of this repo to get it
4. Open HPLC software in Windows host machine, load the correct HPLC method, turn on the hardware, queue the analysis sequence, obtain the report folder path
5. Update the variable `lab_to_test` in `test_rxn_lab_physical.py` based on the lab involved in the physical test
6. Update the variable `hplc_report_target_folder` in `test_rxn_lab_physical.py` with the HPLC report folder path obtained from the HPLC software, e.g. `"C:\\Chem32\\1\\Data\\Optimization\\Optimization 2022-09-19 22-41-02"`, you may need to use the WSL2 specific path if the test is running in WSL2 environment, see more details in `test_rxn_lab_physical.py`
7. Retrieve the IP address for the Windows host machine, if running in Windows directly, you can use `localhost`, however, for those running in WSL2, the IP address can be obtained by executing below command (see [this post](for more details, see https://pscheit.medium.com/get-the-ip-address-of-the-desktop-windows-host-in-wsl2-7dc61653ad51) for more details):
    ```sh
    echo $(ipconfig.exe | grep 'vEthernet (WSL)' -A4 | cut -d":" -f 2 | tail -n1 | sed -e 's/\s*//g')
    ```
    Replace the `vapourtec_ip_address` variable in `test_rxn_lab_physical.py` with the obtained IP address
8. Run test via executing: `pytest -s tests/test_rxn_lab_physical.py`

> **NOTE** The whole physical test process can take ~45 minutes.

#### 4.3.2 Dockerised test
Coming soon...


&nbsp;
## 5. Upload docker image to GitHub

Developers who add new features to the `RxnOptGoa Agent` handle the distribution of the docker image on GitHub. If you want to add new features that suit your project and release the docker image independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your GitHub account and password ([personal access token](https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/creating-a-personal-access-token))
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine

### 5.1 Stable version release

The release process can be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!) **NOTE: the release process is only tested in WSL2 environment.**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/RxnOptGoalAgent
$ ./upload_docker_image_to_github.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, the change performed automatically during the release process should be commited, i.e., in python script `Agents/RxnOptGoalAgent/docker-compose.github.yml`
```
image: ghcr.io/cambridge-cares/rxn_opt_goal_agent:x.x.x
```

**NOTE: the visibility of the uploaded docker image is set as private by default, developer who uploaded the image need to change the package visibility to public manually after the upload.**

### 5.2 Snapshot version release

If you would like to release the package in SNAPSHOT version, below commands can be used intead:

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/RxnOptGoalAgent
$ ./upload_docker_image_to_github.sh -v x.x.x-SNAPSHOT
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, commit the change in version number following the same procedure as in the stable version release.


&nbsp;
# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
