# Description #

The `chemistry_and_robots` package provides a collection of dataclasses and SPARQL query/update functions that are used by a series of agents capable of conducting automated reaction experiments as part of [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project. `chemistry_and_robots` uses `pyderivationagent>=1.1.0` to access `PySparqlClient` provided in `pyderivationagent.kg_operations` to form its SPARQL query/update utilities. For technical details, below are a few useful links:
- [`pyderivationagent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_derivation_agent) - python wrapper for derivation agent
- [`py4jps`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/JPS_BASE_LIB/python_wrapper) - python wrapper for jps-base-lib

# Installation #
For development and testing reasons, follow below instructions to get a copy of the project up and running on your local system.

## Virtual environment setup

It is highly recommended to install `chemistry_and_robots` packages in a [virtual environment](https://docs.python.org/3/tutorial/venv.html). The following steps can be taken to build a virtual environment:

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

## Installation via pip

The following command can be used to install the `chemistry_and_robots` package.

```sh
(<venv_name>) $ pip install chemistry_and_robots
```

# How to use it #

## Ontology data model
This package provides ontological data models from five main ontologies, namely:

 - `OntoReaction`: ontology for chemical reaction
 - `OntoDoE`: ontology for design of experiment
 - `OntoLab`: ontology for laboratory
 - `OntoVapourtec`: ontology for vapourtec flow chemistry hardware
 - `OntoHPLC`: ontology for HPLC analysis equipment

All of the concepts are directly or indirectly inherited from the `BaseOntology` class which itself is inherited from `pydantic.BaseModel`. The design of these data model classes serve as a persistence layer between the agent operations in chemistry_and_robots and the data stored in the knowledge graph. Additionally, all TBox IRIs involved in the chemistry_and_robots as part of The World Avatar project are provided in the `chemistry_and_robots.data_model.iris.py`. Developer can import this module to make use of the concepts and relationships.

## SPARQL client
A SPARQL client class `chemistry_and_robots.kg_operations.sparql_client.ChemistryAndRobotsSparqlClient` is provided as part of this package. It provides a few SPARQL query and update functions that are helpful in handling data instantiated using the above ontology data models. These functions have been used to develope a few python agents, for more details, please refer to: [`DoEAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/DoEAgent), [`VapourtecExecutionAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecExecutionAgent), [`HPLCPostProAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCPostProAgent), [`VapourtecAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/VapourtecAgent), and [`HPLCAgent`](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Agents/HPLCAgent).

## Test
Unit and integration tests are written for this package. The tests should pass if you already correctly setup the [Docker Environment](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Environment) and obtained access to [Docker Image Registry](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry). To run tests, please execute below commands (remember to replace the `<your_absolute_path_to>` with actual path):

`(Linux)`
```sh
cd /<your_absolute_path_to>/TheWorldAvatar/Agents/utils/chemistry_and_robots
pytest -s --reruns 5 --reruns-delay 5
```

# New features and package release #

Developers who add new features to the `chemistry_and_robots` package handle the distribution of the package on PyPI and Test-PyPI. If you want to add new features that suit your project and release the package independently, i.e. become a developer/maintainer, please contact the repository's administrator to indicate your interest.

The release procedure is currently semi-automated and requires a few items:

- Your Test-PyPI and PyPI account and password
- The version number x.x.x for the release
- Clone of `TheWorldAvatar` repository on your local machine
- Docker-desktop is installed and running on your local machine
- You have access to the docker.cmclinnovations.com registry on your local machine, for more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry

## Stable version release
For a stable version release, please create and checkout to a new branch from your feature branch once you are happy with the feature and above details are ready. The release process can then be started by using the commands below, depending on the operating system you're using. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!)

WARNING: at the moment, releasing package from Windows OS has an issue that the package will not be installed correctly while executing "$PIPPATH --disable-pip-version-check install -e $SPATH"[dev]"" in install_script_pip.sh. Please use Linux to release future versions before the issue is fixed.

`(Windows)`

```cmd
$ cd \<absolute_path_to>\TheWorldAvatar\Agents\utils\chemistry_and_robots
$ release_chemistry_and_robots_to_pypi.sh -v x.x.x
```

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/utils/chemistry_and_robots
$ ./release_chemistry_and_robots_to_pypi.sh -v x.x.x
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, change the version number in `Agents/utils/chemistry_and_robots/release_chemistry_and_robots_to_pypi.sh` to the one you used for the script release.
```sh
echo "./release_chemistry_and_robots_to_pypi.sh -v 0.0.1   - release version 0.0.1"
```

The changes mentioned above should then be committed with the changes performed automatically during the release process, specifically in python script `Agents/utils/chemistry_and_robots/chemistry_and_robots/__init__.py`
```
__version__ = "0.0.1"
```

and `Agents/utils/chemistry_and_robots/setup.py`
```
version='0.0.1',
```

Finally, merge the release branch back to the feature branch and make a Pull Request for the feature branch to be merged back into the `main` branch.

## Development version release
For development version release, you may do it in your feature branch. The development package will be released to TestPyPI repository by default. Once you have collected the required information, the release process can then be started by using the commands below. (REMEMBER TO CHANGE THE CORRECT VALUES FOR `<absolute_path_to>` IN THE COMMANDS BELOW!)

**NOTE: The development release requires the version number to contain a, b or rc (alpha, beta or release candidate) to be compatible with the pre-release configuration on TestPyPI. You may provide number at the end to differentiate different development version in the same pre-release stage, e.g., "1.1.0a1". In case no number is provided in the end (i.e., "1.0.0a"), "0" will be appended automatically to make it "1.0.0a0". For example, see [prerelease-example](https://pypi.org/project/prerelease-example/#history).**

`(Linux)`
```sh
$ cd /<absolute_path_to>/TheWorldAvatar/Agents/utils/chemistry_and_robots
$ ./release_chemistry_and_robots_to_pypi.sh -d x.x.xa
```

Please follow the instructions presented in the console once the process has begun. If everything goes well, change the version number and commit changes following the same procedure as in the stable version release, but no pull request is required.

# Authors #

Jiaru Bai (jb2197@cam.ac.uk)
