# PubChem Agent

## Description

The `PubChem Agent` is a simple API to communicate with The World Avatar (TWA) knowledge graph to query for the requested information and autopopulate the missing data using the PubChem PUG REST API using the OntoSpecies ontology.

## Installation

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Requirements

- You need Python > 3.9 to run the `PubChem Agent`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version >=8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

### Virtual environment setup

It is highly recommended to use a [virtual environment](https://docs.python.org/3/tutorial/venv.html) for the `PubChem agent` installation. The virtual environment can be created as follows:

`(Windows)`

```cmd
$ python -3.9 -m venv pubchemagent_venv
$ pubchemagent_venv\Scripts\activate.bat
(pubchemagent_venv) $ 
```

`(Linux)`

```sh
$ python3 -m venv pubchemagent_venv
$ source pubchemagent_venv\bin\activate
(pubchemagent_venv) $
```

The above commands will create and activate the virtual environment `pubchemagent_venv` in the current directory.

### Installation from the version-controlled source (for developers)

This type of installation is only for the developers. To install `PubChem agent` directly from its repository you need to first clone the `TheWorldAvatar` project. Then simply navigate to the *TheWorldAvatar\Agents\PubChemAgent* directory and execute the following commands:

```bash
# build and install
(pubchemagent_venv) $ python -m pip install .

# or build for in-place development
(pubchemagent_venv) $ python -m  pip install -e .
```

Alternatively, use the provided `install_script_pip.sh` convenience scripts, that can create virtual environment and install the `PubChemAgent` in one go:

```bash
# create the environment and install the project
$ install_script_pip.sh -v -i
# create the environment and install the project for in-place development
$ install_script_pip.sh -v -i -e
```

Note that installing the project for in-place development (setting the `-e` flag) also installs the required python packages for development and testing. To test the code, simply run the following commands:

```bash
(pubchemagent_venv) $ pytest tests\test_pubchemagent.py
```

## Build and deploy the agent with Docker

### Environment variables

The dockerised agent can be deployed as standalone version (i.e. outside a larger Docker stack) or deployed to an (existing) stack. Several key environment variables need to be set in the Docker compose file:

```docker
# Required environment variables for both Stack and "standalone" (i.e. outside stack) deployment
- STACK_NAME=TEST         # to be left blank for "standalone" deployment
# Additional environment variables required for Stack deployment
# (can be left blank for "standalone" deployment)
- NAMESPACE=ontospecies   # Target Blazegraph namespace
# Additional environment variables required for "standalone deployment"
# (can be left blank for Stack deployment)
- QUERY_ENDPOINT=
- UPDATE_ENDPOINT=
```

The STACK_NAME variable is used to identify the deployment mode of the agent. In case the STACK_NAME is left blank, default Blazegraph endpoint setting will be taken from the docker-compose file. Otherwise they will be retrieved using the StackClients based on the provided NAMESPACE variable.

Please note:

All variables defined here (except for STACK_NAME) serve as default values. To omit any of those default values, either remove the key completely or just leave it blank.

A missing STACK_NAME variable will result in an error; however, when deploying using the stack-manager start up script, the STACK_NAME variable will be set automatically for all services. Hence, this could be left blank here; however, if provided, it needs to match the STACK_NAME used by the stack-manager!

### Build and publish the Docker image

To build and publish the agent Docker image please use the following commands. Please note that all of those commands are bundled in the publish_docker_image.sh convenience script (only target image (i.e. production/debug) needs to be adjusted at top of that script).

```bash
# Building the Docker image (production / debug)
docker-compose -f docker-compose.yml  build
docker-compose -f docker-compose.debug.yml  build

# Publish the Docker image to the Github container registry
docker image push ghcr.io/cambridge-cares/<image tag>:<version>
```

Time out issues have been observed when building the image. If this happens, please try pulling the required stack-clients image first by

```bash
docker pull docker.cmclinnovations.com/stack-client:1.6.2.
```

### Docker deployment

Deploy the dockerised agent by running the following code in the command prompt from the same location where this README is located (ideally, use a bash terminal to avoid potential issues with inconsistent path separators).

```bash
# Deploy the Docker images (production / debug) locally
docker-compose -f docker-compose.yml  up
docker-compose -f docker-compose.debug.yml  up
```

To verify the correct startup of the agent, open the URL address the agent is running on, e.g. <http://localhost:5000> in your browser.

### Stack Deployment

If you want to spin up this agent as part of a stack, do the following:

- Build the production image using the commands provided above (do not spin up the image)
- Copy the pubchem-agent.json file from the stack-manager-input-config folder into the inputs/config folder of the stack manager
- Start the stack manager as usual (i.e. bash ./stack.sh start <STACK_NAME> from the stack-manager repo). This should start the container. Please use a bash terminal to avoid potential issues with inconsistent path separators.
- The agent shall become available at ```<http://<HOST>:<PORT>/>```

### Notes on debugging

The stack deployment of the agent is focused on the production image of the agent. To debug the agent, it is easiest to deploy the debug version locally by providing all required parameters in the docker-compose.debug.yml and running the below commands (build as required):

```bash
docker-compose -f docker-compose.debug.yml  build
docker-compose -f docker-compose.debug.yml  up
```

This spins up the agent on <http://localhost:5000>, waiting for the debugger to attach. To attach to the container and start debugging, please use the provided Python: Debug Flask within Docker debug configuration. Although outside the stack, this procedure allows to debug all essential functionality of the agent (without the need to have a full stack running).

## How to use

The agent can be used as a simple command line tool or as a web agent.

### Command line usage

 The agent can be run from the command line via the `pubchemagent` command which accepts the following options:

 ```bash
Usage:
    pubchemagent  (--inchi=<inchi>)

Options:
--inchi=<inchi>                     inchi string
```

Example usage:

```bash
(pubchemagent_venv) $ pubchemagent --inchi='InChI=1/Ar'
```

### Web agent usage

The web agent uses the [TheWorldAvatar](https://github.com/cambridge-cares/TheWorldAvatar) project knowledge graph to retrieve most of the species data needed
Only one input is required in the agent main route:

These are:

/api/pubchemagent/query?`inchi=InChI=inchistring`

```bash
inchi=<Inchi>   inchi 
```

In order to use the pubchemagent as a web agent, simply start a server with the following app entry point:

`(Windows)`

```cmd
(pubchemagent_venv) $ set FLASK_APP=pubchemagent\flaskapp\wsgi.py & flask run
```

`(Linux)`

```bash
(pubchemagent_venv) $ export FLASK_APP=pubchem\flaskapp\wsgi.py && flask run
```

## Authors

Laura Pascazio (<lp521@cam.ac.uk>)
Ali Naseri
