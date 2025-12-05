# PubChem agent

## Description

The `PubChem Agent` enables The World Avatar (TWA) knowledge graph to augment itself, upon request, with chemical information queried from PubChem using the PubChem PUG REST API and represented semantically using the OntoSpecies ontology.

## Installation

These instructions will help you get a copy of the project running on your local machine for development and testing.

### Requirements

- You need Python ≥ 3.9 to run the `PubChem Agent`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
- You also need to install a [Java Runtime Environment version ≥ 8](https://adoptopenjdk.net/?variant=openjdk8&jvmVariant=hotspot)

### Clone the Repository

To get started, first clone the `PubChem Agent` repository and navigate into the
project directory:

```bash
$ git clone https://github.com/cambridge-cares/TheWorldAvatar.git
$ cd TheWorldAvatar/Agents/PubChemAgent
```

### Set Environment variables

The Dockerised PubChem Agent can be deployed either as a standalone container, or within an existing Docker stack. Several key environment variables need to be set in your ```docker-compose.yml``` file:

```docker
# Required environment variables for both stack and standalone (i.e. outside stack) deployment

# Stack name. Leave blank for standalone deployment.
# Example: STACK_NAME=chemistry_stack
- STACK_NAME=

# Target Blazegraph namespace.
# Example: NAMESPACE=ontospecies4
- NAMESPACE=

# Blazegraph SPARQL query endpoint.
# Example: QUERY_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
- QUERY_ENDPOINT=

# Blazegraph SPARQL update endpoint.
# Example: UPDATE_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
- UPDATE_ENDPOINT=

# Optional environment variables (if authentication is needed)

# Blazegraph user
- BG_USER=

# Blazegraph password
- BG_PASSWORD=
```

### Installation from the version-controlled source

To install the `PubChem Agent` on Linux or WSL, choose one of the following four installation methods.

#### Option 1 — Create a virtual environment manually, then install (standard installation)

```bash
# create a virtual environment
$ python3.9 -m venv pubchemagent_venv
# activate the virtual environment
$ source pubchemagent_venv/bin/activate
# perform a standard installation (package files are copied into the environment)
(pubchemagent_venv) $ python -m pip install .
```

#### Option 2 — Create a virtual environment manually, then install in editable mode

```bash
# create a virtual environment
$ python3.9 -m venv pubchemagent_venv
# activate the virtual environment
$ source pubchemagent_venv/bin/activate
# install in editable mode, which links the project directory into the environment for active development
(pubchemagent_venv) $ python -m pip install -e .
```

#### Option 3 — Use the convenience script to create the environment and install

```bash
# create a virtual environment and install the package (non-editable)
$ ./install_script_pip.sh -v -i
# activate the virtual environment
$ source pubchemagent_venv/bin/activate
```

#### Option 4 — Use the convenience script to create the environment and install in editable mode

```bash
# create a virtual environment and install the package in editable development mode
$ ./install_script_pip.sh -v -i -e
# activate the virtual environment
$ source pubchemagent_venv/bin/activate
```

### Running the Tests

To run the test suite, install pytest inside the virtual environment:

```bash
(pubchemagent_venv) $ pip install pytest
```

Run the tests using:

```bash
(pubchemagent_venv) $ python -m pytest tests/test_pubchemagent.py
```

## Build and deploy the agent with Docker

### Build and publish the Docker image

To build and publish the agent's Docker image, use the following commands. All of these commands are also wrapped inside the ```publish_docker_image.sh``` convenience script (you only need to adjust the target image tag inside that script).

```bash
# Build the Docker image (production)
docker-compose -f docker-compose.yml  build

# Build the Docker image (debug)
docker-compose -f docker-compose.debug.yml  build

# Authenticate with the GitHub Container Registry (if not already logged in)
docker login ghcr.io
# If prompted, enter your GitHub username and a personal access token with permission and scope to read and write the container repository.

# Publish the Docker image to the Github Container Registry
docker image push ghcr.io/theworldavatar/pubchem_agent:1.1.0
```

### Docker deployment

To deploy the Dockerised agent, run the following commands from the directory where this README is located (use a bash terminal to avoid potential issues with inconsistent path separators).

```bash
# Run the production container locally
docker-compose -f docker-compose.yml  up

# Run the debug container locally
docker-compose -f docker-compose.debug.yml  up
```

To verify that the agent has started correctly, open <http://localhost:5000> in your browser.

To retrieve the IRI of a species (e.g., argon), open <http://localhost:5000/query/species?inchi=InChI=1/Ar>. If argon already exists in the namespace, the agent will return its IRI.
If not, the agent queries PubChem, instantiates the species in the target namespace, and returns the newly created IRI.

If you are able to access this endpoint successfully, it means the agent has been deployed correctly. You can now begin retrieving and instantiating species using their InChIs.

### Stack Deployment

If you want to deploy this agent as part of a TWA stack, follow these steps:

- Set the ```STACK_NAME``` in your ```docker-compose.yml``` file, for example:
  ```bash
  STACK_NAME=chemistry_stack
  ```
- Assuming that you already built and published the docker image. If not, build and publish the production image using the commands provided below:
  ```bash
  docker-compose -f docker-compose.yml build
  docker image push ghcr.io/theworldavatar/pubchem_agent:1.1.0
  ```
- Clone the stack repository and enter the ```stack-manager``` directory using these commands:
```bash
$ git clone https://github.com/TheWorldAvatar/stack.git
$ cd stack/stack-manager
```
- Create two files called ```postgis_password``` and ```geoserver_password``` in the ```stack-manager/inputs/secrets/``` directory. Populate the files with the intended passwords for PostGIS and GeoServer, respectively.
- Copy the ```pubchem-agent.json``` and ```blazegraph.json``` file from the ```PubChemAgent/pubchemagent/stack-manager-input-config``` folder into the ```inputs/config/services``` folder of the stack manager. Set the enviromental variables NAMESPACE, QUERY_ENDPOINT and UPDATE_ENDPOINT in ```pubchem-agent.json```. The environmental variable ```STACK_NAME``` is already set to ```chemistry_stack``` in the provided configuration. Do not change it.
- Copy ```chemistry_stack.json``` from the ```PubChemAgent/pubchemagent/stack-manager-input-config``` folder into the ```inputs/config``` folder of the stack manager.
- To start the stack manager using the default port 3838, run the following from ```stack/stack-manager``` directory.

```bash
bash ./stack.sh start chemistry_stack
```

If you want to run it on a different port, run the following command instead by setting the port number:

```bash
bash ./stack.sh start chemistry_stack <PORT_NUMBER>
```

Use a Bash terminal to avoid potential issues with inconsistent path separators. Once started, the agent will be available at <http://localhost:PORT/pubchemagent/> (e.g. <http://localhost:3838/pubchemagent/> when using the default port).

If the default port is used, verify that the agent has started correctly by opening <http://localhost:3838/pubchemagent/> in your browser.

To retrieve the IRI of a species (e.g., argon), open <http://localhost:3838/pubchemagent/query/species?inchi=InChI=1/Ar>. If argon already exists in the namespace, the agent will return its IRI.
If not, the agent will query PubChem, instantiate the species, and return the new IRI.

If you can access this endpoint successfully, it means you have successfully spun up the agent within the stack. You can now start retrieving and instantiating species using their InChI identifiers, and you may skip the rest of the sections.

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
--inchi=<inchi>        inchi string
```

Example usage:

```bash
(pubchemagent_venv) $ pubchemagent --inchi='InChI=1/Ar'
```

### Web agent usage

The agent can be run as a web agent sending the following http request with the following option:

(If the agent has been installed from the version-controlled source or deployed from docker image)
<http://localhost:5000/pubchemagent/query/species?inchi='inchi_string>'
example usage: <http://localhost:5000/pubchemagent/query/species?inchi='InChI=1/Ar>'

(Stack deployment)
<http://localhost:3838/pubchemagent/query/species?inchi='inchi_string>'
example usage: <http://localhost:3838/pubchemagent/query/species?inchi='InChI=1/Ar>'

In order to use the pubchemagent as a web agent when installed from the version-controlled source, simply start a server with the following app entry point:

`(Windows)`

```cmd
(pubchemagent_venv) $ set FLASK_APP=pubchemagent\flaskapp\wsgi.py & flask run
```

`(Linux)`

```bash
(pubchemagent_venv) $ export FLASK_APP=pubchemagent\flaskapp\wsgi.py && flask run
```

## Authors

Laura Pascazio (<lp521@cam.ac.uk>)
Ali Naseri
Feroz Farazi (<msff2@cam.ac.uk>)
