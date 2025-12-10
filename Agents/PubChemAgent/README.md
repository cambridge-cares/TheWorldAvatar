# PubChem agent

## Description

The `PubChem Agent` enables The World Avatar (TWA) knowledge graph to augment itself, upon request, with chemical information queried from PubChem using the PubChem PUG REST API and represented semantically using the OntoSpecies ontology.

## Installation

These instructions will help you get a copy of the project running on your local machine for development and testing.

### Requirements

- You need Python ≥ 3.11 to run the `PubChem Agent`. You can install Python by going to the official Python [download page](https://www.python.org/getit/)
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
$ python3 -m venv pubchemagent_venv
# activate the virtual environment
$ source pubchemagent_venv/bin/activate
# perform a standard installation (package files are copied into the environment)
(pubchemagent_venv) $ python -m pip install .
```

#### Option 2 — Create a virtual environment manually, then install in editable mode

```bash
# create a virtual environment
$ python3 -m venv pubchemagent_venv
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
docker compose build

# Build the Docker image (debug)
docker compose -f docker-compose.debug.yml build

# Authenticate with the GitHub Container Registry (if not already logged in)
docker login ghcr.io
# If prompted, enter your GitHub username and a personal access token with permission and scope to read and write the container repository.

# Publish the Docker image to the Github Container Registry
docker image push ghcr.io/theworldavatar/pubchem_agent:1.1.0
```

### Docker deployment

To deploy the Dockerised agent, run the following commands from the directory where this README is located (use a bash terminal to avoid potential issues with inconsistent path separators).

```bash
# Run the production container outside of a stack
docker compose up

# Run the debug container outside of a stack
docker compose -f docker-compose.debug.yml up
```

To verify that the agent has started correctly, open <http://localhost:5000> in your browser.

To retrieve the IRI of a species (e.g., argon), open <http://localhost:5000/query/species?inchi=InChI=1/Ar>. If argon already exists in the namespace, the agent will return its IRI.
If not, the agent queries PubChem, instantiates the species in the target namespace, and returns the newly created IRI.

If you are able to access this endpoint successfully, it means the agent has been deployed correctly. You can now begin retrieving and instantiating species using their InChIs.

**Important notes:**

The PubChem Agent works fully with any external Blazegraph endpoint, as long as the required environment variables `NAMESPACE`, `QUERY_ENDPOINT` and `UPDATE_ENDPOINT` are correctly set.

If your deployment already relies on a remote Blazegraph instance (for example, a server-hosted namespace), you can safely remove the Blazegraph service definition from both the `docker-compose.yml` and `docker-compose.debug.yml` files:

This means deleting the following blocks:

```yaml
  blazegraph_os:
    container_name: "blazegraph_os"
    image: ghcr.io/cambridge-cares/blazegraph:1.1.0
    restart: unless-stopped
    ports:
      - "8080:8080"
    volumes:
      - blazegraph_os_data:/blazegraph
    networks:
      - default

volumes:
  blazegraph_os_data:
    name: "blazegraph_os_data"
```

### Stack Deployment

If you want to deploy this agent as part of a TWA stack, follow these steps:

- Assuming that you already built and published the docker image. If not, build and publish the production image using the commands provided below:
  ```bash
  docker compose build
  docker image push ghcr.io/theworldavatar/pubchem_agent:1.1.0
  ```
- Clone the stack repository and enter the ```stack-manager``` directory using these commands:
```bash
$ git clone https://github.com/TheWorldAvatar/stack.git
$ cd stack/stack-manager
```
- Create two files called ```postgis_password``` and ```geoserver_password``` in the ```stack-manager/inputs/secrets/``` directory. Populate the files with the intended passwords for PostGIS and GeoServer, respectively.
- Copy the ```pubchem-agent.json``` and ```blazegraph.json``` files from the ```PubChemAgent/pubchemagent/stack-manager-input-config``` folder into the ```inputs/config/services``` folder of the stack manager. Set the enviromental variables NAMESPACE, QUERY_ENDPOINT and UPDATE_ENDPOINT in ```pubchem-agent.json```.
- Copy ```chemistry_stack.json``` from the ```PubChemAgent/pubchemagent/stack-manager-input-config``` folder into the ```inputs/config``` folder of the stack manager.
- To start the stack manager using the default port 3838, run the following from ```stack/stack-manager``` directory.

```bash
bash ./stack.sh start <STACK_NAME>
```

If you want to run it on a different port, run the following command instead by setting the port number:

```bash
bash ./stack.sh start <STACK_NAME> <PORT_NUMBER>
```

Use a Bash terminal to avoid potential issues with inconsistent path separators. Once started, the agent will be available at <http://localhost:PORT/pubchemagent/> (e.g. <http://localhost:3838/pubchemagent/> when using the default port).

If the default port is used, verify that the agent has started correctly by opening <http://localhost:3838/pubchemagent/> in your browser.

To retrieve the IRI of a species (e.g., argon), open <http://localhost:3838/pubchemagent/query/species?inchi=InChI=1/Ar>. If argon already exists in the namespace, the agent will return its IRI.
If not, the agent will query PubChem, instantiate the species, and return the new IRI.

If you can access this endpoint successfully, it means you have successfully spun up the agent within the stack. You can now start retrieving and instantiating species using their InChI identifiers, and you may skip the rest of the sections.

#### Why do you need `blazegraph.json`?

The file `PubChemAgent/pubchemagent/stack-manager-input-config/blazegraph.json` is included in the stack manager only to spin up an additional Blazegraph instance for development.

When deploying the `PubChemAgent` inside a stack, some developers find it useful to launch a dedicated Blazegraph service for local testing or experimentation.  
The provided `blazegraph.json` enables the stack manager to start this optional Blazegraph instance.

**Important notes:**

- This file is *not required* to run the agent.
- The PubChem Agent works fully with any external Blazegraph endpoint, as long as the appropriate `NAMESPACE`, `QUERY_ENDPOINT` and `UPDATE_ENDPOINT` environment variables are provided.

If your deployment uses an existing remote Blazegraph (e.g., a server-hosted namespace), you may safely omit this file **and** remove the corresponding `Configs` block from the `pubchem-agent.json` file:

```json
"Configs": [
    {
        "ConfigName": "blazegraph"
    }
]
```

### Notes on debugging

The stack deployment of the agent is focused on the production image of the agent. To debug the agent, it is easiest to deploy the debug version locally by providing all required parameters in the docker-compose.debug.yml and running the below commands (build as required):

```bash
docker-compose -f docker-compose.debug.yml  build
docker-compose -f docker-compose.debug.yml  up
```

This spins up the agent on <http://localhost:5000>, waiting for the debugger to attach. To attach to the container and start debugging, please use the provided Python: Debug Flask within Docker debug configuration. Although outside the stack, this procedure allows to debug all essential functionality of the agent (without the need to have a full stack running).

## Running the agent without Docker

If you installed the agent from the version-controlled source, you can run it using Flask.
Before starting the server, you must define the following required SPARQL endpoint environment variables.

1. Set mandatory environment variables

The agent requires the following variables:

NAMESPACE – the Blazegraph namespace (e.g., ontospecies4)

QUERY_ENDPOINT – SPARQL query endpoint URL

UPDATE_ENDPOINT – SPARQL update endpoint URL

`(Windows)`

```cmd
(pubchemagent_venv) $ set NAMESPACE=ontospecies4
(pubchemagent_venv) $ set QUERY_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
(pubchemagent_venv) $ set UPDATE_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
```

`(Linux)`

```bash
(pubchemagent_venv) $ export NAMESPACE=ontospecies4
(pubchemagent_venv) $ export QUERY_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
(pubchemagent_venv) $ export UPDATE_ENDPOINT=http://<your-blazegraph-host>/blazegraph/namespace/ontospecies4/sparql
```

2. Start the Flask server

`(Windows)`

```cmd
(pubchemagent_venv) $ set FLASK_APP=pubchemagent\flaskapp\wsgi.py & flask run
```

`(Linux)`

```bash
(pubchemagent_venv) $ export FLASK_APP=pubchemagent.flaskapp.wsgi.py && flask run
```

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

### Using the agent with HTTP requests

You can interact with the agent through its HTTP API using tools such as `curl`.

#### Flask

If the Flask server is running at http://127.0.0.1:5000, send a GET request to:

```bash
(pubchemagent_venv) $ curl -G "http://127.0.0.1:5000/query/species" \
     --data-urlencode "inchi=InChI=1/Ar"
```

#### Docker (outside the stack)

If the agent is running in Docker at http://localhost:5000, send a GET request to:

```bash
(pubchemagent_venv) $ curl -G "http://localhost:5000/query/species" \
     --data-urlencode "inchi=InChI=1/Ar"
```

#### Docker (in the stack)

If the stack is running on http://localhost:3838, send a GET request to:

```bash
(pubchemagent_venv) $ curl -G "http://localhost:3838/pubchemagent/query/species" \
     --data-urlencode "inchi=InChI=1/Ar"
```

## To-Do

The following improvements are planned for future releases:

### 1. Replace placeholder `<None>` handling in triple generation

In some cases, PubChem does not provide an element IRI for an atom.  
When this happens, the current implementation produces a placeholder IRI such as `<None>`, which is invalid.  
To avoid generating invalid triples, the code replaces `<None>` with `<file:///app/None>` as a workaround.

This behaviour is not correct and should be replaced with a proper handling strategy.

A complete fix should:

- Validate all atom (element) IRIs before triple generation  
- Raise a clear, descriptive error when required chemical information is missing  
- Prevent generation of invalid or placeholder IRIs  
- Remove the need for post-generation string replacement

**Relevant code:**  
`pubchemagent/kgoperations/querytemplates.py` → inside `pubchem_atom_insert()`

## Authors

- Laura Pascazio (<lp521@cam.ac.uk>)
- Ali Naseri
- Feroz Farazi (<msff2@cam.ac.uk>)
