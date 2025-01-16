## Spin up docker containers

Spinning up docker containers requires knowledge about docker, please refer to [Setting up Docker](https://github.com/cambridge-cares/TheWorldAvatar/wiki/Setting-up-Docker) and [Docker SDK for Python](https://docker-py.readthedocs.io/en/7.0.0/index.html) if you do not have relevant experience.

> Please refer to [Blazegraph](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/db/blazegraph) and [Fileserver](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/db/fileserver) for technical details, brief example of how to use them are provided below.


### Blazegraph

Among many available triple stores, Blazegraph is commonly used within `TheWorldAvatar` and we have provided a convenient docker image [`ghcr.io/cambridge-cares/blazegraph:1.1.0`](https://github.com/orgs/cambridge-cares/packages/container/package/blazegraph) for users to deploy.

To spin up a Blazegraph docker container locally using Python:

```python
import docker
# Connect to Docker using the default socket or the configuration in your environment:
client = docker.from_env()

# Run Blazegraph container
# It returns a Container object that we will need later for stopping it
blazegraph = client.containers.run(
    'ghcr.io/cambridge-cares/blazegraph:1.1.0',
    ports={'8080/tcp': 9999}, # this binds the internal port 8080/tcp to the external port 9999
    detach=True # this runs the container in the background
)
```

The Blazegraph should now be accessible at the below endpoint for both query and update:

> SPARQL query and update endpoint might differ for other triple stores, e.g. [RDF4J](https://rdf4j.org/documentation/).

```python
# This is the default namespace `kb`
# Note the port we are accessing is 9999 as specified when spinning it up
sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'
```

To stop the blazegraph docker container after all operations:

```python
blazegraph.stop()
```


### Fileserver

Similarly, we have provided a convenient docker image of fileserver [`ghcr.io/cambridge-cares/fileserver:1.1.0`](https://github.com/orgs/cambridge-cares/packages/container/package/fileserver) for users to deploy.

To spin up a Fileserver docker container locally using Python:

```python
import docker
# Connect to Docker using the default socket or the configuration in your environment:
client = docker.from_env()

# Run Fileserver container
# It returns a Container object that we will need later for stopping it
fileserver = client.containers.run(
    'ghcr.io/cambridge-cares/fileserver:1.1.0',
    ports={'8080/tcp': 9998}, # this binds the internal port 8080/tcp to the external port 9998
    detach=True # this runs the container in the background
)
```

The Fileserver should now be accessible at the below endpoint for upload and download:

```python
# Note the port we are accessing is 9998 as specified when spinning it up
fs_url = 'http://localhost:9998/FileServer/'
# The fileserver image comes with a default username and password
fs_user = 'fs_user'
fs_password = 'fs_pass'
```

For how to interact with Fileserver, please see examples in the section **Interact with fileserver** below.

To stop the fileserver docker container after all operations:

```python
fileserver.stop()
```


### Use `docker-compose.yml`

If you wish to use `docker-compose.yml` instead, you may create the below yml file and compose it up:

> NOTE please remember to create the file `./secrets/blazegraph_password.txt` and `./secrets/fileserver_password.txt` with your desired password populated.

> NOTE the default username of blazegraph and fileserver are `bg_user` and `fs_user` respectively.

```yml
version: "3.8"

services:
  # Blazegraph
  blazegraph:
    image: ghcr.io/cambridge-cares/blazegraph:1.1.0
    container_name: "blazegraph"
    ports:
      - 9999:8080
    environment:
      BLAZEGRAPH_PASSWORD_FILE: /run/secrets/blazegraph_password
    # Add a secret to set the password for BASIC authentication
    secrets:
      - blazegraph_password

  # File server
  fileserver:
    image: ghcr.io/cambridge-cares/fileserver:1.1.0
    container_name: "fileserver"
    ports:
      - 9998:8080
    # Add secret to set BASIC authentication password
    secrets:
      - file_server_password

# Secrets used to set runtime passwords
secrets:
  blazegraph_password:
    file: ./secrets/blazegraph_password.txt
  file_server_password:
    file: ./secrets/fileserver_password.txt
```

> For more details on how to use `docker compose`, plese refer to [Docker Compose overview](https://docs.docker.com/compose/).


## Instantiation of the `PySparqlClient`

To initialise a SPARQL client with the above `sparql_endpoint` and `fs_url` with the basic authentication:

> NOTE remember to populate the passwords `<your_blazegraph_password>` and `<your_fileserver_password>` based on those provided in your secret files.

```python
from twa.kg_operations import PySparqlClient

# sparql_endpoint = 'http://localhost:9999/blazegraph/namespace/kb/sparql'
# fs_url = 'http://localhost:9998/FileServer/'
sparql_client = PySparqlClient(
    query_endpoint = sparql_endpoint,
    update_endpoint = sparql_endpoint,
    kg_user = 'bg_user',
    kg_password = '<your_blazegraph_password>',
    fs_url = fs_url,
    fs_user = 'fs_user',
    fs_pwd = '<your_fileserver_password>'
)
```

We can swap `sparql_endpoint` with URL of any other SPARQL endpoint what one wish to query/update.


## SPARQL query and update

There are a few convenient functions provided in `PySparqlClient` that we can use, e.g. to get the total amount of triples in the SPARQL endpoint:

```python
num_of_triples = sparql_client.get_amount_of_triples()
```

To perform custom update and query:
```python
# Update the triple store by inserting data
sparql_client.perform_update(
    'INSERT DATA {<https://s> <https://p> <https://o>.}'
)

# Query the first 10 triples in the triple store
triples = sparql_client.perform_query(
    'SELECT * WHERE {?s ?p ?o} LIMIT 10'
)
```

> For tutorials of SPARQL operations, please refer to the [official W3C documentation](https://www.w3.org/TR/sparql11-overview/). Some useful tips and tricks can be found at [SPARQL tips and tricks](https://github.com/cambridge-cares/TheWorldAvatar/wiki/SPARQL-tips-and-tricks).


## Use `rdflib.Graph` object
Python package `rdflib` is a convenient tool to work with RDF data. It has `rdflib.Graph` object which can be used as a knowledge graph in memory to temporarily host data before uploading it to a triple store. It is more `pythonic` compared to writing out the triples manually when constructing the SPARQL insert clause.

To add triples to `rdflib.Graph` object and upload it to a triple store:
```python
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import FOAF, RDF

# Instantiate a rdflib.Graph object
g = Graph()

# Create dummy data
bob = URIRef("http://example.org/people/Bob")
name = Literal("Bob")
age = Literal(24)

# Add data to rdflib.Graph object
g.add((bob, RDF.type, FOAF.Person))
g.add((bob, FOAF.name, name))
g.add((bob, FOAF.age, age))

# Upload the triples to triple store
sparql_client.upload_graph(g)
```

> For more tutorials on how to use `rdflib`, please refer to [rdflib 7.0.0](https://rdflib.readthedocs.io/en/stable/).


## Upload TBox from a given URL to a triple store

In some use cases, in addition to data in ABox, we may want to upload the TBox to the same triple store to perform reasoning. We can again make use of `rdflib.Graph`:

```python
# Here we take OntoDoE as an example
tbox_url = 'https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontodoe/OntoDoE.owl'

# Parse the TBox to rdflib.Graph object
tbox_g = Graph()
tbox_g.parse(tbox_url, format='xml')

# Upload it to triple store
sparql_client.upload_graph(tbox_g)
```


## Interact with fileserver

For use cases involving processing raw files, e.g. reports from HPLC jobs, one can use the below to upload/download files to/from the fileserver:

> Authentication is required to interact with fileserver docker container, the default username is `fs_user` and the default password is `fs_pass`.

```python
# Upload local file to remote fileserver
local_file_path = '<local_file_path>'
remote_file_path, timestamp_upload = sparql_client.upload_file(local_file_path)

# Download remote file to local machine with user-specified file path
downloaded_file_path = '<downloaded_file_path>'
sparql_client.download_file(remote_file_path, downloaded_file_path)
```

> NOTE this function does NOT instantiate relevant triples of the uploaded files in the triple store. A [feature request](https://github.com/cambridge-cares/TheWorldAvatar/issues/718) exist for this which will be addressed in the next release. Please see [here](https://github.com/cambridge-cares/TheWorldAvatar/blob/d24c68349a3b27d7b77528abc651db899ff49f7b/Agents/utils/chemistry_and_robots/chemistry_and_robots/kg_operations/sparql_client.py#L1650) for a preliminary example attempted in another project.

> For all available operations over Fileserver, please visit its [technical details](https://github.com/cambridge-cares/TheWorldAvatar/tree/main/Deploy/stacks/db/fileserver).


## Create custom sparql client

Depends on use cases, users may want to define custom SPARQL queries/updates that can be re-used across different Python modules. One way to do this could be inheriting the `PySparqlClient` class:

```python
# Define custom sparql client class
class OntoKinSparqlClient(PySparqlClient):
    def get_reaction_mechanisms(self, limit: int = 10):
        query = f"""
            PREFIX ontokin: <http://www.theworldavatar.com/ontology/ontokin/OntoKin.owl#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>	SELECT ?mechanismIRI
            WHERE	{{ ?mechanismIRI rdf:type ontokin:ReactionMechanism . }} LIMIT {limit}
        """
        response = self.perform_query(query)
        return [list(res.values())[0] for res in response]


# Initialise custom sparql client in the same way as PySparqlClient
ontokin_sparql_client = OntoKinSparqlClient(sparql_endpoint, sparql_endpoint)

# Perform custom queries
ontokin_sparql_client.get_reaction_mechanisms()
```
