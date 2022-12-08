from pathlib import Path
from rdflib import Graph
import os

from pyderivationagent import PySparqlClient
from pyderivationagent import config_derivation_agent


# Folder paths
THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR, "env_files")
SECRETS_DIR = os.path.join(THIS_DIR, "secrets")
TRIPLES_DIR = os.path.join(THIS_DIR, "triples")

# Credentials
BLAZEGRAPH_USERNAME_FILE = os.path.join(SECRETS_DIR, "blazegraph_username.txt")
BLAZEGRAPH_PASSWORD_FILE = os.path.join(SECRETS_DIR, "blazegraph_password.txt")

# Config for kg endpoint
ROGI_ENV = os.path.join(ENV_FILES_DIR, "agent.goal.iter.env.deploy")
config = config_derivation_agent(ROGI_ENV)


if __name__ == '__main__':
    # Initialise the sparql_client
    with open(BLAZEGRAPH_USERNAME_FILE, 'r') as file:
        kg_username = file.read().rstrip()
    with open(BLAZEGRAPH_PASSWORD_FILE, 'r') as file:
        kg_password = file.read().rstrip()
    sparql_client = PySparqlClient(
        query_endpoint=config.SPARQL_QUERY_ENDPOINT,
        update_endpoint=config.SPARQL_UPDATE_ENDPOINT,
        kg_user=kg_username,
        kg_password=kg_password,
    )

    # Upload the example triples for testing
    # NOTE remember to put the lab specific triples in the triples folder
    pathlist = Path(TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)
        print(f"Uploaded triples to KG: {str(path)}")
