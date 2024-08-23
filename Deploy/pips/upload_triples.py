from pathlib import Path
from rdflib import Graph
import os

from pyderivationagent import PySparqlClient
from pyderivationagent import config_generic
from pyderivationagent import Config

from chemistry_and_robots.data_model import iris


class ConfigKG(Config):
    SPARQL_QUERY_ENDPOINT: str
    SPARQL_UPDATE_ENDPOINT: str
    KG_USERNAME: str
    KG_PASSWORD: str
    FILE_SERVER_ENDPOINT: str
    FILE_SERVER_USERNAME: str
    FILE_SERVER_PASSWORD: str
    EMAIL_RECIPIENT: str
    EMAIL_USERNAME: str


# Folder paths
THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR, "env_files")
TRIPLES_DIR = os.path.join(THIS_DIR, "triples")

# Config for kg endpoint
AGENT_CREDENTIALS_ENV_DEPLOY = os.path.join(ENV_FILES_DIR, "agent.credentials.env.deploy")
config = config_generic(ConfigKG, AGENT_CREDENTIALS_ENV_DEPLOY)


if __name__ == '__main__':
    # Initialise the sparql_client
    sparql_client = PySparqlClient(
        query_endpoint=config.SPARQL_QUERY_ENDPOINT,
        update_endpoint=config.SPARQL_UPDATE_ENDPOINT,
        kg_user=config.KG_USERNAME,
        kg_password=config.KG_PASSWORD,
    )

    # Upload tbox
    sparql_client.upload_ontology_tbox(iris.ONTODOE)
    sparql_client.upload_ontology_tbox(iris.ONTOREACTION)

    # Upload the example triples for testing
    # NOTE remember to put the lab specific triples in the triples folder
    pathlist = Path(TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)
        print(f"Uploaded triples to KG: {str(path)}")
