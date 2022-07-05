from pathlib import Path
from rdflib import Graph
import pkgutil
import os

from pyderivationagent.kg_operations import TIME_HASTIME
from pyderivationagent.kg_operations import TIME_INTIMEPOSITION
from pyderivationagent.kg_operations import TIME_NUMERICPOSITION

import tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples(sparql_client, derivation_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Create folder for downloaded files
    if not os.path.exists(cf.DOWNLOADED_DIR):
        os.mkdir(cf.DOWNLOADED_DIR)

    # Upload the example triples for testing
    pathlist = Path(cf.TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        sparql_client.uploadOntology(str(path))

    # Add timestamp to pure inputs
    for input in cf.DERIVATION_INPUTS:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
