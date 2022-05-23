from rdflib import Graph
import pkgutil
import os

from pyderivationagent.kg_operations import TIME_HASTIME
from pyderivationagent.kg_operations import TIME_INTIMEPOSITION
from pyderivationagent.kg_operations import TIME_NUMERICPOSITION

import doeagent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples(generate_random_download_path, sparql_client, derivation_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Create folder for downloaded files
    if not os.path.exists(cf.DOWNLOADED_DIR):
        os.mkdir(cf.DOWNLOADED_DIR)

	# Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
    for f in ['ontoagent/Service__DoE.ttl', 'sample_data/doe.ttl', 'sample_data/rxn_data.ttl', 'sample_data/dummy_lab.ttl']:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = generate_random_download_path("ttl")
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)
        # the serialised files will be deleted at the end of testing session

    # Add timestamp to pure inputs
    for input in cf.DERIVATION_INPUTS:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
