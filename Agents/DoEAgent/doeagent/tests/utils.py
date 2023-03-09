from rdflib import Graph
import pkgutil
import os

from pyderivationagent.data_model import TIME_HASTIME
from pyderivationagent.data_model import TIME_INTIMEPOSITION
from pyderivationagent.data_model import TIME_NUMERICPOSITION

import doeagent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples(sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Create folder for downloaded files
    if not os.path.exists(cf.DOWNLOADED_DIR):
        os.mkdir(cf.DOWNLOADED_DIR)

	# Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
    for f in [
        'sample_data/doe.ttl', # for normal DoE test
        'sample_data/rxn_data.ttl', # historical data for DoE test
        'sample_data/dummy_lab.ttl', # lab information
        'sample_data/doe_no_prior_data.ttl' # for DoE test without prior experiment data
    ]:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        sparql_client.uploadGraph(g)

    # additional historical data with infinite system response for DoE test
    g = Graph().parse(os.path.join(cf.TEST_TRIPLES_DIR, 'rxn_exp_6_inf_data.ttl'))
    sparql_client.uploadGraph(g)

    # Timestamp of pure inputs are not added here as they will be added at creation of the derivation instance


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
