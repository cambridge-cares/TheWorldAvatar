from rdflib import Graph
import pkgutil
import os

from pyderivationagent.kg_operations import TIME_HASTIME
from pyderivationagent.kg_operations import TIME_INTIMEPOSITION
from pyderivationagent.kg_operations import TIME_NUMERICPOSITION

import vtexeagent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples(generate_random_download_path, sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Create folder for downloaded files
    if not os.path.exists(cf.DOWNLOADED_DIR):
        os.mkdir(cf.DOWNLOADED_DIR)

	# Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
    for f in [
		'sample_data/dummy_lab.ttl', 'sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl', 'sample_data/rxn_data.ttl',
	]:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = generate_random_download_path("ttl")
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)
        # the serialised files will be deleted at the end of testing session


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])

def get_agilent_derivation(rxn_exp_iri: str, sparql_client):
    query = """SELECT ?agilent_derivation WHERE {?agilent_derivation <%s> <%s>; <%s> ?agilent_agent. ?agilent_agent ^<%s>/a <%s>.}""" % (
        cf.ONTODERIVATION_ISDERIVEDFROM, rxn_exp_iri, cf.ONTODERIVATION_ISDERIVEDUSING, cf.ONTOLAB_ISMANAGEDBY, cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY
    )
    response = sparql_client.performQuery(query)
    return response[0]['agilent_derivation'] if len(response) > 0 else None

def get_derivation_outputs(derivation_iri: str, sparql_client):
    query = """SELECT ?derivation_outputs WHERE {?derivation_outputs <%s> <%s>.}""" % (cf.ONTODERIVATION_BELONGSTO, derivation_iri)
    response = sparql_client.performQuery(query)
    return [r['derivation_outputs'] for r in response]
