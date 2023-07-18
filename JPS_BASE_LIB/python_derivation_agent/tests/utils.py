from pathlib import Path
from rdflib import Graph
from rdflib import URIRef
from rdflib import RDF
import uuid

import pyderivationagent.data_model as dm
from .conftest import RESOURCE_DIR
from .conftest import AllInstances

from .agents.sparql_client_for_test import RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples_assert_pure_inputs(sparql_client, delete_all_triples=True) -> AllInstances:
    # Delete all triples before initialising prepared triples
    if delete_all_triples:
        sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")
    pathlist = Path(RESOURCE_DIR).glob('**/*.ttl')
    for path in pathlist:
        # uploadOntology is changed to uploadGraph, which seems more stable with docker as observed in tests
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)

    # Initialise the instances storage
    all_instances = AllInstances()
    all_instances.IRI_UPPER_LIMIT = sparql_client.getUpperLimit()
    all_instances.IRI_LOWER_LIMIT = sparql_client.getLowerLimit()
    all_instances.IRI_NUM_OF_PTS = sparql_client.getNumOfPoints()
    all_instances.VAL_UPPER_LIMIT = sparql_client.getValue(all_instances.IRI_UPPER_LIMIT)
    all_instances.VAL_LOWER_LIMIT = sparql_client.getValue(all_instances.IRI_LOWER_LIMIT)
    all_instances.VAL_NUM_OF_PTS = sparql_client.getValue(all_instances.IRI_NUM_OF_PTS)

    # timestamps of pure inputs will be added automatically when creating derivation markup
    # they are NOT added here so that we can test the framework is still working as expected

    assert not sparql_client.getPointsInKG()
    assert not sparql_client.getMaxValueIRI()
    assert not sparql_client.getMinValueIRI()
    assert not sparql_client.getDifferenceIRI()

    return all_instances


def initialise_triples_for_exception_throw_test(sparql_client) -> str:
    input_placeholder_exception_throw_iri = "http://" + str(uuid.uuid4())
    g = Graph()
    g.add((URIRef(input_placeholder_exception_throw_iri), RDF.type, URIRef(RANDOM_EXAMPLE_INPUTPLACEHOLDEREXCEPTIONTHROW)))
    sparql_client.uploadGraph(g)
    return input_placeholder_exception_throw_iri


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, dm.TIME_HASTIME, dm.TIME_INTIMEPOSITION, dm.TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
