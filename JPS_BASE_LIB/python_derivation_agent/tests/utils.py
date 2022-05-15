from pathlib import Path

from pyderivationagent.kg_operations import TIME_HASTIME
from pyderivationagent.kg_operations import TIME_INTIMEPOSITION
from pyderivationagent.kg_operations import TIME_NUMERICPOSITION
from tests.conftest import RESOURCE_DIR
from tests.conftest import AllInstances

# ----------------------------------------------------------------------------------
# Utility functions
# ----------------------------------------------------------------------------------

def initialise_triples_assert_pure_inputs(sparql_client, derivation_client) -> AllInstances:
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")
    pathlist = Path(RESOURCE_DIR).glob('**/*.ttl')
    for path in pathlist:
        sparql_client.uploadOntology(str(path))

    # Initialise the instances storage
    all_instances = AllInstances()
    all_instances.IRI_UPPER_LIMIT = sparql_client.getUpperLimit()
    all_instances.IRI_LOWER_LIMIT = sparql_client.getLowerLimit()
    all_instances.IRI_NUM_OF_PTS = sparql_client.getNumOfPoints()
    all_instances.VAL_UPPER_LIMIT = sparql_client.getValue(all_instances.IRI_UPPER_LIMIT)
    all_instances.VAL_LOWER_LIMIT = sparql_client.getValue(all_instances.IRI_LOWER_LIMIT)
    all_instances.VAL_NUM_OF_PTS = sparql_client.getValue(all_instances.IRI_NUM_OF_PTS)

    # Add timestamp to pure inputs
    derivation_client.addTimeInstance(all_instances.IRI_UPPER_LIMIT)
    derivation_client.updateTimestamp(all_instances.IRI_UPPER_LIMIT)
    derivation_client.addTimeInstance(all_instances.IRI_LOWER_LIMIT)
    derivation_client.updateTimestamp(all_instances.IRI_LOWER_LIMIT)
    derivation_client.addTimeInstance(all_instances.IRI_NUM_OF_PTS)
    derivation_client.updateTimestamp(all_instances.IRI_NUM_OF_PTS)

    assert not sparql_client.getListOfPoints()
    assert not sparql_client.getPointsInKG()
    assert not sparql_client.getMaxValueIRI()
    assert not sparql_client.getMinValueIRI()
    assert not sparql_client.getDifferenceIRI()

    return all_instances


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
