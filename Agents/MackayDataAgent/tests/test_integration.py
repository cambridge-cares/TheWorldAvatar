#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
#          Magnus Mueller (mm2692@cam.ac.uk)    #
# Date: 25 Jul 2023                             #
#################################################

# The purpose of this module is to test the Agent Flask App. HTTP requests are sent
# to the Flask App and the response is checked.

import copy
import pytest
import requests
from pathlib import Path
from rdflib import Graph
from rdflib import RDF
from py4jps import agentlogging

from data_classes.iris import TS_TIMESERIES
from tests import conftest as cf
from kg_access.tsclient_wrapper import DOUBLE
from pyderivationagent.data_model.iris import ONTODERIVATION_DERIVATIONWITHTIMESERIES
from flask_testing import LiveServerTestCase

# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('prod')


#@pytest.mark.skip(reason="")
def test_example_triples():
    """
    This test checks that the example triples are correct in syntax.

    Raises:
        e: If the example triples are not valid RDF.
    """
    g = Graph()
    pathlist = Path(cf.TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        try:
            g.parse(str(path))
        except Exception as e:
            raise e


#@pytest.mark.skip(reason="")
def test_example_data_instantiation(initialise_clients):
    """
    This test checks that all example data gets correctly instantiated,
    including associated time series data in PostgreSQL.
    """
    # Get required clients from fixture
    sparql_client, _, rdb_url = initialise_clients
    print('client initiated')
    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    triples = cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples



#@pytest.mark.skip(reason="")
@pytest.mark.parametrize("input_set, output",
    [
        (cf.TEST_INPUT1, cf.TEST_OUTPUT1),
    ],)
def test_create_api(
    initialise_clients, create_example_agent, input_set, output
):
    """
    Test if API Agent performs derivation update as expected (using
    default Prophet model without covariates)

    """
    # Get forecast agent IRI for current test case
    agent_iri = cf.AGENT_IRI
    agent_url = cf.AGENT_URL

    # Get required clients from fixture
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Register API agent in KG
    api_agent = create_example_agent(ontoagent_service_iri=agent_iri,
                         ontoagent_http_url=agent_url)


    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples
    # Create derivation instance for API
    derivation = derivation_client.createSyncDerivationForNewInfo(agent_iri, input_set,
                                                                  ONTODERIVATION_DERIVATIONWITHTIMESERIES)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(input_set) # timestamps for pure inputs
    triples += cf.CREATED_TS_TRIPLES
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT # output timestamp
    triples +=  3    # derivation type + associated agent + belongsTo
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there is 1 derivation output (i.e. Forecast IRI)
    assert len(derivation_outputs) == 1
    assert TS_TIMESERIES in derivation_outputs
    assert len(derivation_outputs[TS_TIMESERIES]) == 1

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    #Retrieve downloaded timeseries and verify its details
    tsIRI = list(derivation_outputs[TS_TIMESERIES])[0]
    target_iri = api_agent.api_sparql_client.get_target_iri_from_map_iri(input_set[0])
    data_in_rdb = api_agent.ts_client.get_timeseries(target_iri)
    assert data_in_rdb == output
    # Verify time values

    print("All check passed.")