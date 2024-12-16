# The purpose of this module is to test the API Agent main functions

import copy
import pytest
import requests
from pathlib import Path
from rdflib import Graph
from rdflib import RDF
from py4jps import agentlogging

from data_classes.iris import TS_TIMESERIES
from data_classes.ts_data_classes import parse_time_to_unix, parse_incomplete_time
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
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")
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
    Test if API Agent creates derivation as expected

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
    api_agent, run_thread = create_example_agent(agent_iri, agent_url)


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
    time_in_rdb, value_in_rdb = api_agent.ts_client.get_timeseries(target_iri)
    print("Downloaded time: {}".format(time_in_rdb))
    print("Downloaded value: {}".format(value_in_rdb))
    # The external API data may change, here we assume that the external timeseries only adds new T-V pair, and T-V pairs as of the time this test creates (2024-Apr-02) should still exist in future version of data
    expect_time, expect_value = output
    for time in expect_time:
        assert time in time_in_rdb
    for value in expect_value:
        assert value in value_in_rdb    # Verify time values
    #print("All check passed.")
    cf.clear_database(rdb_url)
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")
    run_thread.shutdown()#stop the agent server



# Test if update of API is successful
@pytest.mark.parametrize("input_set, output",
                         [
                             (cf.TEST_INPUT1, cf.TEST_OUTPUT1),
                         ],)
def test_update(initialise_clients, create_example_agent, input_set, output):
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
    api_agent, run_thread = create_example_agent(agent_iri, agent_url)


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
    time_in_rdb, value_in_rdb = api_agent.ts_client.get_timeseries(target_iri)
    print("Downloaded time: {}".format(time_in_rdb))
    print("Downloaded value: {}".format(value_in_rdb))
    print(tsIRI)
    first_timestamp_ts = api_agent.api_sparql_client.get_timestep(tsIRI)
    print('first ts:')
    print(first_timestamp_ts)
    # We delete some timeseries (mock an API change, as only difference in source data vs API will trigger download)
    api_agent.ts_client.delete_timeseries_records(target_iri,'2010','3000')
    out = api_agent.ts_client.get_timeseries(target_iri)
    print('After deleting some TS records:')
    print(out) # verify RDB record is correctly modified
    update_func = api_agent.get_self_update_func(tsIRI)
    # We update meta data stamp to trigger update function
    api_agent.stamp_meta_current_time(input_set[0], True)
    update_func() # Explicitly call the update, note that this is periodically self-called in normal run
    target_iri = api_agent.api_sparql_client.get_target_iri_from_map_iri(input_set[0])
    time_update, value_update = api_agent.ts_client.get_timeseries(target_iri)
    print("Updated time: {}".format(time_update))
    print("Updated value: {}".format(value_update))
    assert time_update == time_in_rdb
    assert value_update == value_in_rdb
    snd_timestamp_ts = api_agent.api_sparql_client.get_timestep(tsIRI)
    print('second ts:')
    print(snd_timestamp_ts)
    assert int(snd_timestamp_ts) > int(first_timestamp_ts)

    print("All check passed.")
    run_thread.shutdown()#stop the agent server