#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
# Date: 22 Aug 2023                             #
#################################################

# The purpose of this module is to test the dockerised emission agent in 
# combination with Blazegraph and PostgreSQL spun up as test containers

import copy
import pytest
import numpy as np
import pandas as pd
from pathlib import Path
from rdflib import Graph
from rdflib import RDF
from operator import eq, gt

from py4jps import agentlogging

import emissionagent.datamodel as dm

from . import conftest as cf


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('dev')


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
    sparql_client, ts_client, _, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    triples = cf.ABOX_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_url) == 0

    # Initialise and upload time series
    ts_client.init_timeseries(dataIRI=cf.PROVIDED_HEAT_AMOUNT_1,
                              times=cf.TIMES, values=cf.VALUES_1,
                              ts_type=cf.DOUBLE, time_format=cf.TIME_FORMAT)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_url) == 2
    assert sparql_client.getAmountOfTriples() == (triples + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    times, values = ts_client.retrieve_timeseries(cf.PROVIDED_HEAT_AMOUNT_1)
    assert times == cf.TIMES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES_1

    # Verify that dropping all tables works as expected
    cf.clear_database(rdb_url)
    assert cf.get_number_of_rdb_tables(rdb_url) == 0


#@pytest.mark.skip(reason="")
@pytest.mark.parametrize(
    "derivation_input_set, amount_iri, ts_times, ts_values, expected_results",
    [
        (cf.DERIVATION_INPUTS_1, cf.DATA_IRI_1, cf.TIMES, cf.VALUES_1, cf.EXPECTED_OUTPUTS_1)
    ],
)
def test_estimate_emissions(
    initialise_clients, create_example_agent, derivation_input_set, amount_iri, 
    ts_times, ts_values, expected_results
):
    """
    Test if Emission Agent performs synchronous derivation update as expected
    """

    # Get required clients from fixture
    sparql_client, ts_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=amount_iri,
                              times=ts_times, values=ts_values,
                              ts_type=cf.DOUBLE, time_format=cf.TIME_FORMAT)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.ABOX_TRIPLES + cf.TS_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Register derivation agent in KG
    # - Successful agent registration within the KG is required to create/pick up derivations
    # - Hence, the dockerised agent is started without initial registration and registration
    #   is done within the test to guarantee that test Blazegraph will be ready
    # - The "belated" registration of the dockerised agents can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    agent = create_example_agent() 

    # Verify expected number of triples after derivation registration
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES + cf.DERIV_OUTPUT_TRIPLES
    assert sparql_client.getAmountOfTriples() == triples

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.OD_EMISSION)

    # Create derivation instance for new information (incl. timestamps for pure inputs)
    derivation = derivation_client.createSyncDerivationForNewInfo(agent.agentIRI, derivation_input_set,
                                                                  cf.ONTODERIVATION_DERIVATION)
    derivation_iri = derivation.getIri()
    print(f"Initialised successfully, created synchronous derivation instance: {derivation_iri}")
    
    # Verify expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += len(agent.POLLUTANTS) * cf.EMISSION_TRIPLES                # triples for new emission instances
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(agent.POLLUTANTS) * cf.RDF_TYPES_PER_EMISSION          # all belongTo triples
    triples += len(derivation_input_set) + 2    # number of inputs + derivation type + associated agent
    assert sparql_client.getAmountOfTriples() == triples

    # Query input & output of the derivation instance
    derivation_inputs, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify correct number of output types
    assert len(derivation_outputs) == cf.OUTPUT_TYPES
    assert dm.OD_EMISSION in derivation_outputs
    assert len(derivation_outputs[dm.OD_EMISSION]) == len(agent.POLLUTANTS)

    # Verify inputs (i.e., derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Retrieve instantiated emissions and verify their details
    emission_iris = list(derivation_outputs[dm.OD_EMISSION])
    emissions = sparql_client.get_emission_details(emission_iris)
    assert emissions == expected_results

#     # Update derivation interval and add latest timestamp to trigger update
#     cf.update_derivation_interval(derivation_iri, cf.FC_INTERVAL_2, sparql_client)
#     assert sparql_client.getAmountOfTriples() == triples
#     derivation_client.addTimeInstanceCurrentTimestamp(cf.FC_INTERVAL_2)
#     triples += cf.TIME_TRIPLES_PER_PURE_INPUT
#     assert sparql_client.getAmountOfTriples() == triples

#     # Request for derivation update and verify that no new triples have been added,
#     # only time series and interval values have been amended
#     derivation_client.unifiedUpdateDerivation(derivation_iri)
#     if not overwrite_forecast:
#         triples += cf.FORECAST_TRIPLES          # triples for new forecast
#         if with_unit:
#             triples += cf.UNIT_TRIPLES
#     assert sparql_client.getAmountOfTriples() == triples

#     # Retrieve updated forecast details
#     _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
#     fcIRI = list(derivation_outputs[dm.TS_FORECAST])[0]
#     fc_intervals = sparql_client.get_forecast_details(fcIRI)
#     inp_interval = sparql_client.get_interval_details(fc_intervals['input_interval_iri'])
#     outp_interval = sparql_client.get_interval_details(fc_intervals['output_interval_iri'])
#     assert inp_interval['start_unix'] == cf.T_2 - input_chunk_length*3600
#     assert inp_interval['end_unix'] == cf.T_2 - 3600
#     assert outp_interval['start_unix'] == cf.T_2
#     assert outp_interval['end_unix'] == cf.T_3
    
#     # Assess updated forecast error and create plot for visual inspection
#     errors = cf.assess_forecast_error(dataIRI, fcIRI, sparql_client, ts_client, 
#                                       agent_url=agent_url, name=case+'_updated')
#     print(f'Forecast errors for case: {case}_updated')
#     for k,v in errors.items():
#         print(f'{k}: {round(v,5)}')

    print("All check passed.")
