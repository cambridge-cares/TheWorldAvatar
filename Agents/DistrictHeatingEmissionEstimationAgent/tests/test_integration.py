#################################################
# Authors: Markus Hofmeister (mh807@cam.ac.uk)  #
# Date: 22 Aug 2023                             #
#################################################

# The purpose of this module is to test the dockerised emission agent in 
# combination with Blazegraph and PostgreSQL spun up as test containers

import copy
import pytest
from pathlib import Path
from rdflib import Graph
from rdflib import RDF

from py4jps import agentlogging

import emissionagent.datamodel as dm
from emissionagent.agent.emission_estimation import extract_relevant_gas_or_heat_amount

from . import conftest as cf


# Initialise logger instance (ensure consistent logger level`)
logger = agentlogging.get_logger('dev')


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


@pytest.mark.parametrize(
    "amount_iri, ts_times, ts_values, expected_to_fail, simulation_time, expected_amount",
    [
        (cf.DATA_IRI_1, cf.TIMES, cf.VALUES_1, False, cf.SIMULATION_TIME_1, cf.VALUE_1),
        (cf.DATA_IRI_1, cf.TIMES, cf.VALUES_1, False, cf.SIMULATION_TIME_2, cf.VALUE_2),
        (cf.DATA_IRI_1, cf.TIMES, cf.VALUES_1, True, cf.SIMULATION_TIME_3, cf.VALUE_3),
    ],
)
def test_extract_time_series_amounts(
    initialise_clients, amount_iri, ts_times, ts_values, expected_to_fail,
    simulation_time, expected_amount
):
    """
    This function tests whether the correct time series values are extracted
    from ConsumedGas and ProvidedHeat time series for given SimulationTime
    """

    # Get required clients from fixture
    sparql_client, ts_client, _, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # (it first DELETES ALL DATA in the specified SPARQL/RDB endpoints)
    cf.initialise_triples(sparql_client)
    cf.clear_database(rdb_url)
    ts_client.init_timeseries(dataIRI=amount_iri,
                              times=ts_times, values=ts_values,
                              ts_type=cf.DOUBLE, time_format=cf.TIME_FORMAT)
    
    if not expected_to_fail:
        amount = extract_relevant_gas_or_heat_amount(ts_client=ts_client, 
                        kg_client=sparql_client, derivationIRI='TestDerivation',
                        dataIRIs=[amount_iri], simulation_time_iri=simulation_time)
        assert amount == expected_amount
    
    else:
        with pytest.raises(ValueError) as exc_info:
            extract_relevant_gas_or_heat_amount(ts_client=ts_client, 
                        kg_client=sparql_client, derivationIRI='TestDerivation',
                        dataIRIs=[amount_iri], simulation_time_iri=simulation_time)
        # Check if expected error message is raised
        assert expected_amount in str(exc_info.value)


@pytest.mark.parametrize(
    "derivation_input_set, amount_iris, ts_times, ts_values, expected_results",
    [
        (cf.DERIVATION_INPUTS_1, [cf.DATA_IRI_2], cf.TIMES, cf.VALUES_1, [cf.EXPECTED_OUTPUTS_1, cf.EXPECTED_OUTPUTS_2]),
        (cf.DERIVATION_INPUTS_2, [cf.DATA_IRI_3, cf.DATA_IRI_4], cf.TIMES, cf.VALUES_1, [cf.EXPECTED_OUTPUTS_3, cf.EXPECTED_OUTPUTS_4])
    ],
)
def test_derive_emissions(
    initialise_clients, create_example_agent, derivation_input_set, amount_iris, 
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
    for iri in amount_iris:
        ts_client.init_timeseries(dataIRI=iri, times=ts_times, values=ts_values,
                                  ts_type=cf.DOUBLE, time_format=cf.TIME_FORMAT)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = cf.ABOX_TRIPLES + len(amount_iris) * cf.TS_TRIPLES
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

    # Retrieve instantiated emissions and verify their details
    emission_iris = list(derivation_outputs[dm.OD_EMISSION])
    emissions = sparql_client.get_emission_details(emission_iris)
    assert emissions == expected_results[0]

    # Verify inputs (i.e., derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in derivation_inputs:
        for j in derivation_inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    # Update derivation's simulation time and add latest timestamp to trigger update
    cf.update_simulation_time(derivation_iri, cf.SIMULATION_TIME_2, sparql_client)
    assert sparql_client.getAmountOfTriples() == triples
    derivation_client.addTimeInstanceCurrentTimestamp(cf.SIMULATION_TIME_2)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT
    assert sparql_client.getAmountOfTriples() == triples

    # Request for derivation update and verify that no new triples have been added,
    # only emission values have been amended
    derivation_client.unifiedUpdateDerivation(derivation_iri)
    assert sparql_client.getAmountOfTriples() == triples

    # Retrieve updated emission derivation details
    _, derivation_outputs = cf.get_derivation_inputs_outputs(derivation_iri, sparql_client)
    # Verify correct number and details of updated derivation outputs
    assert len(derivation_outputs) == cf.OUTPUT_TYPES
    assert dm.OD_EMISSION in derivation_outputs
    assert len(derivation_outputs[dm.OD_EMISSION]) == len(agent.POLLUTANTS)
    emission_iris = list(derivation_outputs[dm.OD_EMISSION])
    emissions = sparql_client.get_emission_details(emission_iris)
    assert emissions == expected_results[1]

    print("All checks passed.")
