from pathlib import Path
from rdflib import Graph
from rdflib import RDF
import pytest
import time
import copy

import avgsqmpriceagent.datamodel as dm

from . import conftest as cf


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
    # Get SPARQL client from fixture
    sparql_client, _, rdb_url = initialise_clients

    ### TRIPPLE STORE ###
    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples (ABox & TBox)
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES)

    ### POSTGRESQL ###
    # Verify that Postgres database is empty
    assert cf.get_number_of_rdb_tables(rdb_url) == 0

    # Initialise and Upload time series
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify that expected tables and triples are created (i.e. dbTable + 1 ts table)
    assert cf.get_number_of_rdb_tables(rdb_url) == 2
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)

    # Verify correct retrieval of time series data
    dates, values = cf.retrieve_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI)
    assert dates == cf.DATES
    # Account for rounding errors
    assert pytest.approx(values, rel=1e-5) == cf.VALUES

    # Verify that dropping all tables works as expected
    cf.initialise_database(rdb_url)
    assert cf.get_number_of_rdb_tables(rdb_url) == 0


@pytest.mark.parametrize(
    "derivation_input_set, expected_postcode, expected_avg, local_agent_test",
    [
        (cf.DERIVATION_INPUTS_1, cf.POSTCODE_1, cf.AVGPRICE_1, True),   # local agent instance test
        (cf.DERIVATION_INPUTS_2, cf.POSTCODE_2, cf.AVGPRICE_2, True),  
        (cf.DERIVATION_INPUTS_3, cf.POSTCODE_2, cf.AVGPRICE_2, True), 
        (cf.DERIVATION_INPUTS_1, cf.POSTCODE_1, cf.AVGPRICE_1, False)   # deployed docker agent test
    ],
)
def test_monitor_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expected_postcode, 
    expected_avg, local_agent_test, mocker    
):
    """
        Test if derivation agent performs derivation update as expected, the `local_agent_test` 
        parameter controls if the agent performing the update is instantiating in memory (for quick
        debugging) or deployed in docker container (to mimic the production environment)
    """
    # -------------------------------------------------------------------------
    # Mock call to ONS API and simply return all transaction records as alias
    # for transaction records from nearby postcodes
    mocker.patch('avgsqmpriceagent.agent.avgprice_estimation.AvgSqmPriceAgent.get_transactions_from_nearest_postcodes',
                 return_value=cf.ALL_TRANSACTION_RECORDS)
    # -------------------------------------------------------------------------

    # Get required clients from fixtures
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # It first DELETES ALL DATA in the specified SPARQL/RDB endpoints
    # It then SPARQL updates all triples stated in test_triples folder to SPARQL endpoint +
    # Initialises PropertyPriceIndex time series and uploads test data to RDB
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_url)
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify correct number of triples (not marked up with timestamp yet)
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)

    # Create agent instance and register agent in KG
    # EXPLANATION: 
    # 1) Test Docker stack spins up Blazegraph, Postgres and Agent container, where agent
    #    endpoints (loaded from mocked `stack_configs_mock.py`) contain `docker.host.internal`
    #    to ensure intra-container communication
    # 2) However, successful agent registration within the KG cannot be guaranteed as both are within 
    #    the same Stack and sequence of startup (i.e. agent registration only after KG is available)
    #    cannot be guaranteed; however, this is required to properly pick up derivations
    # 3) Hence, the Dockerised agent is started without initial registration within the Stack and
    #    registration is done within the test to guarantee that Blazegraph will be ready
    # 4) The "belated" registration of the Dockerised agent can be achieved by registering "another local"
    #    agent instance with the same ONTOAGENT_SERVICE_IRI, while registering a "new" agent with a 
    #    different ONTOAGENT_SERVICE_IRI will actually register a local agent instance in the KG
    agent = create_example_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.OBE_AVERAGE_SM_PRICE)

    # Create derivation instance for new information
    # As of pyderivationagent==1.3.0 this also initialises all timestamps for pure inputs
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")
    
    # Expected number of triples after derivation registration
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 1    # number of inputs + derivation instance type
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_STATUS_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES
    triples += cf.DERIV_OUTPUT_TRIPLES

    # Verify correct number of triples (incl. timestamp & agent triples)
    assert sparql_client.getAmountOfTriples() == triples    

    if local_agent_test:
        # Start the scheduler to monitor derivations if it's local agent test
        agent._start_monitoring_derivations()

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # Query the output of the derivation instance
    derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
    
    # Verify that there are 2 derivation outputs (i.e. AveragePrice and Measure IRIs)
    assert len(derivation_outputs) == 2
    assert dm.OBE_AVERAGE_SM_PRICE in derivation_outputs
    assert len(derivation_outputs[dm.OBE_AVERAGE_SM_PRICE]) == 1
    assert dm.OM_MEASURE in derivation_outputs
    assert len(derivation_outputs[dm.OM_MEASURE]) == 1
    
    # Verify the values of the derivation output
    avg_iri = derivation_outputs[dm.OBE_AVERAGE_SM_PRICE][0]
    inputs, postcode, price = cf.get_avgsqmprice_details(sparql_client, avg_iri)
    # Verify postcode
    assert len(postcode) == 1
    assert postcode[0] == expected_postcode
    # Verify price
    assert len(price) == 1
    assert price[0] == expected_avg

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in inputs:
        for j in inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test
    if local_agent_test:
        agent.scheduler.shutdown()


@pytest.mark.parametrize(
    "derivation_input_set, expected_postcode, local_agent_test",
    [
        (cf.DERIVATION_INPUTS_3, cf.POSTCODE_2, True),   # local agent instance test
        (cf.DERIVATION_INPUTS_3, cf.POSTCODE_2, False)   # deployed docker agent test
    ],
)
def test_monitor_derivations_no_tx(
    initialise_clients, create_example_agent, derivation_input_set, expected_postcode,
    local_agent_test, mocker    
):
    """
        Test if derivation agent performs derivation update as expected, the `local_agent_test` 
        parameter controls if the agent performing the update is instantiating in memory (for quick
        debugging) or deployed in docker container (to mimic the production environment)
    """
    # -------------------------------------------------------------------------
    # Mock call to ONS API and simply return empty list
    mocker.patch('avgsqmpriceagent.agent.avgprice_estimation.AvgSqmPriceAgent.get_transactions_from_nearest_postcodes',
                 return_value=[])
    # -------------------------------------------------------------------------

    # Get required clients from fixtures
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_url)
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify correct number of triples (not marked up with timestamp yet)
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)

    # Create agent instance and register agent in KG
    agent = create_example_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.OBE_AVERAGE_SM_PRICE)

    # Create derivation instance for new information
    # As of pyderivationagent==1.3.0 this also initialises all timestamps for pure inputs
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")
    
    # Expected number of triples after derivation registration
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 1    # number of inputs + derivation instance type
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_STATUS_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES
    triples += cf.DERIV_OUTPUT_TRIPLES

    # Verify correct number of triples (incl. timestamp & agent triples)
    assert sparql_client.getAmountOfTriples() == triples    

    if local_agent_test:
        # Start the scheduler to monitor derivations if it's local agent test
        agent._start_monitoring_derivations()

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # Query the output of the derivation instance
    derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
    print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")

    # Verify that there are 2 derivation outputs (i.e. AveragePrice and Measure IRIs)
    assert len(derivation_outputs) == 2
    assert dm.OBE_AVERAGE_SM_PRICE in derivation_outputs
    assert len(derivation_outputs[dm.OBE_AVERAGE_SM_PRICE]) == 1
    assert dm.OM_MEASURE in derivation_outputs
    assert len(derivation_outputs[dm.OM_MEASURE]) == 1

    # Verify that no average price details are instantiated
    avg_iri = derivation_outputs[dm.OBE_AVERAGE_SM_PRICE][0]
    inputs, postcode, price = cf.get_avgsqmprice_details(sparql_client, avg_iri)
    # Verify postcode
    assert len(postcode) == 1
    assert postcode[0] == expected_postcode
    # Verify price
    assert price[0] is None

    # Verify inputs (i.e. derived from)
    # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
    derivation_input_set_copy = copy.deepcopy(derivation_input_set)
    for i in inputs:
        for j in inputs[i]:
            assert j in derivation_input_set_copy
            derivation_input_set_copy.remove(j)
    assert len(derivation_input_set_copy) == 0

    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test
    if local_agent_test:
        agent.scheduler.shutdown()


@pytest.mark.parametrize(
    "derivation_input_set, expected_postcode, expected_avg, local_agent_test",
    [
        (cf.DERIVATION_INPUTS_3, cf.POSTCODE_2, cf.AVGPRICE_2, True)   # local agent instance test
    ],
)
def test_sequence_of_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expected_postcode,
    expected_avg, local_agent_test, mocker    
):
    """
        Test if derivation agent performs derivation update as expected in case
        of a sequence of (un-)successful derivations, i.e. successful avg calculation 
        followed by unsuccessful followed by successful avg calculation.
        NOTE: This test is only for local agent instance as mocking with side_effects 
              is rather tricky for Dockerised version
    """

    # Define subsequent derivation runs and expected behaviour, i.e.
    # [ success of average calculation (boolean), 
    #   expected change in instantiated triples (compared to previous run)]
    runs = {'run1': [True, cf.DERIV_OUTPUT_COMPUATBLE],
            'run2': [False, cf.DERIV_OUTPUT_NONCOMPUATBLE - cf.DERIV_OUTPUT_COMPUATBLE],
            'run3': [True, cf.DERIV_OUTPUT_COMPUATBLE - cf.DERIV_OUTPUT_NONCOMPUATBLE] 
           }

    # -------------------------------------------------------------------------
    # Mock call to ONS API with different results for recurring calls
    mocker.patch('avgsqmpriceagent.agent.avgprice_estimation.AvgSqmPriceAgent.get_transactions_from_nearest_postcodes',
                 side_effect=[cf.ALL_TRANSACTION_RECORDS, [], cf.ALL_TRANSACTION_RECORDS])
    # -------------------------------------------------------------------------

    # Get required clients from fixtures
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_url)
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify correct number of triples (not marked up with timestamp yet)
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)

    # Create agent instance and register agent in KG
    agent = create_example_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, RDF.type.toPython(), dm.OBE_AVERAGE_SM_PRICE)

    # Create derivation instance for new information
    # As of pyderivationagent==1.3.0 this also initialises all timestamps for pure inputs
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")
    
    # Expected number of triples after derivation registration
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 1    # number of inputs + derivation instance type
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_STATUS_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES
    triples += cf.DERIV_OUTPUT_TRIPLES

    # Verify correct number of triples (incl. timestamp & agent triples)
    assert sparql_client.getAmountOfTriples() == triples    

    if local_agent_test:
        # Start the scheduler to monitor derivations if it's local agent test
        agent._start_monitoring_derivations()

    # Initialise timestamp of the derivation instance
    currentTimestamp_derivation = 0
    lastUpdateTimestamp_derivation = 0

    for r in runs:
        # Query timestamp of the derivation for every 10 seconds until it's updated
        while currentTimestamp_derivation <= lastUpdateTimestamp_derivation:
            time.sleep(10)
            currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)
        lastUpdateTimestamp_derivation = currentTimestamp_derivation

        # Assert expected number of triples
        triples += runs[r][1]
        assert sparql_client.getAmountOfTriples() == triples   

        # Query the output of the derivation instance
        derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
        print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")

        # Verify that there are 2 derivation outputs (i.e. AveragePrice and Measure IRIs)
        assert len(derivation_outputs) == 2
        assert dm.OBE_AVERAGE_SM_PRICE in derivation_outputs
        assert len(derivation_outputs[dm.OBE_AVERAGE_SM_PRICE]) == 1
        assert dm.OM_MEASURE in derivation_outputs
        assert len(derivation_outputs[dm.OM_MEASURE]) == 1

        # Verify the values of the derivation output
        avg_iri = derivation_outputs[dm.OBE_AVERAGE_SM_PRICE][0]
        inputs, postcode, price = cf.get_avgsqmprice_details(sparql_client, avg_iri)
        # Verify postcode
        assert len(postcode) == 1
        assert postcode[0] == expected_postcode
        # Verify price
        if runs[r][0]:
            assert len(price) == 1
            assert price[0] == expected_avg
        else:
            assert price[0] is None

        # Verify inputs (i.e. derived from)
        # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
        derivation_input_set_copy = copy.deepcopy(derivation_input_set)
        for i in inputs:
            for j in inputs[i]:
                assert j in derivation_input_set_copy
                derivation_input_set_copy.remove(j)
        assert len(derivation_input_set_copy) == 0

        # Request for derivation update 
        # (after updating timestamp of pure input to trigger new derivation)
        any_pure_input = [i for inp in inputs.values() for i in inp][0]
        derivation_client.updateTimestamp(any_pure_input)
        derivation_client.updateDerivationAsyn(derivation_iri)

    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test
    if local_agent_test:
        agent.scheduler.shutdown()