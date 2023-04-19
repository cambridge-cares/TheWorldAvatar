from pathlib import Path
from rdflib import Graph
import pytest
import time
import copy

import propertyvalueestimation.datamodel as dm
from propertyvalueestimation.datamodel.data import GBP_SYMBOL

from . import conftest as cf
from tests.mockutils.env_configs_mock import DOCKERISED_TEST


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
    "derivation_input_set, expect_exception, expected_estimate",
    [
        (cf.DERIVATION_INPUTS_1, False, cf.MARKET_VALUE_1),
        (cf.DERIVATION_INPUTS_2, False, cf.MARKET_VALUE_1),
        (cf.DERIVATION_INPUTS_3, False, cf.MARKET_VALUE_2),
        (cf.DERIVATION_INPUTS_4, True, cf.EXCEPTION_STATUS_1),
    ],
)
def test_monitor_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expect_exception, expected_estimate    
):
    """
        Test if derivation agent performs derivation update as expected
        The global variable DOCKERISED_TEST controls if the test (incl. agent) is running locally
        within memory or deployed in docker container (to mimic the production environment)
    """

    # Get required clients from fixtures
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    # It first DELETES ALL DATA in the specified SPARQL/RDB endpoints
    # It then SPARQL updates all triples stated in test_triples folder to SPARQL endpoint +
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_url)
    # Initialises PropertyPriceIndex time series and uploads test data to RDB
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    assert sparql_client.getAmountOfTriples() == triples

    # Create agent instance and register agent in KG
    # - Successful agent registration within the KG is required to pick up markup up derivations
    # - Hence, the Dockerised agent is started without initial registration within the Stack and
    #   registration is done within the test to guarantee that test Blazegraph will be ready
    # - The "belated" registration of the Dockerised agent can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    agent = create_example_agent()

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.OM_AMOUNT_MONEY)

    # Create derivation instance for new information
    # As of pyderivationagent>=1.3.0 this also initialises all timestamps for pure inputs
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")
    
    # Expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 1    # number of inputs + derivation instance type
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_STATUS_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES
    triples += cf.DERIV_OUTPUT_TRIPLES

    # Verify correct number of triples (incl. timestamp & agent triples)
    assert sparql_client.getAmountOfTriples() == triples    

    if not DOCKERISED_TEST:
        # Start the scheduler to monitor derivations if it's local agent test
        agent._start_monitoring_derivations()

    if expect_exception:
        # Verify that agent throws (i.e. instantiates) correct Exception message for
        # erroneous derivation markup
        time.sleep(10)
        exception = cf.get_derivation_status(sparql_client, derivation_iri)
        assert expected_estimate in exception

    else:
        t1 = time.time()
        # Query timestamp of the derivation for every 10 seconds until it's updated
        currentTimestamp_derivation = 0
        while currentTimestamp_derivation == 0:
            # Fail test if after 60s no derivation has been identified/updated
            if time.time() - t1 > 60:
                pytest.fail()
            time.sleep(10)
            currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

        # Assert that there's now an instance with rdf:type of the output signature in the KG
        assert sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.OM_AMOUNT_MONEY)

        # Verify correct number of triples (incl. timestamp & agent triples)
        triples += cf.MARKET_VALUE_COMPUTABLE
        assert sparql_client.getAmountOfTriples() == triples    

        # Query the output of the derivation instance
        derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
        print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
        
        # Verify that there are 2 derivation outputs (i.e. AmountOfMoney and Measure IRIs)
        assert len(derivation_outputs) == 2
        assert dm.OM_AMOUNT_MONEY in derivation_outputs
        assert len(derivation_outputs[dm.OM_AMOUNT_MONEY]) == 1
        assert dm.OM_MEASURE in derivation_outputs
        assert len(derivation_outputs[dm.OM_MEASURE]) == 1
        
        # Verify the values of the derivation output
        market_value_iri = derivation_outputs[dm.OM_AMOUNT_MONEY][0]
        inputs, market_value, unit = cf.get_marketvalue_details(sparql_client, market_value_iri)
        # Verify market value
        assert len(market_value) == 1
        assert pytest.approx(market_value[0], rel=1e-5) == expected_estimate
        # Verify monetary unit symbol (due to previously observed encoding issues)
        assert len(unit) == 1
        assert unit[0] == GBP_SYMBOL

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
    if not DOCKERISED_TEST:
        agent.scheduler.shutdown()


@pytest.mark.parametrize(
    "derivation_input_set, expected_estimate",
    [
        (cf.DERIVATION_INPUTS_3, cf.MARKET_VALUE_2),
    ],
)
def test_sequence_of_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expected_estimate    
):
    """
        Test if derivation agent performs derivation update as expected in case
        of a sequence of (un-)successful derivations, i.e. successful property 
        estimation followed by unsuccessful followed by successful assessment
    """

    # Get required clients from fixtures
    sparql_client, derivation_client, rdb_url = initialise_clients

    # Initialise all triples in test_triples + initialise time series in RDB
    cf.initialise_triples(sparql_client)
    cf.initialise_database(rdb_url)
    # Initialises PropertyPriceIndex time series and uploads test data to RDB
    cf.initialise_timeseries(kgclient=sparql_client, rdb_url=rdb_url, 
                             rdb_user=cf.DB_USER, rdb_password=cf.DB_PASSWORD,
                             dataIRI=cf.PRICE_INDEX_INSTANCE_IRI,
                             dates=cf.DATES, values=cf.VALUES)

    # -------------------------------------------------------------------------
    # Retrieve average square metre price measure instance and value
    measure, value = cf.get_avgprice_and_measure(sparql_client, cf.DERIVATION_INPUTS_3)
    triple = f'<{measure}> <{dm.OM_NUM_VALUE}> \"{value}\"^^<{dm.XSD_FLOAT}>'

    # Define SPARQL updates to amend KG between subsequent derivation runs 
    # and expected behaviour, i.e.
    # [ SPARQL update, affected triples (+/-),
    #   success of average calculation (boolean), 
    #   expected change in instantiated triples (compared to previous run)]
    runs = {'run1': [f'DELETE DATA {{ {triple} }}', -1,
                     True, cf.MARKET_VALUE_COMPUTABLE],
            'run2': [f'INSERT DATA {{ {triple} }}', +1,
                     False, cf.MARKET_VALUE_NONCOMPUTABLE - cf.MARKET_VALUE_COMPUTABLE],
            'run3': ['', 0, True, cf.MARKET_VALUE_COMPUTABLE - cf.MARKET_VALUE_NONCOMPUTABLE] 
           }
    # -------------------------------------------------------------------------


    # Verify correct number of triples (not marked up with timestamp yet)
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    assert sparql_client.getAmountOfTriples() == triples

    # Create agent instance and register agent in KG
    agent = create_example_agent()

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.OM_AMOUNT_MONEY)

    # Create derivation instance for new information
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, derivation_input_set)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")
    
    # Expected number of triples after derivation registration
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT * len(derivation_input_set) # timestamps for pure inputs
    triples += cf.TIME_TRIPLES_PER_PURE_INPUT                             # timestamps for derivation instance
    triples += len(derivation_input_set) + 1    # number of inputs + derivation instance type
    triples += cf.AGENT_SERVICE_TRIPLES
    triples += cf.DERIV_STATUS_TRIPLES
    triples += cf.DERIV_INPUT_TRIPLES
    triples += cf.DERIV_OUTPUT_TRIPLES

    # Verify correct number of triples (incl. timestamp & agent triples)
    assert sparql_client.getAmountOfTriples() == triples    

    if not DOCKERISED_TEST:
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
        triples += runs[r][3]
        assert sparql_client.getAmountOfTriples() == triples   

        # Query the output of the derivation instance
        derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
        print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
        
        # Verify that there are 2 derivation outputs (i.e. AmountOfMoney and Measure IRIs)
        assert len(derivation_outputs) == 2
        assert dm.OM_AMOUNT_MONEY in derivation_outputs
        assert len(derivation_outputs[dm.OM_AMOUNT_MONEY]) == 1
        assert dm.OM_MEASURE in derivation_outputs
        assert len(derivation_outputs[dm.OM_MEASURE]) == 1
        
        # Verify the values of the derivation output
        market_value_iri = derivation_outputs[dm.OM_AMOUNT_MONEY][0]
        inputs, market_value, _ = cf.get_marketvalue_details(sparql_client, market_value_iri)
        # Verify market value
        if runs[r][2]:
        # Verify market value
            assert len(market_value) == 1
            assert pytest.approx(market_value[0], rel=1e-5) == expected_estimate
        else:
            assert market_value == []

        # Verify inputs (i.e. derived from)
        # Create deeepcopy to avoid modifying original cf.DERIVATION_INPUTS_... between tests
        derivation_input_set_copy = copy.deepcopy(derivation_input_set)
        for i in inputs:
            for j in inputs[i]:
                assert j in derivation_input_set_copy
                derivation_input_set_copy.remove(j)
        assert len(derivation_input_set_copy) == 0

        # Update KG with SPARQL update
        sparql_client.performUpdate(runs[r][0])
        triples += runs[r][1]
        # Request for derivation update 
        # (after updating timestamp of pure input to trigger new derivation)
        any_pure_input = [i for inp in inputs.values() for i in inp][0]
        derivation_client.updateTimestamp(any_pure_input)
        derivation_client.unifiedUpdateDerivation(derivation_iri)

    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test
    if not DOCKERISED_TEST:
        agent.scheduler.shutdown()


@pytest.mark.skipif(not DOCKERISED_TEST, reason="Test only applicable for Dockerised test")
def test_wrongly_marked_up_derivations(
    initialise_clients, create_example_agent    
):
    """
        Test that no derivation outputs are generated if the markup is incorrect, i.e.
        an agent with a different ONTOAGENT_SERVICE_IRI gets registered
    """

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
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES + cf.TS_TRIPLES)
    assert sparql_client.getAmountOfTriples() == triples

    # Create agent instance and register agent in KG; however, use a different
    # ONTOAGENT_SERVICE_IRI than the one started when spinning up the Dockerised agent
    agent = create_example_agent(alter_agent_iri=True)

    # Create derivation instance for new information
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(agent.agentIRI, cf.DERIVATION_INPUTS_1)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    with pytest.raises(Exception, match="No derivation has been identified/updated"):

        t1 = time.time()
        # Query timestamp of the derivation for every 10 seconds until it's updated
        currentTimestamp_derivation = 0
        while currentTimestamp_derivation == 0:
            # Raise exception if after 60s no derivation has been identified/updated
            if time.time() - t1 > 60:
                raise Exception("No derivation has been identified/updated")
            time.sleep(10)
            currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)
