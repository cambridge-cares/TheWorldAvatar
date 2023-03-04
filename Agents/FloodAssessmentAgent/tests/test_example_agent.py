from pathlib import Path
from rdflib import Graph
import pytest
import time
import copy

import floodassessment.datamodel as dm

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
        This test checks that all example data gets correctly instantiated
    """
    # Get SPARQL client from fixture
    sparql_client, _ = initialise_clients

    # Verify that KG is empty
    assert sparql_client.getAmountOfTriples() == 0

    # Upload example test triples (ABox & TBox)
    cf.initialise_triples(sparql_client)

    # Verify instantiation of expected number of triples
    assert sparql_client.getAmountOfTriples() == (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES)

    # Verify proper rdf:type declarations
    for bldg in cf.BUILDINGS:
        assert sparql_client.checkInstanceClass(bldg, dm.OBE_BUILDING)
    for marketvalue in cf.MARKET_VALUES:
        assert sparql_client.checkInstanceClass(marketvalue, dm.OM_AMOUNT_MONEY)
    for warning in cf.FLOOD_WARNINGS:
        assert sparql_client.checkInstanceClass(warning, dm.FLOOD_ALERT_WARNING)

    # Verify that poundSterling symbol is correctly retrieved and decoded
    # NOTE: `summarise_affected_property_values`` summarises only market values with
    #       correct poundSterling symbol --> passing this test implies that symbol
    #       is correctly retrieved and decoded
    market_value = sparql_client.summarise_affected_property_values(cf.MARKET_VALUES)
    assert pytest.approx(market_value, rel=1e-5) == cf.FLOOD_ASSESSMENT_2[1]


@pytest.mark.parametrize(
    "derivation_input_set, expect_exception, expected_deriv_triples, expected_deriv_types, expected_assessment",
    [
        (cf.DERIVATION_INPUTS_1, False, cf.DERIVATION_TRIPLES_1, cf.DERIVATION_OUTPUTS_1, cf.FLOOD_ASSESSMENT_1),   # Inactive flood warning --> values at risk = 0
        (cf.DERIVATION_INPUTS_2, False, cf.DERIVATION_TRIPLES_2, cf.DERIVATION_OUTPUTS_2, cf.FLOOD_ASSESSMENT_2),   # Active flood warnings --> actual values at risk
        (cf.DERIVATION_INPUTS_3, False, cf.DERIVATION_TRIPLES_2, cf.DERIVATION_OUTPUTS_2, cf.FLOOD_ASSESSMENT_3),
        (cf.DERIVATION_INPUTS_5, False, cf.DERIVATION_TRIPLES_5, cf.DERIVATION_OUTPUTS_5, cf.FLOOD_ASSESSMENT_5),
        (cf.DERIVATION_INPUTS_4, True, None, None, cf.EXCEPTION_STATUS_1),                                          # Test exception        
    ],
)
def test_monitor_derivations(
    initialise_clients, create_example_agent, derivation_input_set, expect_exception, 
    expected_deriv_triples, expected_deriv_types, expected_assessment, mocker
):
    """
        Test if derivation agent performs derivation update as expected
        The global variable DOCKERISED_TEST controls if the test (incl. agent) is running locally
        within memory or deployed in docker container (to mimic the production environment)
    """

    # -------------------------------------------------------------------------
    # Mock method call to assess number of people potentially affected by flood
    # (normally to be assessed using PostGIS' geospatial capabilities via Ontop)
    mocker.patch('floodassessment.agent.impact_assessment.FloodAssessmentAgent.estimate_number_of_affected_people',
                 return_value=None)
    # -------------------------------------------------------------------------

    # Get required clients from fixtures
    sparql_client, derivation_client = initialise_clients

    # Initialise all triples in test_triples (It first DELETES ALL DATA in the 
    # specified SPARQL endpoint, before uploading all triples in test_triples folder)
    cf.initialise_triples(sparql_client)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES)
    assert sparql_client.getAmountOfTriples() == triples

    # Create agent instance and register agent in KG
    # - Successful agent registration within the KG is required to pick up marked up derivations
    # - Hence, the Dockerised agent is started without initial registration within the Stack and
    #   registration is done within the test to guarantee that test Blazegraph will be ready
    # - The "belated" registration of the Dockerised agent can be achieved by registering "another"
    #   agent instance with the same ONTOAGENT_SERVICE_IRI
    agent = create_example_agent()

    # Assert that there's currently no instance having rdf:type of the output signature in the KG
    assert not sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.FLOOD_POPULATION)
    assert not sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.FLOOD_BUILDING)
    assert not sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.FLOOD_IMPACT)

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
        assert expected_assessment in exception

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
        #NOTE: Flood:Buildings will always get instantiated (even if no buildings are at risk)
        assert sparql_client.check_if_triple_exist(None, dm.RDF_TYPE, dm.FLOOD_BUILDING)

        # Verify correct number of triples (incl. timestamp & agent triples)
        triples += expected_deriv_triples
        assert sparql_client.getAmountOfTriples() == triples    

        # Query the output of the derivation instance
        derivation_outputs = cf.get_derivation_outputs(derivation_iri, sparql_client)
        print(f"Generated derivation outputs that belongsTo the derivation instance: {', '.join(derivation_outputs)}")
        
        # Verify that derivation outputs (i.e. "belongsTo") match expected number and types
        expected_output = {k:expected_deriv_types.count(k) for k in set(expected_deriv_types)}
        assert len(derivation_outputs) == len(expected_output)
        for k in expected_output:
            assert k in derivation_outputs
            assert len(derivation_outputs[k]) == expected_output[k]
        
        # Verify the values of the derivation output
        inputs, impacts = cf.get_flood_assessment_details(sparql_client, derivation_iri)
        # Verify number of buildings at risk
        assert len(impacts['bldgs']) == 1
        assert pytest.approx(impacts['bldgs'][0], rel=1e-5) == expected_assessment[0]
        # Verify value of buildings at risk
        if impacts.get('bldg_value'):
            assert len(impacts['bldg_value']) == 1
            assert pytest.approx(impacts['bldg_value'][0], rel=1e-5) == expected_assessment[1]
            # Verify monetary unit symbol (due to previously observed encoding issues)
            assert len(impacts['bldg_unit']) == 1
            assert impacts['bldg_unit'][0] == dm.GBP_SYMBOL
        # Verify impact estimate
        if impacts.get('impact_value'):
            assert len(impacts['impact_value']) == 1
            assert pytest.approx(impacts['impact_value'][0], rel=1e-5) == expected_assessment[1]
            # Verify monetary unit symbol (due to previously observed encoding issues)
            assert len(impacts['impact_unit']) == 1
            assert impacts['impact_unit'][0] == dm.GBP_SYMBOL
        # Verify number of people at risk
        if impacts.get('population'):
            assert len(impacts['population']) == 1
            assert pytest.approx(impacts['population'][0], rel=1e-5) == expected_assessment[2]

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


@pytest.mark.skipif(not DOCKERISED_TEST, reason="Test only applicable for Dockerised test")
def test_wrongly_marked_up_derivations(
    initialise_clients, create_example_agent    
):
    """
        Test that no derivation outputs are generated if the markup is incorrect, i.e.
        an agent with a different ONTOAGENT_SERVICE_IRI gets registered
    """

    # Get required clients from fixtures
    sparql_client, derivation_client = initialise_clients

    # Initialise all triples in test_triples
    cf.initialise_triples(sparql_client)

    # Verify correct number of triples (not marked up with timestamp yet)
    triples = (cf.TBOX_TRIPLES + cf.ABOX_TRIPLES)
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
