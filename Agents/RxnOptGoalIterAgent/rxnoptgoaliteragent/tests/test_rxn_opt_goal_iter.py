from rdflib import URIRef
from rdflib import Literal
import pytest
import time

import chemistry_and_robots.kg_operations.dict_and_list as dal
import rxnoptgoaliteragent.tests.conftest as cf

# ----------------------------------------------------------------------------------
# Test cases for RxnOptGoalIterAgent
# ----------------------------------------------------------------------------------

@pytest.mark.parametrize(
    "goal_set_iri,lst_existing_result_quantity,derivation_inputs,local_agent_test",
    [
        (cf.IRIs.GOALSET_1.value, [cf.IRIs.RESULT_QUANTITY_1.value], cf.IRIs.DERIVATION_INPUTS.value, True), # local agent instance test
        (cf.IRIs.GOALSET_1.value, [cf.IRIs.RESULT_QUANTITY_1.value], cf.IRIs.DERIVATION_INPUTS.value, False), # deployed docker agent test
        (cf.IRIs.GOALSET_1.value, [cf.IRIs.RESULT_QUANTITY_1.value], cf.IRIs.DERIVATION_INPUTS_NO_PRIOR_DATA.value, True), # local agent instance test
        (cf.IRIs.GOALSET_1.value, [cf.IRIs.RESULT_QUANTITY_1.value], cf.IRIs.DERIVATION_INPUTS_NO_PRIOR_DATA.value, False), # deployed docker agent test
    ],
)
def test_rogi_agent(
    initialise_clients, create_rogi_agent,
    goal_set_iri, lst_existing_result_quantity, derivation_inputs, local_agent_test
):
    sparql_client, derivation_client = initialise_clients

    # Initialise all triples in the knowledge graph
    cf.initialise_triples(sparql_client, derivation_client, derivation_inputs)

    # Create agent instance, register agent in KG
    rogi_agent = create_rogi_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        rogi_agent._start_monitoring_derivations()

    # Get the goal set instance
    goal_set_instance = sparql_client.get_goal_set_instance(goal_set_iri)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(rogi_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    # Wait until the postpro derivation is created
    postpro_derivation_iri = None
    while not postpro_derivation_iri:
        time.sleep(10)
        postpro_derivation_iri = cf.get_postpro_derivation(goal_set_iri, sparql_client)

    # Create dummy triples that mimic the output of the postpro derivation
    # This function also uploads dummy triples to KG, later these will be picked up by ROGI agent
    g = cf.create_dummy_triples_for_performance_indicator(goal_set_instance, postpro_derivation_iri, sparql_client)

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = cf.get_timestamp(derivation_iri, sparql_client)

    # Query the output triples about OntoGoal:Result of the derivation
    lst_new_result = cf.get_result_from_rogi_derivation(derivation_iri, sparql_client)
    lst_new_result_quantity = [r.refersTo for r in lst_new_result]
    # Query the new goal set instance, the each new result should be connected to corresponding goal
    new_goal_set_instance = sparql_client.get_goal_set_instance(goal_set_iri)
    # new_goal_set_instance = cf.GoalSet()
    lst_new_goal_result_quantity = [res.instance_iri for goal in new_goal_set_instance.hasGoal for res in goal.hasResult if res.instance_iri not in lst_existing_result_quantity]
    assert len(lst_new_result_quantity) == len(lst_new_goal_result_quantity)
    assert dal.check_if_two_lists_equal(lst_new_result_quantity, lst_new_goal_result_quantity)
    for goal in new_goal_set_instance.hasGoal:
        for result in goal.hasResult:
            if result.instance_iri not in lst_existing_result_quantity:
                assert (URIRef(result.instance_iri), URIRef(cf.OM_HASVALUE), URIRef(result.hasValue.instance_iri)) in g
                assert (URIRef(result.hasValue.instance_iri), URIRef(cf.OM_HASUNIT), URIRef(result.hasValue.hasUnit)) in g
                assert (URIRef(result.hasValue.instance_iri), URIRef(cf.OM_HASNUMERICALVALUE), Literal(result.hasValue.hasNumericalValue)) in g

    # Shutdown the scheduler to clean up if it's local agent test (as the rogi_agent scheduler must have started)
    if local_agent_test:
        rogi_agent.scheduler.shutdown()
