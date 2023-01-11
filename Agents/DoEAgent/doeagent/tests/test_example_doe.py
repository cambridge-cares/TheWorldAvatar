import pytest
import time

import doeagent.tests.utils as utils


@pytest.mark.parametrize(
    "doe_iri,derivation_inputs,local_agent_test",
    [
        (utils.cf.DOE_IRI, utils.cf.DERIVATION_INPUTS, True), # local agent instance test
        (utils.cf.DOE_IRI, utils.cf.DERIVATION_INPUTS, False), # deployed docker agent test
        (utils.cf.DOE_NO_PRIOR_EXPERIMENT_IRI, [utils.cf.DOE_NO_PRIOR_EXPERIMENT_IRI], True), # local agent instance test
        (utils.cf.DOE_NO_PRIOR_EXPERIMENT_IRI, [utils.cf.DOE_NO_PRIOR_EXPERIMENT_IRI], False), # deployed docker agent test
        (utils.cf.DOE_ANOTHER_TEST_NO_PRIOR_EXPERIMENT_IRI, [utils.cf.DOE_ANOTHER_TEST_NO_PRIOR_EXPERIMENT_IRI], True), # local agent instance test
        (utils.cf.DOE_ANOTHER_TEST_NO_PRIOR_EXPERIMENT_IRI, [utils.cf.DOE_ANOTHER_TEST_NO_PRIOR_EXPERIMENT_IRI], False), # deployed docker agent test
    ],
)
def test_example_doe(
    initialise_clients, create_doe_agent,
    doe_iri, derivation_inputs, local_agent_test
):
    sparql_client, derivation_client = initialise_clients

    # Initialise all triples in the knowledge graph
    utils.initialise_triples(sparql_client)

    # Create agent instance, register agent in KG
    doe_agent = create_doe_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        doe_agent._start_monitoring_derivations()

    # Assert that there's currently no new experiment associated with the DoE instance
    old_doe_instance = sparql_client.get_doe_instance(doe_iri)
    assert old_doe_instance.proposesNewExperiment is None

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(doe_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    # Query timestamp of the derivation for every 20 seconds until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    # Query the iri of the new proposed NewExperiment
    new_doe_instance = sparql_client.get_doe_instance(doe_iri)
    assert new_doe_instance.proposesNewExperiment is not None
    new_exp_instance = new_doe_instance.proposesNewExperiment
    new_exp_iri = new_exp_instance.instance_iri
    print(f"New experiment suggested successfully, suggested experiment instance: {new_exp_iri}")

    # Check if all the suggested conditions are within the DoE range
    for design_variable in new_doe_instance.hasDomain.hasDesignVariable:
        if isinstance(design_variable, utils.cf.ContinuousVariable):
            rxn_cond = new_exp_instance.get_reaction_condition(design_variable.refersTo.clz, design_variable.positionalID)
            assert rxn_cond.hasValue.hasNumericalValue <= design_variable.upperLimit
            assert design_variable.lowerLimit <= rxn_cond.hasValue.hasNumericalValue
        else:
            # TODO add checks for CategoricalVariable
            pass
    # Check if all the fixed parameters are the same as the DoE instance
    if new_doe_instance.hasDomain.hasFixedParameter is not None:
        for fixed_parameter in new_doe_instance.hasDomain.hasFixedParameter:
            rxn_cond = new_exp_instance.get_reaction_condition(fixed_parameter.refersTo.clz, fixed_parameter.positionalID)
            assert rxn_cond.hasValue.hasNumericalValue == fixed_parameter.refersTo.hasValue.hasNumericalValue
    print("All check passed.")

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    if local_agent_test:
        doe_agent.scheduler.shutdown()
