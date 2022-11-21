# this file tests the integration of the rxn and goal
# 1. rogi agent and rxn given pre-defined triples
# 1.1 add test assertions to test_rxn_integration.py
# 1.2 reuse the test assertions in test_rxn_with_goal.py for this test
# 2. rogi agent and rxn given a goal request
# 3. rogi agent and rxn given a goal request with iteration

# sequence
# 1.1, 1.2 (then 1 will be done)
# 2 (then the whole workflow is tested for one experiment)
# redesign the way to pass the doe info, amend 2
# 3 (this is to test the iterative process)
# to do this, we need to modify the execution agent part about autosampler
# need to double check with Connor if we can make manual collection of samples
# so that we can manually select the autosampler site
# then, we can test the iterative process


import pytest
import time

import tests.conftest as cf

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')


# ----------------------------------------------------------------------------------
# Test cases for the integration of the rxn and goal
# i.e. the connection between all reaction optimisation agents
# RxnOptGoalAgent/RxnOptGoalIterAgent/DoEAgent/VapourtecScheduleAgent/HPLCPostProAgent/VapourtecAgent/HPLCAgent
# 1. rogi agent and all rxn agents given pre-defined goal triples (leave the ROG agent out)
# 2. TODO rogi agent and all rxn agents given a goal request (all seven agents are involved)
# 3. TODO rogi agent and all rxn agents given a goal request with iteration (all seven agents are involved)
# ----------------------------------------------------------------------------------
# def test_(initialise_blazegraph_fileserver_with_test_triples):
#     sparql_url, fileserver_url = initialise_blazegraph_fileserver_with_test_triples
#     print(sparql_url)
#     print(fileserver_url)
#     time.sleep(600)

@pytest.mark.parametrize(
    "goal_set_iri,derivation_inputs,hplc_report_target_folder,fcexp_file_container_folder,local_agent_test",
    [
        (cf.IRIs.GOALSET_1.value, cf.IRIs.DERIVATION_INPUTS.value, cf.HPLC_REPORT_LOCAL_TEST_DIR, cf.FCEXP_FILE_DIR, True),
        # (cf.rogi_cf.IRIs.GOALSET_1.value, cf.rogi_cf.IRIs.DERIVATION_INPUTS.value, cf.DOCKER_INTEGRATION_DIR, None, False),
    ],
)
def test_rxn_rogi(
    initialise_blazegraph_fileserver_with_test_triples,
    # initialise_test_triples,
    create_rogi_agent, create_doe_agent, create_vapourtec_schedule_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    goal_set_iri, derivation_inputs, hplc_report_target_folder, fcexp_file_container_folder, local_agent_test
):
    # endpoint = initialise_test_triples
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    rogi_agent = create_rogi_agent(
        register_agent=True,
    )
    doe_agent = create_doe_agent(
        register_agent=True,
    )
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        register_agent=True,
    )
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    vapourtec_agent = create_vapourtec_agent(
        env_file=cf.LAB1_VAPOURTEC_AGENT_ENV,
        register_agent=True,
        fcexp_file_container_folder=fcexp_file_container_folder,
        dry_run=True,
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=cf.LAB1_HPLC_AGENT_ENV,
        register_agent=True,
        hplc_report_container_dir=hplc_report_target_folder,
        dry_run=True,
    )

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        rogi_agent.start_all_periodical_job()
        doe_agent.start_all_periodical_job()
        vapourtec_schedule_agent.start_all_periodical_job()
        hplc_postpro_agent.start_all_periodical_job()
        vapourtec_agent._start_monitoring_derivations()
        hplc_agent.start_all_periodical_job()
        vapourtec_agent.sparql_client.update_vapourtec_rs400_state(vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    # Get the goal set instance
    goal_set_instance = rogi_agent.sparql_client.get_goal_set_instance(goal_set_iri)

    # Add timestamp to the derivation_inputs
    for input in derivation_inputs:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(rogi_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance for ROGI agent: {derivation_iri}")

    # Wait until the rogi derivation is done
    rogi_completed_one_iter = False
    while not rogi_completed_one_iter:
        time.sleep(10)
        rogi_completed_one_iter = sparql_client.check_if_rogi_complete_one_iter(derivation_iri)
        print(f"ROGI derivation {derivation_iri} is completed: {rogi_completed_one_iter}, current time: {time.time()}")

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    if local_agent_test:
        rogi_agent.scheduler.shutdown()
        doe_agent.scheduler.shutdown()
        vapourtec_schedule_agent.scheduler.shutdown()
        hplc_postpro_agent.scheduler.shutdown()
        vapourtec_agent.scheduler.shutdown()
        hplc_agent.scheduler.shutdown()

    try:
        # Test all triples are correctly generated
        cf.assert_one_rxn_iteration(
            sparql_client,
            doe_agent,
            vapourtec_schedule_agent,
            vapourtec_agent,
            hplc_agent,
            hplc_postpro_agent,
            rogi_agent,
            goal_set_iri,
        )
    except Exception as e:
        print(e)
        raise e
