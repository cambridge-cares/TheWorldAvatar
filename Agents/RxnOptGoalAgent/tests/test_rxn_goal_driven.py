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

import logging
logging.getLogger("chemistry_and_robots_sparql_client").setLevel(logging.INFO)
logger = logging.getLogger('test_rxn_with_goal')


# ----------------------------------------------------------------------------------
# Test cases for the integration of the rxn and goal
# i.e. the connection between all reaction optimisation agents
# RxnOptGoalAgent/RxnOptGoalIterAgent/DoEAgent/VapourtecExecutionAgent/HPLCPostProAgent/VapourtecAgent/HPLCAgent
# 1. rogi agent and all rxn agents given pre-defined goal triples (leave the ROG agent out)
# 2. TODO rogi agent and all rxn agents given a goal request (all seven agents are involved)
# 3. TODO rogi agent and all rxn agents given a goal request with iteration (all seven agents are involved)
# ----------------------------------------------------------------------------------
# def test_(initialise_blazegraph_fileserver_with_test_triples):
#     sparql_url, fileserver_url = initialise_blazegraph_fileserver_with_test_triples
#     print(sparql_url)
#     print(fileserver_url)
#     time.sleep(600)

# TODO below test needs to be updated
# vapourtec execution -> vapourtec schedule
# property check for the generated triples
# hplc dry run
@pytest.mark.parametrize(
    "goal_set_iri,derivation_inputs,hplc_report_target_folder,fcexp_file_container_folder,local_agent_test",
    [
        (cf.rogi_cf.IRIs.GOALSET_1.value, cf.rogi_cf.IRIs.DERIVATION_INPUTS.value, cf.HPLC_REPORT_LOCAL_TEST_DIR, cf.FCEXP_FILE_DIR, True),
        # (cf.rogi_cf.IRIs.GOALSET_1.value, cf.rogi_cf.IRIs.DERIVATION_INPUTS.value, cf.DOCKER_INTEGRATION_DIR, None, False),
    ],
)
def test_rxn_rogi(
    initialise_blazegraph_fileserver_with_test_triples,
    # initialise_test_triples,
    retrieve_hplc_report, create_rogi_agent,
    create_doe_agent, create_vapourtec_execution_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
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
    vapourtec_execution_agent = create_vapourtec_execution_agent(
        register_agent=True,
    )
    hplc_postpro_agent = create_hplc_postpro_agent(
        register_agent=True,
    )
    vapourtec_agent = create_vapourtec_agent(
        register_agent=True,
        fcexp_file_container_folder=fcexp_file_container_folder
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    hplc_agent = create_hplc_agent(
        register_agent=True,
        hplc_report_container_dir=hplc_report_target_folder
    )

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        print("------------------------------------------------------")
        print(rogi_agent.kgUser)
        print(rogi_agent.kgPassword)
        print(rogi_agent.sparql_client.getAmountOfTriples())
        # rogi_agent.monitor_async_derivations()
        print("------------------------------------------------------")

        rogi_agent._start_monitoring_derivations()
        doe_agent._start_monitoring_derivations()
        vapourtec_execution_agent._start_monitoring_derivations()
        hplc_postpro_agent._start_monitoring_derivations()
        vapourtec_agent._start_monitoring_derivations()
        hplc_agent._start_monitoring_derivations()
        hplc_agent._start_monitoring_local_report_folder()
        vapourtec_agent.sparql_client.update_vapourtec_rs400_state(vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    # Get the goal set instance
    goal_set_instance = rogi_agent.sparql_client.get_goal_set_instance(goal_set_iri)

    # Add timestamp to the derivation_inputs
    for input in derivation_inputs:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)

    # Create derivation instance for new information, the timestamp of this derivation is 0
    derivation_iri = derivation_client.createAsyncDerivationForNewInfo(rogi_agent.agentIRI, derivation_inputs)
    print(f"Initialised successfully, created asynchronous derivation instance: {derivation_iri}")

    time.sleep(120)
    # Generate random file and upload it to KG fileserver
    hplc_agent.get_dict_of_hplc_files() # perform the init check first
    # Generate HPLC report for test in hplc_report_target_folder
    # For docker-integration test, the file will be mounted to docker automatically
    logger.info("======================================================================================================================")
    local_file_path, timestamp_last_modified = retrieve_hplc_report(hplc_agent.hplc_report_file_extension, hplc_report_target_folder)
    logger.info("Dummy HPLC report for test generated with local file path <%s> at %s" % (local_file_path, str(timestamp_last_modified)))
    logger.info("======================================================================================================================")
    try:
        hplc_agent.monitor_local_report_folder() # now the generated report can be uploaded
    except Exception as e:
        time.sleep(1000)

    time.sleep(3600)
    # time.sleep(10)

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    if local_agent_test:
        rogi_agent.scheduler.shutdown()
        doe_agent.scheduler.shutdown()
        vapourtec_execution_agent.scheduler.shutdown()
        hplc_postpro_agent.scheduler.shutdown()
        vapourtec_agent.scheduler.shutdown()
        hplc_agent.scheduler.shutdown()
