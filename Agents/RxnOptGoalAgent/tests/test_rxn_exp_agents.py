import pytest
import time

import tests.conftest as cf

import logging
logger = logging.getLogger('test_rxn_integration')


# ----------------------------------------------------------------------------------
# Test cases for the integration of the agents (DoE/Exe/Postproc/HPLC/Vapourtec)
# NOTE this test case is for local agents, i.e. instances in memory, not dockerised
# ----------------------------------------------------------------------------------

@pytest.mark.parametrize(
    "vt_env_file,hplc_env_file,derivation_inputs,hplc_report_target_folder,fcexp_file_container_folder",
    [
        # (cf.LAB1_VAPOURTEC_AGENT_ENV,cf.LAB1_HPLC_AGENT_ENV,[cf.DOE_IRI],cf.HPLC_REPORT_LOCAL_TEST_DIR, cf.FCEXP_FILE_DIR),
        (cf.LAB2_VAPOURTEC_AGENT_ENV,cf.LAB2_HPLC_AGENT_ENV,[cf.DOE_IRI],cf.HPLC_REPORT_LOCAL_TEST_DIR, cf.FCEXP_FILE_DIR),
    ],
)
def test_exp_agents(
    initialise_blazegraph_fileserver_with_test_triples,
    retrieve_hplc_report,
    create_doe_agent, create_vapourtec_execution_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    vt_env_file, hplc_env_file,
    derivation_inputs, hplc_report_target_folder, fcexp_file_container_folder
):
    sparql_client, derivation_client = initialise_blazegraph_fileserver_with_test_triples

    # Assert that there's currently no new experiment associated with the DoE instance
    assert sparql_client.getNewExperimentFromDoE(cf.DOE_IRI) is None

    # Add timestamp to the derivation_inputs
    for input in derivation_inputs:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    doe_agent = create_doe_agent(register_agent=True, random_agent_iri=True)
    vapourtec_execution_agent = create_vapourtec_execution_agent(register_agent=True, random_agent_iri=True)
    hplc_postpro_agent = create_hplc_postpro_agent(register_agent=True, random_agent_iri=True)
    vapourtec_agent = create_vapourtec_agent(
        env_file=vt_env_file,
        register_agent=True,
        random_agent_iri=True,
        fcexp_file_container_folder=fcexp_file_container_folder
    )
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) as it's for local_agent_test
    hplc_agent = create_hplc_agent(
        env_file=hplc_env_file,
        register_agent=True,
        random_agent_iri=True,
        hplc_report_container_dir=hplc_report_target_folder
    )

    # Start the scheduler to monitor derivations as it's local agent test
    doe_agent._start_monitoring_derivations()
    vapourtec_execution_agent._start_monitoring_derivations()
    hplc_postpro_agent._start_monitoring_derivations()
    vapourtec_agent._start_monitoring_derivations()
    hplc_agent._start_monitoring_derivations()
    hplc_agent._start_monitoring_local_report_folder()
    sparql_client.update_vapourtec_rs400_state(vapourtec_agent.vapourtec_digital_twin, cf.ONTOVAPOURTEC_IDLE, time.time())

    # Create derivation instance for new information, the timestamp of this derivation is 0
    doe_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(doe_agent.agentIRI, derivation_inputs)
    logger.info(f"Initialised successfully, created asynchronous doe derivation instance: {doe_derivation_iri}")
    exe_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(vapourtec_execution_agent.agentIRI, [doe_derivation_iri])
    logger.info(f"Initialised successfully, created asynchronous exe derivation instance: {exe_derivation_iri}")
    postproc_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(hplc_postpro_agent.agentIRI, [exe_derivation_iri])
    logger.info(f"Initialised successfully, created asynchronous postproc derivation instance: {postproc_derivation_iri}")

    time.sleep(60)
    # Generate random file and upload it to KG fileserver
    hplc_agent.get_dict_of_hplc_files() # perform the init check first
    # Generate HPLC report for test in hplc_report_target_folder
    # For docker-integration test, the file will be mounted to docker automatically
    logger.info("======================================================================================================================")
    local_file_path, timestamp_last_modified = retrieve_hplc_report(hplc_agent.hplc_report_file_extension, hplc_report_target_folder)
    logger.info("Dummy HPLC report for test generated with local file path <%s> at %s" % (local_file_path, str(timestamp_last_modified)))
    logger.info("======================================================================================================================")
    hplc_agent.monitor_local_report_folder() # now the generated report can be uploaded

    time.sleep(3600)
    # Check if the whole workflow is executed successfully

    # Shutdown the scheduler to clean up as it's local agent test (as the doe_agent scheduler must have started)
    doe_agent.scheduler.shutdown()
    vapourtec_execution_agent.scheduler.shutdown()
    hplc_postpro_agent.scheduler.shutdown()
    vapourtec_agent.scheduler.shutdown()
    hplc_agent.scheduler.shutdown()
