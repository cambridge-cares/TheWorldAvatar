import pytest
import time

import tests.utils as utils

import logging
logger = logging.getLogger('test_rxn_integration')

@pytest.mark.parametrize(
    "hplc_report_target_folder,fcexp_file_container_folder,local_agent_test",
    [
        # (utils.cf.HPLC_REPORT_LOCAL_TEST_DIR, utils.cf.FCEXP_FILE_DIR, True),
        (utils.cf.DOCKER_INTEGRATION_DIR, None, False),
    ],
)
def test_rxn_integration(
    initialise_clients, retrieve_hplc_report,
    create_doe_agent, create_vapourtec_execution_agent, create_hplc_postpro_agent, create_vapourtec_agent, create_hplc_agent,
    hplc_report_target_folder, fcexp_file_container_folder, local_agent_test
):
    sparql_client, derivation_client = initialise_clients

    # Verify that knowledge base is empty
    res = sparql_client.getAmountOfTriples()
    assert res == 0

    # Initialise all triples in the knowledge graph
    utils.initialise_triples(sparql_client, derivation_client)

    # Assert that there's currently no new experiment associated with the DoE instance
    assert sparql_client.getNewExperimentFromDoE(utils.cf.DOE_IRI) is None

    # Create agent instances, this also register the agents to the KG
    # NOTE that this should be done by agent themselves at real deployment
    doe_agent = create_doe_agent(register_agent=True, random_agent_iri=local_agent_test)
    vapourtec_execution_agent = create_vapourtec_execution_agent(register_agent=True, random_agent_iri=local_agent_test)
    hplc_postpro_agent = create_hplc_postpro_agent(register_agent=True, random_agent_iri=local_agent_test)
    vapourtec_agent = create_vapourtec_agent(register_agent=True, random_agent_iri=local_agent_test, fcexp_file_container_folder=fcexp_file_container_folder)
    # Set the hplc_report_container_dir to be hplc_report_target_folder (on host machine) if it's for local_agent_test
    if local_agent_test:
        hplc_agent = create_hplc_agent(register_agent=True, random_agent_iri=local_agent_test, hplc_report_container_dir=hplc_report_target_folder)
    else:
        hplc_agent = create_hplc_agent(register_agent=True, random_agent_iri=local_agent_test)

    # Start the scheduler to monitor derivations if it's local agent test
    if local_agent_test:
        doe_agent._start_monitoring_derivations()
        vapourtec_execution_agent._start_monitoring_derivations()
        hplc_postpro_agent._start_monitoring_derivations()
        vapourtec_agent._start_monitoring_derivations()
        hplc_agent._start_monitoring_derivations()
        hplc_agent._start_monitoring_local_report_folder()
        sparql_client.update_vapourtec_rs400_state(vapourtec_agent.vapourtec_digital_twin, utils.cf.ONTOVAPOURTEC_IDLE, time.time())

    # Create derivation instance for new information, the timestamp of this derivation is 0
    doe_derivation_iri = derivation_client.createAsyncDerivationForNewInfo(doe_agent.agentIRI, utils.cf.DERIVATION_INPUTS)
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

    # Shutdown the scheduler to clean up if it's local agent test (as the doe_agent scheduler must have started)
    if local_agent_test:
        doe_agent.scheduler.shutdown()
        vapourtec_execution_agent.scheduler.shutdown()
        hplc_postpro_agent.scheduler.shutdown()
        vapourtec_agent.scheduler.shutdown()
        hplc_agent.scheduler.shutdown()
