import filecmp
import pytest
import time

import vapourtecagent.tests.utils as utils


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,vapourtec_rs400_iri,vapourtec_r4_reactor_iri,fcexp_file_container_folder,derivation_periodic_timescale",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_DUMMY_IRI, utils.cf.FCEXP_FILE_DIR, 6),
        (utils.cf.NEW_RXN_EXP_2_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI, utils.cf.FCEXP_FILE_DIR, 7),
        (utils.cf.NEW_RXN_EXP_3_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_DUMMY_IRI, utils.cf.FCEXP_FILE_DIR, 8),
    ],
)
def test_monitor_derivation(
    initialise_client, create_vapourtec_agent, generate_random_download_path,
    new_rxn_exp_iri, vapourtec_rs400_iri, vapourtec_r4_reactor_iri, fcexp_file_container_folder, derivation_periodic_timescale
):

    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Assign new_rxn_exp_iri to vapourtec_rs400_iri
    # NOTE this should be done by VapourtecExecutionAgent in normal operation
    sparql_client.assign_rxn_exp_to_r4_reactor(new_rxn_exp_iri, vapourtec_r4_reactor_iri)

    # Save a local variable of vapourtec_rs_400
    old_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_rs400_iri])
    assert len(old_rs400_list) == 1
    old_rs400 = old_rs400_list[0]
    old_autosampler = old_rs400.get_autosampler()
    old_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in old_autosampler.hasSite if site.holds.isFilledWith is not None]}

    # Create instance of agent and start monitor derivations
    vapourtec_agent = create_vapourtec_agent(
        vapourtec_digital_twin=vapourtec_rs400_iri,
        fcexp_file_container_folder=fcexp_file_container_folder,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )
    vapourtec_agent._start_monitoring_derivations()

    # Instantiate derivation instance
    derivation_iri = vapourtec_agent.derivation_client.createAsyncDerivationForNewInfo(vapourtec_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    ## Check execution of derivation
    # First, check if the file is generated and uploaded correctly
    vapourtec_input_file_iri = utils.get_vapourtec_input_file_iri(derivation_iri, sparql_client)
    vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri)
    local_file_path = vapourtec_input_file.localFilePath
    remote_file_path = vapourtec_input_file.remoteFilePath
    # Genereate random download path
    full_downloaded_path = generate_random_download_path('csv')
    # Download the file and make sure all the content are the same
    sparql_client.downloadFile(remote_file_path, full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Second, check if settings were generated for all reaction conditions
    rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)[0]
    assert all([condition.translateToParameterSetting is not None for condition in rxn_exp_instance.hasReactionCondition if condition.clz != utils.cf.ONTOREACTION_REACTIONPRESSURE])

    # Third, check there is chemical solution instance
    chemical_solution_iri = utils.get_chemical_solution_iri(derivation_iri, sparql_client)
    assert chemical_solution_iri is not None
    new_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_rs400_iri])
    assert len(new_rs400_list) == 1
    new_rs400 = new_rs400_list[0]
    new_autosampler = new_rs400.get_autosampler()
    new_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in new_autosampler.hasSite if site.holds.isFilledWith is not None]}
    # NOTE below is commented out as the reactor outlet is now send to the waste tank
    # assert chemical_solution_iri in new_autosampler_liquid_level
    # assert new_autosampler_liquid_level[chemical_solution_iri] >= 0

    # Forth, check if the autosampler liquid level is changed
    for chem_sol in old_autosampler_liquid_level:
        # NOTE below is commented out as the reactor outlet is now send to the waste tank
        # if chem_sol == chemical_solution_iri:
        #     assert new_autosampler_liquid_level[chem_sol] >= old_autosampler_liquid_level[chem_sol]
        assert new_autosampler_liquid_level[chem_sol] <= old_autosampler_liquid_level[chem_sol]

    # Shutdown the scheduler to clean up before the next test
    vapourtec_agent.scheduler.shutdown()


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,vapourtec_rs400_iri,vapourtec_r4_reactor_iri,derivation_periodic_timescale,fcexp_file_host_folder",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_DUMMY_IRI, 6, utils.cf.DOCKER_INTEGRATION_DIR),
        (utils.cf.NEW_RXN_EXP_2_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI, 7, utils.cf.DOCKER_INTEGRATION_DIR),
        (utils.cf.NEW_RXN_EXP_3_IRI, utils.cf.VAPOURTECRS400_DUMMY_IRI, utils.cf.VAPOURTECR4REACTOR_DUMMY_IRI, 8, utils.cf.DOCKER_INTEGRATION_DIR),
    ],
)
def test_docker_integration(
    initialise_client, create_vapourtec_agent, generate_random_download_path,
    new_rxn_exp_iri, vapourtec_rs400_iri, vapourtec_r4_reactor_iri, derivation_periodic_timescale, fcexp_file_host_folder
):

    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Assign new_rxn_exp_iri to vapourtec_rs400_iri
    # NOTE this should be done by VapourtecExecutionAgent in normal operation
    sparql_client.assign_rxn_exp_to_r4_reactor(new_rxn_exp_iri, vapourtec_r4_reactor_iri)

    # Save a local variable of vapourtec_rs_400
    old_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_rs400_iri])
    assert len(old_rs400_list) == 1
    old_rs400 = old_rs400_list[0]
    old_autosampler = old_rs400.get_autosampler()
    old_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in old_autosampler.hasSite if site.holds.isFilledWith is not None]}

    # Create instance of agent and start monitor derivations
    vapourtec_agent = create_vapourtec_agent(
        vapourtec_digital_twin=vapourtec_rs400_iri,
        register_agent=True,
        random_agent_iri=False,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )

    # Instantiate derivation instance
    derivation_iri = vapourtec_agent.derivation_client.createAsyncDerivationForNewInfo(vapourtec_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    ## Check execution of derivation
    # First, check if the file is generated and uploaded correctly
    vapourtec_input_file_iri = utils.get_vapourtec_input_file_iri(derivation_iri, sparql_client)
    vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri)
    container_file_path = vapourtec_input_file.localFilePath
    local_file_path = container_file_path.replace(vapourtec_agent.fcexp_file_container_folder, fcexp_file_host_folder)
    remote_file_path = utils.cf.host_docker_internal_to_localhost(vapourtec_input_file.remoteFilePath)
    # Genereate random download path
    full_downloaded_path = generate_random_download_path('csv')
    # Download the file and make sure all the content are the same
    sparql_client.downloadFile(remote_file_path, full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Second, check if settings were generated for all reaction conditions
    rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)[0]
    assert all([condition.translateToParameterSetting is not None for condition in rxn_exp_instance.hasReactionCondition if condition.clz != utils.cf.ONTOREACTION_REACTIONPRESSURE])

    # Third, check there is chemical solution instance
    chemical_solution_iri = utils.get_chemical_solution_iri(derivation_iri, sparql_client)
    assert chemical_solution_iri is not None
    new_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_rs400_iri])
    assert len(new_rs400_list) == 1
    new_rs400 = new_rs400_list[0]
    new_autosampler = new_rs400.get_autosampler()
    new_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in new_autosampler.hasSite if site.holds.isFilledWith is not None]}
    # NOTE below is commented out as the reactor outlet is now send to the waste tank
    # assert chemical_solution_iri in new_autosampler_liquid_level
    # assert new_autosampler_liquid_level[chemical_solution_iri] >= 0

    # Forth, check if the autosampler liquid level is changed
    for chem_sol in old_autosampler_liquid_level:
        # NOTE below is commented out as the reactor outlet is now send to the waste tank
        # if chem_sol == chemical_solution_iri:
        #     assert new_autosampler_liquid_level[chem_sol] >= old_autosampler_liquid_level[chem_sol]
        assert new_autosampler_liquid_level[chem_sol] <= old_autosampler_liquid_level[chem_sol]

    # NOTE additional check in docker integration
    # Fifth, check if the stateLastUpdatedAt of vapourtec state is changed
    assert new_rs400.hasState.stateLastUpdatedAt > old_rs400.hasState.stateLastUpdatedAt
