from rdflib import Graph
from rdflib import URIRef
from rdflib import RDF
import filecmp
import pytest
import uuid
import time

import vtexeagent.tests.utils as utils


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,derivation_periodic_timescale",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, 6),
        (utils.cf.NEW_RXN_EXP_2_IRI, 7),
        (utils.cf.NEW_RXN_EXP_3_IRI, 8),
    ],
)
def test_monitor_derivation(
    initialise_client, create_vapourtec_execution_agent, generate_random_download_path,
    new_rxn_exp_iri, derivation_periodic_timescale
):
    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Query reaction experiment, check it's not assigned to any reactor
    lst_unassigned_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_unassigned_rxn_exp_instance) == 1
    assert lst_unassigned_rxn_exp_instance[0].isAssignedTo is None

    # Create instance of agent and start monitor derivations
    vapourtec_execution_agent = create_vapourtec_execution_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )
    vapourtec_execution_agent._start_monitoring_derivations()

    # NOTE Add placeholder agent service iri to manage the digital twin of all hardware
    # NOTE This should actually be done by the VapourtecAgent/HPLCAgent themselves when they are deployed
    sparql_client.performUpdate("""INSERT {?hplc <%s> <%s>. ?vapourtec <%s> <%s>.} WHERE {?hplc a <%s>. ?vapourtec a <%s>.}""" % (
        utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, utils.cf.ONTOVAPOURTEC_VAPOURTECRS400
    ))

    # Add timestamp to new_rxn_exp_iri (pure inputs)
    vapourtec_execution_agent.derivationClient.addTimeInstance(new_rxn_exp_iri)
    vapourtec_execution_agent.derivationClient.updateTimestamp(new_rxn_exp_iri)

    # Instantiate derivation instance
    derivation_iri = vapourtec_execution_agent.derivationClient.createAsyncDerivationForNewInfo(vapourtec_execution_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)

    # Insert placeholder triples to let the VapourtecExecutionAgent finish job
    g = Graph()
    placeholder_hplcjob = "http://placeholder/" + str(uuid.uuid4())
    placeholder_hplcreport = "http://placeholder/" + str(uuid.uuid4())
    g.add((URIRef(placeholder_hplcjob), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCJOB)))
    g.add((URIRef(placeholder_hplcreport), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCREPORT)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTODERIVATION_BELONGSTO), URIRef(hplc_derivation)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTOHPLC_HASREPORT), URIRef(placeholder_hplcreport)))
    sparql_client.uploadGraph(g)

    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    # Check execution of derivation
    # (1) reaction experiment should be assigned to a reactor
    lst_done_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_done_rxn_exp_instance) == 1
    assert lst_done_rxn_exp_instance[0].isAssignedTo is not None
    # (2) HPLCReport instance should added to the derivation outputs
    lst_derivation_outputs = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_derivation_outputs) == 1
    assert placeholder_hplcreport == lst_derivation_outputs[0]

    # Shutdown the scheduler to clean up before the next test
    vapourtec_execution_agent.scheduler.shutdown()


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,derivation_periodic_timescale",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, 6),
        (utils.cf.NEW_RXN_EXP_2_IRI, 7),
        (utils.cf.NEW_RXN_EXP_3_IRI, 8),
    ],
)
def test_docker_integration(
    initialise_client, create_vapourtec_execution_agent, generate_random_download_path,
    new_rxn_exp_iri, derivation_periodic_timescale
):
    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Query reaction experiment, check it's not assigned to any reactor
    lst_unassigned_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_unassigned_rxn_exp_instance) == 1
    assert lst_unassigned_rxn_exp_instance[0].isAssignedTo is None

    # Create instance of agent and start monitor derivations
    vapourtec_execution_agent = create_vapourtec_execution_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )

    # NOTE Add placeholder agent service iri to manage the digital twin of all hardware
    # NOTE This should actually be done by the VapourtecAgent/HPLCAgent themselves when they are deployed
    sparql_client.performUpdate("""INSERT {?hplc <%s> <%s>. ?vapourtec <%s> <%s>.} WHERE {?hplc a <%s>. ?vapourtec a <%s>.}""" % (
        utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, utils.cf.ONTOVAPOURTEC_VAPOURTECRS400
    ))

    # Add timestamp to new_rxn_exp_iri (pure inputs)
    vapourtec_execution_agent.derivationClient.addTimeInstance(new_rxn_exp_iri)
    vapourtec_execution_agent.derivationClient.updateTimestamp(new_rxn_exp_iri)

    # Instantiate derivation instance
    derivation_iri = vapourtec_execution_agent.derivationClient.createAsyncDerivationForNewInfo(vapourtec_execution_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)

    # Insert placeholder triples to let the VapourtecExecutionAgent finish job
    g = Graph()
    placeholder_hplcjob = "http://placeholder/" + str(uuid.uuid4())
    placeholder_hplcreport = "http://placeholder/" + str(uuid.uuid4())
    g.add((URIRef(placeholder_hplcjob), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCJOB)))
    g.add((URIRef(placeholder_hplcreport), RDF.type, URIRef(utils.cf.ONTOHPLC_HPLCREPORT)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTODERIVATION_BELONGSTO), URIRef(hplc_derivation)))
    g.add((URIRef(placeholder_hplcjob), URIRef(utils.cf.ONTOHPLC_HASREPORT), URIRef(placeholder_hplcreport)))
    sparql_client.uploadGraph(g)

    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)

    # Check execution of derivation
    # (1) reaction experiment should be assigned to a reactor
    lst_done_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_done_rxn_exp_instance) == 1
    assert lst_done_rxn_exp_instance[0].isAssignedTo is not None
    # (2) HPLCReport instance should added to the derivation outputs
    lst_derivation_outputs = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_derivation_outputs) == 1
    assert placeholder_hplcreport == lst_derivation_outputs[0]


# NOTE the derivation_periodic_timescale (6, 7, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "new_rxn_exp_iri,derivation_periodic_timescale,fcexp_file_host_folder,hplc_report_wsl_folder",
    [
        (utils.cf.NEW_RXN_EXP_1_IRI, 6, utils.cf.DOCKER_INTEGRATION_VAPOURTEC_DIR, utils.cf.DOCKER_INTEGRATION_HPLC_DIR),
        (utils.cf.NEW_RXN_EXP_2_IRI, 7, utils.cf.DOCKER_INTEGRATION_VAPOURTEC_DIR, utils.cf.DOCKER_INTEGRATION_HPLC_DIR),
        (utils.cf.NEW_RXN_EXP_3_IRI, 8, utils.cf.DOCKER_INTEGRATION_VAPOURTEC_DIR, utils.cf.DOCKER_INTEGRATION_HPLC_DIR),
    ],
)
def test_three_agents_docker_integration(
    initialise_client, create_vapourtec_execution_agent, generate_random_download_path, create_test_report,
    create_vapourtec_agent, create_hplc_agent,
    new_rxn_exp_iri, derivation_periodic_timescale, fcexp_file_host_folder, hplc_report_wsl_folder
):
    # Initialise triples
    sparql_client = initialise_client
    utils.initialise_triples(generate_random_download_path, sparql_client)

    # Query reaction experiment, check it's not assigned to any reactor
    lst_unassigned_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_unassigned_rxn_exp_instance) == 1
    assert lst_unassigned_rxn_exp_instance[0].isAssignedTo is None

    # Create instance of three agents
    vapourtec_execution_agent = create_vapourtec_execution_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )
    vapourtec_agent = create_vapourtec_agent(
        register_agent=True
    )
    hplc_agent = create_hplc_agent(
        register_agent=True
    )

    # Check if the vapourtec agent is registered with the vapourtec hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        vapourtec_agent.vapourtec_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, vapourtec_agent.agentIRI
    ))[0]['ASK']

    # Check if the hplc agent is registered with the hplc hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        hplc_agent.hplc_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
    ))[0]['ASK']

    # Save a local variable of vapourtec_rs_400
    old_rs400 = sparql_client.get_vapourtec_rs400(vapourtec_agent.vapourtec_digital_twin)
    old_autosampler = sparql_client.get_autosampler_from_vapourtec_rs400(old_rs400)
    old_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in old_autosampler.hasSite if site.holds.isFilledWith is not None]}

    # Add timestamp to new_rxn_exp_iri (pure inputs)
    vapourtec_execution_agent.derivationClient.addTimeInstance(new_rxn_exp_iri)
    vapourtec_execution_agent.derivationClient.updateTimestamp(new_rxn_exp_iri)

    # Instantiate derivation instance
    vtexe_derivation_iri = vapourtec_execution_agent.derivationClient.createAsyncDerivationForNewInfo(vapourtec_execution_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    vapourtec_derivation = None
    while not vapourtec_derivation:
        time.sleep(10)
        vapourtec_derivation = utils.get_vapourtec_derivation(new_rxn_exp_iri, sparql_client)

    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)

    # Wait until derivations for vapourtec and hplc are instantiated
    hplc_derivation_is_in_progress = False
    while not hplc_derivation_is_in_progress:
        time.sleep(20)
        hplc_derivation_is_in_progress = utils.if_hplc_derivation_is_in_progress(hplc_derivation, sparql_client)

    ################################
    ## Check Vapourtec Derivation ##
    ################################
    # First, check if the file is generated and uploaded correctly
    vapourtec_input_file_iri = utils.get_vapourtec_input_file_iri(vapourtec_derivation, sparql_client)
    vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri)
    container_file_path = vapourtec_input_file.localFilePath
    local_file_path = container_file_path.replace(vapourtec_agent.fcexp_file_container_folder, fcexp_file_host_folder)
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
    chemical_solution_iri = utils.get_chemical_solution_iri(vapourtec_derivation, sparql_client)
    assert chemical_solution_iri is not None
    new_rs400 = sparql_client.get_vapourtec_rs400(vapourtec_agent.vapourtec_digital_twin)
    new_autosampler = sparql_client.get_autosampler_from_vapourtec_rs400(new_rs400)
    new_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in new_autosampler.hasSite if site.holds.isFilledWith is not None]}
    assert chemical_solution_iri in new_autosampler_liquid_level
    assert new_autosampler_liquid_level[chemical_solution_iri] >= 0

    # Forth, check if the autosampler liquid level is changed
    for chem_sol in old_autosampler_liquid_level:
        if chem_sol == chemical_solution_iri:
            assert new_autosampler_liquid_level[chem_sol] >= old_autosampler_liquid_level[chem_sol]
        assert new_autosampler_liquid_level[chem_sol] <= old_autosampler_liquid_level[chem_sol]

    # NOTE additional check in docker integration
    # Fifth, check if the stateLastUpdatedAt of vapourtec state is changed
    assert new_rs400.hasState.stateLastUpdatedAt > old_rs400.hasState.stateLastUpdatedAt

    ##############################
    ## Check HPLC Derivation ##
    ##############################
    # Generate random file to the docker integration folder, the agent deployed in docker will pick it up
    generated_file_path = create_test_report(hplc_agent.hplc_report_file_extension, True)
    # NOTE here we need to replace the first absolute path bit with the mounted folder in docker
    local_file_path_in_docker = generated_file_path.replace(hplc_report_wsl_folder+'/', hplc_agent.hplc_report_container_dir)

    ## Check if the content of the uploaded file matches the local file
    # Wait for a bit to let the dockerised agent upload the file
    time.sleep(hplc_agent.hplc_report_periodic_timescale * 2)
    # Query remote file path
    # time.sleep(600)
    remote_file_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_agent.hplc_digital_twin, local_file_path_in_docker)
    # Genereate random download path
    full_downloaded_path = generate_random_download_path(hplc_agent.hplc_report_file_extension)
    # Download the file and make sure all the content are the same
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(generated_file_path,full_downloaded_path)

    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(hplc_derivation, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_agent.hplc_digital_twin, new_rxn_exp_iri, chemical_solution_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(hplc_derivation, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    #######################################################
    ## Check execution of Vapourtec Execution Derivation ##
    #######################################################
    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(vtexe_derivation_iri, sparql_client)

    # Check execution of Vapourtec Execution Derivation
    # (1) reaction experiment should be assigned to a reactor
    lst_done_rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)
    assert len(lst_done_rxn_exp_instance) == 1
    assert lst_done_rxn_exp_instance[0].isAssignedTo is not None
    # (2) HPLCReport instance should added to the derivation outputs
    lst_hplc_report_iri = utils.get_hplc_report_of_hplc_job(lst_hplc_job_iri[0], sparql_client)
    lst_derivation_outputs = utils.get_derivation_outputs(vtexe_derivation_iri, sparql_client)
    assert len(lst_hplc_report_iri) == 1
    assert len(lst_derivation_outputs) == 1
    assert lst_hplc_report_iri == lst_derivation_outputs
