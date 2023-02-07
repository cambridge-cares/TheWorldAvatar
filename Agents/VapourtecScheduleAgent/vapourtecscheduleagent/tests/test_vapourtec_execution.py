from rdflib import Graph
from rdflib import URIRef
from rdflib import RDF
import filecmp
import pytest
import uuid
import time

import vapourtecscheduleagent.tests.utils as utils


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
    initialise_client, create_vapourtec_schedule_agent, generate_random_download_path,
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
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )
    vapourtec_schedule_agent._start_monitoring_derivations()

    # NOTE Add placeholder agent service iri to manage the digital twin of all hardware
    # NOTE This should actually be done by the VapourtecAgent/HPLCAgent themselves when they are deployed
    sparql_client.performUpdate("""INSERT {?hplc <%s> <%s>. ?vapourtec <%s> <%s>.} WHERE {?hplc a <%s>. ?vapourtec a <%s>.}""" % (
        utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, utils.cf.ONTOVAPOURTEC_VAPOURTECRS400
    ))

    # Instantiate derivation instance
    derivation_iri = vapourtec_schedule_agent.derivation_client.createAsyncDerivationForNewInfo(vapourtec_schedule_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)

    # Insert placeholder triples to let the VapourtecScheduleAgent finish job
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
    vapourtec_schedule_agent.scheduler.shutdown()


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
    initialise_client, create_vapourtec_schedule_agent, generate_random_download_path,
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
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
        maximum_concurrent_experiment=1,#maximum_concurrent_experiment,
        register_agent=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
    )

    # NOTE Add placeholder agent service iri to manage the digital twin of all hardware
    # NOTE This should actually be done by the VapourtecAgent/HPLCAgent themselves when they are deployed
    sparql_client.performUpdate("""INSERT {?hplc <%s> <%s>. ?vapourtec <%s> <%s>.} WHERE {?hplc a <%s>. ?vapourtec a <%s>.}""" % (
        utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOLAB_ISMANAGEDBY, str(uuid.uuid4()), utils.cf.ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY, utils.cf.ONTOVAPOURTEC_VAPOURTECRS400
    ))

    # Instantiate derivation instance
    derivation_iri = vapourtec_schedule_agent.derivation_client.createAsyncDerivationForNewInfo(vapourtec_schedule_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)

    # Insert placeholder triples to let the VapourtecScheduleAgent finish job
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
    initialise_client, create_vapourtec_schedule_agent, generate_random_download_path, create_test_report,
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
    vapourtec_schedule_agent = create_vapourtec_schedule_agent(
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
    old_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_agent.vapourtec_digital_twin])
    assert len(old_rs400_list) == 1
    old_rs400 = old_rs400_list[0]
    old_autosampler = old_rs400.get_autosampler()
    old_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in old_autosampler.hasSite if site.holds.isFilledWith is not None]}

    # Instantiate derivation instance
    vtexe_derivation_iri = vapourtec_schedule_agent.derivation_client.createAsyncDerivationForNewInfo(vapourtec_schedule_agent.agentIRI, [new_rxn_exp_iri])

    # Wait until derivations for vapourtec and hplc are instantiated
    vapourtec_derivation = None
    while not vapourtec_derivation:
        time.sleep(10)
        vapourtec_derivation = utils.get_vapourtec_derivation(new_rxn_exp_iri, sparql_client)
        print(f"Waiting for vapourtec derivation to be instantiated, current time: {time.time()}")

    hplc_derivation = None
    while not hplc_derivation:
        time.sleep(10)
        hplc_derivation = utils.get_hplc_derivation(new_rxn_exp_iri, sparql_client)
        print(f"Waiting for hplc derivation to be instantiated, current time: {time.time()}")

    ################################
    ## Check Vapourtec Derivation ##
    ################################
    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_vapourtec_derivation = 0
    while currentTimestamp_vapourtec_derivation == 0:
        time.sleep(20)
        currentTimestamp_vapourtec_derivation = utils.get_timestamp(vapourtec_derivation, sparql_client)

    # First, check if the file is generated and uploaded correctly
    vapourtec_input_file_iri = utils.get_vapourtec_input_file_iri(vapourtec_derivation, sparql_client)
    vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri)
    container_file_path = vapourtec_input_file.localFilePath
    local_file_path = container_file_path.replace(vapourtec_agent.fcexp_file_container_folder, fcexp_file_host_folder)
    remote_file_path = vapourtec_input_file.remoteFilePath
    # Genereate random download path
    full_downloaded_path = generate_random_download_path('csv')
    # Download the file and make sure all the content are the same
    sparql_client.downloadFile(utils.cf.host_docker_internal_to_localhost(remote_file_path), full_downloaded_path)
    assert filecmp.cmp(local_file_path,full_downloaded_path)

    # Second, check if settings were generated for all reaction conditions
    rxn_exp_instance = sparql_client.getReactionExperiment(new_rxn_exp_iri)[0]
    assert all([condition.translateToParameterSetting is not None for condition in rxn_exp_instance.hasReactionCondition if condition.clz != utils.cf.ONTOREACTION_REACTIONPRESSURE])

    # Third, check there is chemical amount instance
    chemical_amount_iri = utils.get_chemical_amount_iri(vapourtec_derivation, sparql_client)
    assert chemical_amount_iri is not None
    new_rs400_list = sparql_client.get_vapourtec_rs400(list_vapourtec_rs400_iri=[vapourtec_agent.vapourtec_digital_twin])
    assert len(new_rs400_list) == 1
    new_rs400 = new_rs400_list[0]
    new_autosampler = new_rs400.get_autosampler()
    new_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in new_autosampler.hasSite if site.holds.isFilledWith is not None]}
    # NOTE below is commented out as the reactor outlet is now send to the waste tank
    # assert chemical_amount_iri in new_autosampler_liquid_level
    # assert new_autosampler_liquid_level[chemical_amount_iri] >= 0

    # Forth, check if the autosampler liquid level is changed
    for chem_amount in old_autosampler_liquid_level:
        # NOTE below is commented out as the reactor outlet is now send to the waste tank
        # if chem_amount == chemical_amount_iri:
        #     assert new_autosampler_liquid_level[chem_amount] >= old_autosampler_liquid_level[chem_amount]
        assert new_autosampler_liquid_level[chem_amount] <= old_autosampler_liquid_level[chem_amount]

    # NOTE additional check in docker integration
    # Fifth, check if the stateLastUpdatedAt of vapourtec state is changed
    assert new_rs400.hasState.stateLastUpdatedAt > old_rs400.hasState.stateLastUpdatedAt

    ##############################
    ## Check HPLC Derivation ##
    ##############################
    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(hplc_derivation, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_agent.hplc_digital_twin, new_rxn_exp_iri, chemical_amount_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(hplc_derivation, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    #######################################################
    ## Check execution of Vapourtec Schedule Derivation ##
    #######################################################
    # Wait until derivation update is finished
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(10)
        currentTimestamp_derivation = utils.get_timestamp(vtexe_derivation_iri, sparql_client)

    # Check execution of Vapourtec Schedule Derivation
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
