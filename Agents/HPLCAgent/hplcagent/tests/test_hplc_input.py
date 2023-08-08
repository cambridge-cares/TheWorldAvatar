import pytest
import filecmp
import time
import os

from py4jps import agentlogging
logger = agentlogging.get_logger('dev')

import hplcagent.tests.utils as utils

pytest_plugins = ["docker_compose"]


# NOTE the hplc_report_periodic_timescale (8, 11) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "hplc_report_periodic_timescale,hplc_report_container_dir,hplc_report_file_extension",
    [
        (8, utils.cf.HPLC_REPORT_DIR, utils.cf.XLSFILE_EXTENSION),
        (11, utils.cf.HPLC_REPORT_DIR, utils.cf.TXTFILE_EXTENSION),
    ],
)
def test_monitor_local_report_folder(
    initialise_client, initialise_hplc_digital_twin_triples, create_hplc_agent,
    create_test_report, generate_random_download_path,
    hplc_report_periodic_timescale, hplc_report_container_dir, hplc_report_file_extension
):

    sparql_client = initialise_client
    hplc_digital_twin = initialise_hplc_digital_twin_triples(sparql_client, hplc_report_file_extension)
    hplc_agent = create_hplc_agent(
        hplc_digital_twin=hplc_digital_twin,
        hplc_report_periodic_timescale=hplc_report_periodic_timescale,
        hplc_report_container_dir=hplc_report_container_dir,
        hplc_report_file_extension=hplc_report_file_extension,
        register_agent=True,
    )

    # Check if the agent is registered with the hplc hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        hplc_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
    ))[0]['ASK']

    hplc_agent._start_monitoring_local_report_folder()

    # Create a random file to be uploaded
    time.sleep(hplc_report_periodic_timescale * 2)
    generated_file_path = create_test_report(hplc_report_file_extension)

    # Wait for a bit to let the agent upload the file
    time.sleep(hplc_report_periodic_timescale * 2)

    # Query remote file path
    remote_file_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, generated_file_path)

    # Genereate random download path
    full_downloaded_path = generate_random_download_path(hplc_report_file_extension)

    # Download the file and make sure all the content are the same
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(generated_file_path,full_downloaded_path)

    # Shutdown the scheduler to clean up before the next test
    hplc_agent.scheduler.shutdown()


# NOTE the hplc_report_periodic_timescale (8, 11) are chosen randomly for the test cases
# NOTE the derivation_periodic_timescale (5, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "hplc_report_periodic_timescale,hplc_report_container_dir,hplc_report_file_extension,derivation_periodic_timescale",
    [
        (8, utils.cf.HPLC_REPORT_DIR, utils.cf.XLSFILE_EXTENSION, 5),
        (11, utils.cf.HPLC_REPORT_DIR, utils.cf.TXTFILE_EXTENSION, 8),
    ],
)
def test_monitor_derivation(
    initialise_client, initialise_hplc_digital_twin_triples, create_hplc_agent, initialise_hplc_derivation_input_triples,
    create_test_report, generate_random_download_path,
    hplc_report_periodic_timescale, hplc_report_container_dir, hplc_report_file_extension, derivation_periodic_timescale
):

    sparql_client = initialise_client
    hplc_digital_twin = initialise_hplc_digital_twin_triples(sparql_client, hplc_report_file_extension)
    hplc_agent = create_hplc_agent(
        hplc_digital_twin=hplc_digital_twin,
        hplc_report_periodic_timescale=hplc_report_periodic_timescale,
        hplc_report_container_dir=hplc_report_container_dir,
        hplc_report_file_extension=hplc_report_file_extension,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
        dry_run=False,
    )

    # Check if the agent is registered with the hplc hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        hplc_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
    ))[0]['ASK']

    ## Instantiate derivation instance
    rxn_exp_iri, chemical_amount_iri = initialise_hplc_derivation_input_triples(sparql_client)
    derivation_inputs = [rxn_exp_iri, chemical_amount_iri]
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = hplc_agent.derivation_client.createAsyncDerivationForNewInfo(hplc_agent.agentIRI, derivation_inputs)
    logger.info(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    # Start monitor derivations
    hplc_agent._start_monitoring_derivations()

    # Wait for some arbitrary time more then the derivation_periodic_timescale
    # NOTE that this time is set to be "long enough" to make sure the derivation is InProgress
    time.sleep(2*derivation_periodic_timescale + 1)

    # Generate random file and upload it to KG fileserver
    hplc_agent.get_dict_of_hplc_files() # perform the init check first
    generated_file_path = create_test_report(hplc_report_file_extension) # generate report
    hplc_agent.monitor_local_report_folder() # now the generated report can be uploaded

    ## Check if the content of the uploaded file matches the local file
    # Query remote file path
    remote_file_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, generated_file_path)
    # Genereate random download path
    full_downloaded_path = generate_random_download_path(hplc_report_file_extension)
    # Download the file and make sure all the content are the same
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(generated_file_path,full_downloaded_path)

    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_digital_twin, rxn_exp_iri, chemical_amount_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    # Shutdown the scheduler to clean up before the next test
    hplc_agent.scheduler.shutdown()

@pytest.mark.parametrize(
    "hplc_report_periodic_timescale,hplc_report_container_dir,hplc_report_file_extension,derivation_periodic_timescale,dry_run",
    [
        (5, utils.cf.HPLC_REPORT_DIR, utils.cf.XLSFILE_EXTENSION, 5, True),
    ],
)
def test_dry_run(
    initialise_client, initialise_hplc_digital_twin_triples, create_hplc_agent, initialise_hplc_derivation_input_triples,
    create_test_report, generate_random_download_path,
    hplc_report_periodic_timescale, hplc_report_container_dir, hplc_report_file_extension, derivation_periodic_timescale,
    dry_run
):

    sparql_client = initialise_client
    hplc_digital_twin = initialise_hplc_digital_twin_triples(sparql_client, hplc_report_file_extension)
    hplc_agent = create_hplc_agent(
        hplc_digital_twin=hplc_digital_twin,
        hplc_report_periodic_timescale=hplc_report_periodic_timescale,
        hplc_report_container_dir=hplc_report_container_dir,
        hplc_report_file_extension=hplc_report_file_extension,
        register_agent=True,
        random_agent_iri=True,
        derivation_periodic_timescale=derivation_periodic_timescale,
        dry_run=dry_run,
    )

    # Check if the agent is registered with the hplc hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        hplc_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
    ))[0]['ASK']

    # Start monitor derivations
    hplc_agent.start_all_periodical_job()

    ## Instantiate derivation instance
    rxn_exp_iri, chemical_amount_iri = initialise_hplc_derivation_input_triples(sparql_client)
    derivation_inputs = [rxn_exp_iri, chemical_amount_iri]
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = hplc_agent.derivation_client.createAsyncDerivationForNewInfo(hplc_agent.agentIRI, derivation_inputs)
    logger.info(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_digital_twin, rxn_exp_iri, chemical_amount_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    # Shutdown the scheduler to clean up before the next test
    hplc_agent.scheduler.shutdown()


def test_docker_integration(
    initialise_client, initialise_hplc_digital_twin_triples, create_hplc_agent, initialise_hplc_derivation_input_triples,
    create_test_report, generate_random_download_path,
):

    sparql_client = initialise_client
    hplc_agent = create_hplc_agent(register_agent=True)
    hplc_digital_twin = initialise_hplc_digital_twin_triples(sparql_client, hplc_agent.hplc_report_file_extension, hplc_agent.hplc_digital_twin)

    # Check if the agent is registered with the hplc hardware
    assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
        hplc_digital_twin, utils.cf.ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
    ))[0]['ASK']

    ## Instantiate derivation instance
    rxn_exp_iri, chemical_amount_iri = initialise_hplc_derivation_input_triples(sparql_client)
    derivation_inputs = [rxn_exp_iri, chemical_amount_iri]
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = hplc_agent.derivation_client.createAsyncDerivationForNewInfo(hplc_agent.agentIRI, derivation_inputs)
    logger.info(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    # Wait for some arbitrary time more then the derivation_periodic_timescale
    time.sleep(hplc_agent.time_interval + 1)

    ###################################################################################################
    ## NOTE below block of codes is commented out as the agent.hplc.env.test::DRY_RUN is set to True ##
    ## Therefore the agent should generate and upload the report file to the KG fileserver by itself ##
    ###################################################################################################
    # # Generate random file to the docker integration folder, the agent deployed in docker will pick it up
    # generated_file_path = create_test_report(hplc_agent.hplc_report_file_extension, True)
    # # NOTE here we need to replace the first absolute path bit with the mounted folder in docker
    # local_file_path_in_docker = generated_file_path.replace(utils.cf.DOCKER_INTEGRATION_DIR+'/', hplc_agent.hplc_report_container_dir)

    # ## Check if the content of the uploaded file matches the local file
    # # Wait for a bit to let the dockerised agent upload the file
    # time.sleep(hplc_agent.hplc_report_periodic_timescale * 2)
    # # Query remote file path
    # # time.sleep(600)
    # remote_file_path = utils.cf.host_docker_internal_to_localhost(sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, local_file_path_in_docker))
    # # Genereate random download path
    # full_downloaded_path = generate_random_download_path(hplc_agent.hplc_report_file_extension)
    # # Download the file and make sure all the content are the same
    # sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    # assert filecmp.cmp(generated_file_path,full_downloaded_path)
    ###################################################################################################
    ## NOTE above block of codes is commented out as the agent.hplc.env.test::DRY_RUN is set to True ##
    ## Therefore the agent should generate and upload the report file to the KG fileserver by itself ##
    ###################################################################################################

    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_digital_twin, rxn_exp_iri, chemical_amount_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    # NOTE no need to remove the file as the agent in docker generates file by itself
    # # Finally, remove the hplc report file generated in the docker integration folder
    # os.remove(generated_file_path)
