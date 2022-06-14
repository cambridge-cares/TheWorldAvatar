import logging
import pytest
import filecmp
import time
import os

logging.getLogger("py4j").setLevel(logging.INFO)
logger = logging.getLogger("test_hplc_input")

import agilentagent.tests.utils as utils

pytest_plugins = ["docker_compose"]


# NOTE the hplc_report_periodic_timescale (8, 11) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "hplc_report_periodic_timescale,hplc_report_file_extension",
    [
        (8, utils.cf.XLSFILE_EXTENSION),
        (11, utils.cf.TXTFILE_EXTENSION),
    ],
)
def test_monitor_local_report_folder(
    initialise_client, initialise_hplc_digital_twin_triples, create_agilent_agent,
    create_test_report, generate_random_download_path,
    hplc_report_periodic_timescale, hplc_report_file_extension
):

    sparql_client = initialise_client
    hplc_digital_twin = initialise_hplc_digital_twin_triples(hplc_report_file_extension)
    agilent_agent = create_agilent_agent(hplc_digital_twin, hplc_report_periodic_timescale, hplc_report_file_extension)

    agilent_agent.start_monitoring_local_report_folder()

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
    agilent_agent.scheduler.shutdown()


# NOTE the hplc_report_periodic_timescale (8, 11) are chosen randomly for the test cases
# NOTE the derivation_periodic_timescale (5, 8) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "hplc_report_periodic_timescale,hplc_report_file_extension,derivation_periodic_timescale",
    [
        (8, utils.cf.XLSFILE_EXTENSION, 5),
        (11, utils.cf.TXTFILE_EXTENSION, 8),
    ],
)
def test_monitor_derivation(
    initialise_client, initialise_hplc_digital_twin_triples, create_agilent_agent, initialise_agilent_derivation_input_triples,
    create_test_report, generate_random_download_path,
    hplc_report_periodic_timescale, hplc_report_file_extension, derivation_periodic_timescale
):

    sparql_client = initialise_client
    hplc_digital_twin = initialise_hplc_digital_twin_triples(hplc_report_file_extension)
    agilent_agent = create_agilent_agent(hplc_digital_twin, hplc_report_periodic_timescale, hplc_report_file_extension, derivation_periodic_timescale)

    ## Instantiate derivation instance
    rxn_exp_iri, chemical_solution_iri = initialise_agilent_derivation_input_triples(sparql_client)
    derivation_inputs = [rxn_exp_iri, chemical_solution_iri]
    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        agilent_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        agilent_agent.derivationClient.updateTimestamp(input)
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = agilent_agent.derivationClient.createAsyncDerivationForNewInfo(agilent_agent.agentIRI, derivation_inputs)
    logger.info(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    # Start monitor derivations
    agilent_agent.start_monitoring_derivations()

    # Wait for some arbitrary time more then the derivation_periodic_timescale
    time.sleep(derivation_periodic_timescale + 1)

    # Generate random file and upload it to KG fileserver
    agilent_agent.get_dict_of_hplc_files() # perform the init check first
    generated_file_path = create_test_report(hplc_report_file_extension) # generate report
    agilent_agent.monitor_local_report_folder() # now the generated report can be uploaded

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
    lst_hplc_job_iri = utils.get_hplc_job(hplc_digital_twin, rxn_exp_iri, chemical_solution_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    # Shutdown the scheduler to clean up before the next test
    agilent_agent.scheduler.shutdown()


def test_docker_integration(
    initialise_client, initialise_hplc_digital_twin_triples, create_agilent_agent, initialise_agilent_derivation_input_triples,
    create_test_report, generate_random_download_path,
):

    sparql_client = initialise_client
    agilent_agent = create_agilent_agent(register=False)
    hplc_digital_twin = initialise_hplc_digital_twin_triples(agilent_agent.hplc_report_file_extension, agilent_agent.hplc_digital_twin)

    ## Instantiate derivation instance
    rxn_exp_iri, chemical_solution_iri = initialise_agilent_derivation_input_triples(sparql_client)
    derivation_inputs = [rxn_exp_iri, chemical_solution_iri]
    # Iterate over the list of inputs to add and update the timestamp
    for input in derivation_inputs:
        agilent_agent.derivationClient.addTimeInstance(input)
        # Update timestamp is needed as the timestamp added using addTimeInstance() is 0
        agilent_agent.derivationClient.updateTimestamp(input)
    # Create derivation instance given above information, the timestamp of this derivation is 0
    derivation_iri = agilent_agent.derivationClient.createAsyncDerivationForNewInfo(agilent_agent.agentIRI, derivation_inputs)
    logger.info(f'Initialised successfully, created derivation instance: <{derivation_iri}>')

    # Wait for some arbitrary time more then the derivation_periodic_timescale
    time.sleep(agilent_agent.time_interval + 1)

    # Generate random file to the docker integration folder, the agent deployed in docker will pick it up
    generated_file_path = create_test_report(agilent_agent.hplc_report_file_extension, True)

    ## Check if the content of the uploaded file matches the local file
    # Wait for a bit to let the dockerised agent upload the file
    time.sleep(agilent_agent.hplc_report_periodic_timescale * 2)
    # Query remote file path
    remote_file_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, generated_file_path)
    # Genereate random download path
    full_downloaded_path = generate_random_download_path(agilent_agent.hplc_report_file_extension)
    # Download the file and make sure all the content are the same
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(generated_file_path,full_downloaded_path)

    ## Check if the derivation is processed and generated the desired triples
    # Query timestamp of the derivation for every 20 until it's updated
    currentTimestamp_derivation = 0
    while currentTimestamp_derivation == 0:
        time.sleep(20)
        currentTimestamp_derivation = utils.get_timestamp(derivation_iri, sparql_client)
    lst_hplc_job_iri = utils.get_hplc_job(hplc_digital_twin, rxn_exp_iri, chemical_solution_iri, sparql_client)
    lst_derivation_outputs_iri = utils.get_derivation_outputs(derivation_iri, sparql_client)
    assert len(lst_hplc_job_iri) == 1
    assert len(lst_derivation_outputs_iri) == 1
    assert lst_hplc_job_iri == lst_derivation_outputs_iri

    # Finally, remove the hplc report file generated in the docker integration folder
    os.remove(generated_file_path)
