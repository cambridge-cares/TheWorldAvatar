import logging
import pytest
import filecmp
import time

logging.getLogger("py4j").setLevel(logging.INFO)

from agilentagent.agent import *

pytest_plugins = ["docker_compose"]

HPLC_DIGITAL_TWIN_1 = 'http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLC_1'
HPLC_DIGITAL_TWIN_2 = 'http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/HPLC_2'
HPLC_REPORT_PERIODIC_TIMESCALE = 10
HPLC_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__HPLCInput#Service'
DERIVATION_PERIODIC_TIMESCALE = 120
DERIVATION_INSTANCE_BASE_URL = 'http://localhost:8080/ontolab/'

class FlaskConfigTest(FlaskConfig):
    # NOTE this to prevent below Exception when instantiating the HPLCInputAgent in the second-fourth test cases:
    # "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    SCHEDULER_API_ENABLED = False

# NOTE the hplc_report_periodic_timescale (8-11) are chosen randomly for the test cases
@pytest.mark.parametrize(
    "hplc_digital_twin,hplc_report_periodic_timescale,filename_extension",
    [
        (HPLC_DIGITAL_TWIN_1, 8, XLSFILE_EXTENSION),
        (HPLC_DIGITAL_TWIN_1, 9, XLSFILE_EXTENSION),
        (HPLC_DIGITAL_TWIN_2, 10, TXTFILE_EXTENSION),
        (HPLC_DIGITAL_TWIN_2, 11, TXTFILE_EXTENSION),
    ],
)
def test_hplc_input_agent(initialise_triples, create_test_report, generate_random_download_path, hplc_digital_twin, hplc_report_periodic_timescale, filename_extension):
    sparql_client, sparql_endpoint, fs_url, fs_user, fs_pwd = initialise_triples

    hplc_input_agent = AgilentHPLCInputAgent(
        hplc_digital_twin=hplc_digital_twin, hplc_report_periodic_timescale=hplc_report_periodic_timescale,
        fs_url=fs_url, fs_user=fs_user, fs_pwd=fs_pwd,
        agent_iri=HPLC_ONTOAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL, kg_url=sparql_endpoint, logger_name='dev',
        flask_config=FlaskConfigTest() # NOTE prevent "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    )

    hplc_input_agent.start_monitoring_local_report_folder()

    # Create a random file to be uploaded
    time.sleep(hplc_report_periodic_timescale * 2)
    generated_file_path = create_test_report(filename_extension)

    # Wait for a bit to let the agent upload the file
    time.sleep(hplc_report_periodic_timescale * 2)

    # Query remote file path
    remote_file_path = sparql_client.get_remote_hplc_report_path_given_local_file(hplc_digital_twin, generated_file_path)

    # Genereate random download path
    full_downloaded_path = generate_random_download_path(filename_extension)

    # Download the file and make sure all the content are the same
    sparql_client.download_remote_raw_hplc_report(remote_file_path=remote_file_path, downloaded_file_path=full_downloaded_path)
    assert filecmp.cmp(generated_file_path,full_downloaded_path)

    # Shutdown the scheduler to clean up before the next test
    hplc_input_agent.scheduler.shutdown()
