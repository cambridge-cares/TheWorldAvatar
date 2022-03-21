# NOTE courtesy of Daniel (dln22), this file is adapted from https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_uploader/tests/conftest.py
from rdflib import Graph
import logging
import pkgutil
import pytest
import shutil
import time
import uuid
import os

logging.getLogger("py4j").setLevel(logging.INFO)

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient
from postprocagent.agent import *

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')
HPLC_REPORT_XLS_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls.xls'
HPLC_REPORT_TXT_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt.txt'

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"

DUMMY_LAB_FOR_POST_PROC_BASE_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab_for_post_proc/'
EXP_1_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/'
NEW_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
NEW_RXN_EXP_2_IRI = EXP_1_BASE_IRI + 'ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
HPLC_DIGITAL_TWIN_1 = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'HPLC_1'
HPLC_DIGITAL_TWIN_2 = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'HPLC_2'
CHEMICAL_SOLUTION_1 = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'ChemicalSolution_1_1'
CHEMICAL_SOLUTION_2 = DUMMY_LAB_FOR_POST_PROC_BASE_IRI + 'ChemicalSolution_2_1'
PLACEHOLDER_PERFORMANCE_INDICATOR_LIST_1 = [
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_yield_1',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_conversion_1',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_eco_score_1',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_e_factor_1',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_sty_1',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_cost_1'
]
PLACEHOLDER_PERFORMANCE_INDICATOR_LIST_2 = [
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_yield_2',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_conversion_2',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_eco_score_2',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_e_factor_2',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_sty_2',
    DUMMY_LAB_FOR_POST_PROC_BASE_IRI+'placeholder_cost_2'
]

POSTPROC_ONTOAGENT_SERVICE = 'http://www.theworldavatar.com/resource/agents/Service__PostProc#Service'
DERIVATION_PERIODIC_TIMESCALE = 20
DERIVATION_INSTANCE_BASE_URL = 'http://localhost:8080/ontolab/'

class FlaskConfigTest(FlaskConfig):
    # NOTE this to prevent below Exception when instantiating the HPLCInputAgent in the second-fourth test cases:
    # "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    SCHEDULER_API_ENABLED = False

def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

# ----------------------------------------------------------------------------------
# Session-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="session")
def get_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"http://localhost:{service.host_port}/{url_route}"
        return service_url

    # this will run only once per entire test session and ensures that all the services
    # in docker containers are ready. Increase the sleep value in case services need a bit
    # more time to run on your machine.
    time.sleep(8)
    return _get_service_url

@pytest.fixture(scope="session")
def get_service_auth():
    def _get_service_auth(service_name):
        password_file = os.path.join(SECRETS_PATH,service_name+'_passwd.txt')
        user_file = os.path.join(SECRETS_PATH,service_name+'_user.txt')

        # read service auth from files
        username = ''
        password = ''
        if os.path.exists(user_file):
            with open(user_file) as f:
                username = f.read().strip()
        if os.path.exists(password_file):
            with open(password_file) as f:
                password = f.read().strip()

        return username, password

    return _get_service_auth

@pytest.fixture(scope="session")
def initialise_triples(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Retrieve endpoint and auth for file server
    fs_url = get_service_url(FS_SERVICE, url_route=FS_ROUTE)
    fs_user, fs_pwd = get_service_auth(FS_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd,
        fs_url=fs_url, fs_user=fs_user, fs_pwd=fs_pwd
    )

    # Create folder for downloaded files
    if not os.path.exists(DOWNLOADED_DIR):
        os.mkdir(DOWNLOADED_DIR)

    # Upload the example triples for testing
    for f in ['ontoagent/Service__PostProc.ttl', 'sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl',
        'sample_data/dummy_lab.ttl', 'sample_data/rxn_data.ttl', 'sample_data/dummy_post_proc.ttl']:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = generate_random_download_path('ttl')
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)

    # Initialise PostProcAgent
    post_proc_agent = PostProcAgent(
        fs_url=fs_url, fs_user=fs_user, fs_pwd=fs_pwd,
        agent_iri=POSTPROC_ONTOAGENT_SERVICE, time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL, kg_url=sparql_endpoint, logger_name='dev',
        flask_config=FlaskConfigTest() # NOTE prevent "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    )

    yield sparql_client, post_proc_agent

    # Clear logger at the end of the test
    clear_loggers()

@pytest.fixture(scope="session")
def retrieve_hplc_report():
    def _retrieve_hplc_report(report_path_in_pkg):
        if report_path_in_pkg.endswith('.xls'):
            local_file_path = generate_random_download_path('xls')
        elif report_path_in_pkg.endswith('.txt'):
            local_file_path = generate_random_download_path('txt')
        else:
            raise NotImplementedError("Handling HPLC raw report (%s) in the chemistry_and_robots package is NOT yet supported due to its file extension." % 
                report_path_in_pkg)
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+report_path_in_pkg)
        with open(local_file_path, 'wb') as file_obj:
            file_obj.write(data)
        timestamp_last_modified = os.path.getmtime(local_file_path)

        return local_file_path, timestamp_last_modified
    return _retrieve_hplc_report

def generate_random_download_path(filename_extension):
    return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
