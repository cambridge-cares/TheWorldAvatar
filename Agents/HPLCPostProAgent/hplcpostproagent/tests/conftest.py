# NOTE courtesy of Daniel (dln22), this file is adapted from https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_uploader/tests/conftest.py
from rdflib import Graph
import requests
import pkgutil
import pytest
import shutil
import time
import uuid
import os

from pyderivationagent.conf import config_derivation_agent

from hplcpostproagent.kg_operations import ChemistryAndRobotsSparqlClient
from hplcpostproagent.agent import *
from flask import Flask

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')
HPLC_REPORT_XLS_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls.xls'
HPLC_REPORT_TXT_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt.txt'
HPLC_REPORT_XLS_INCOMPLETE_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls_incomplete.xls'
HPLC_REPORT_TXT_INCOMPLETE_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt_incomplete.txt'
HPLC_REPORT_XLS_UNIDENTIFIED_PEAKS_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls_unidentified_peaks.xls'
HPLC_REPORT_TXT_UNIDENTIFIED_PEAKS_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt_unidentified_peaks.txt'
HPLC_REPORT_XLS_NO_PRODUCT_PATH_IN_PKG = 'sample_data/raw_hplc_report_xls_no_product.xls'
HPLC_REPORT_TXT_NO_PRODUCT_PATH_IN_PKG = 'sample_data/raw_hplc_report_txt_no_product.txt'


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
HPLC_METHOD_IRI = 'http://example.com/blazegraph/namespace/testlab/dummy_lab/HPLCMethod_Dummy'

# Configuration env files
# NOTE the triple store URL provided in the agent.*.env files are the URL to access blazegraph container WITHIN the docker stack
HPLC_POSTPRO_AGENT_ENV = os.path.join(THIS_DIR,'agent.hplc.postpro.env.test')


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

# NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
# For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
@pytest.fixture(scope="session")
def get_service_url(session_scoped_container_getter):
    def _get_service_url(service_name, url_route):
        service = session_scoped_container_getter.get(service_name).network_info[0]
        service_url = f"http://localhost:{service.host_port}/{url_route}"

        # this will run only once per entire test session
        # it ensures that the services requested in docker containers are ready
        # e.g. the blazegraph service is ready to accept SPARQL query/update
        service_available = False
        while not service_available:
            try:
                response = requests.head(service_url)
                if response.status_code != requests.status_codes.codes.not_found:
                    service_available = True
            except requests.exceptions.ConnectionError:
                time.sleep(3)

        return service_url
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
def initialise_client(get_service_url, get_service_auth):
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

    # Clear triple store before any usage
    sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

    yield sparql_client

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


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_hplc_postpro_agent():
    def _create_hplc_postpro_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
    ):
        hplc_postpro_agent_config = config_derivation_agent(HPLC_POSTPRO_AGENT_ENV)
        hplc_postpro_agent = HPLCPostProAgent(
            register_agent=hplc_postpro_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=hplc_postpro_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=hplc_postpro_agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=hplc_postpro_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(hplc_postpro_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(hplc_postpro_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=hplc_postpro_agent_config.KG_USERNAME,
            kg_password=hplc_postpro_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(hplc_postpro_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=hplc_postpro_agent_config.FILE_SERVER_USERNAME,
            fs_password=hplc_postpro_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=hplc_postpro_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            flask_config=FlaskConfigTest(), # NOTE prevent "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
            logger_name='dev',
            max_thread_monitor_async_derivations=hplc_postpro_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=hplc_postpro_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=hplc_postpro_agent_config.EMAIL_SUBJECT_PREFIX+' WSL2',
            email_username=hplc_postpro_agent_config.EMAIL_USERNAME,
            email_auth_json_path=os.path.join(SECRETS_PATH,'email_auth.json'),
            email_start_end_async_derivations=hplc_postpro_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return hplc_postpro_agent
    return _create_hplc_postpro_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def host_docker_internal_to_localhost(endpoint: str):
    return endpoint.replace("host.docker.internal:", "localhost:")

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

def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])
