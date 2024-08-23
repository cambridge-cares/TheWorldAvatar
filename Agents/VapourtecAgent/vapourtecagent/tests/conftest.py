from flask import Flask
import requests
import pytest
import shutil
import time
import uuid
import os

from vapourtecagent.kg_operations import ChemistryAndRobotsSparqlClient
from vapourtecagent.data_model import *
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.conf import config_vapourtec_agent


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
FCEXP_FILE_DIR = os.path.join(THIS_DIR,'_generated_vapourtec_input_file_for_test')
DOCKER_INTEGRATION_DIR = os.path.join(THIS_DIR,'_for_docker_integration_test')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'_downloaded_files_for_test')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"

VAPOURTEC_AGENT_ENV = os.path.join(THIS_DIR,'agent.vapourtec.env.test')

DUMMY_LAB_BASE_IRI = 'https://example.com/blazegraph/namespace/testlab/dummy_lab/'
VAPOURTECRS400_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecRS400_Dummy'
VAPOURTECR4REACTOR_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Dummy'
VAPOURTECR4REACTOR_ANOTHER_DUMMY_IRI = DUMMY_LAB_BASE_IRI + 'VapourtecR4_Another_Dummy'
EXP_1_BASE_IRI = 'https://www.example.com/triplestore/ontorxn/ReactionExperiment_1/'
NEW_RXN_EXP_1_IRI = EXP_1_BASE_IRI + 'ReactionVariation_fac53bb1-3ae0-4941-9f5b-38738b07ab70'
NEW_RXN_EXP_2_IRI = EXP_1_BASE_IRI + 'ReactionVariation_3bd3166d-f782-4cdc-a6a8-75336afd71a8'
NEW_RXN_EXP_3_IRI = EXP_1_BASE_IRI + 'ReactionVariation_c4b175d9-e53c-4d7e-b053-3a81f7ca0ddf'


def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(FCEXP_FILE_DIR):
        shutil.rmtree(FCEXP_FILE_DIR)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(FCEXP_FILE_DIR):
        shutil.rmtree(FCEXP_FILE_DIR)
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
def generate_random_download_path():
    def _generate_random_download_path(filename_extension):
        return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)
    return _generate_random_download_path

@pytest.fixture(scope="session")
def generate_random_vapourtec_digital_twin():
    def _generate_random_vapourtec_digital_twin(predefined_vapourtec_digital_twin):
        if predefined_vapourtec_digital_twin is None:
            return f'http://www.example.com/placeholder/Vapourtec_{str(uuid.uuid4())}'
        else:
            return predefined_vapourtec_digital_twin
    return _generate_random_vapourtec_digital_twin

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

    # Create folder for HPLC report files
    if not os.path.exists(FCEXP_FILE_DIR):
        os.mkdir(FCEXP_FILE_DIR)
    # Create folder for downloaded files
    if not os.path.exists(DOWNLOADED_DIR):
        os.mkdir(DOWNLOADED_DIR)

    # Clear triple store before any usage
    sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

    yield sparql_client

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_vapourtec_agent():
    def _create_vapourtec_agent(
        vapourtec_digital_twin:str=None,
        vapourtec_state_periodic_timescale:int=None,
        fcexp_file_container_folder:str=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        vapourtec_agent_config = config_vapourtec_agent(VAPOURTEC_AGENT_ENV)
        vapourtec_agent = VapourtecAgent(
            vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN if vapourtec_digital_twin is None else vapourtec_digital_twin,
            vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE if vapourtec_state_periodic_timescale is None else vapourtec_state_periodic_timescale,
            vapourtec_ip_address=vapourtec_agent_config.VAPOURTEC_IP_ADDRESS,
            fcexp_file_container_folder=vapourtec_agent_config.FCEXP_FILE_CONTAINER_FOLDER if fcexp_file_container_folder is None else fcexp_file_container_folder,
            register_agent=vapourtec_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=vapourtec_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=vapourtec_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=vapourtec_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(vapourtec_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(vapourtec_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=vapourtec_agent_config.KG_USERNAME,
            kg_password=vapourtec_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(vapourtec_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=vapourtec_agent_config.FILE_SERVER_USERNAME,
            fs_password=vapourtec_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=vapourtec_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            max_thread_monitor_async_derivations=vapourtec_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=vapourtec_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=vapourtec_agent_config.EMAIL_SUBJECT_PREFIX+' WSL2',
            email_username=vapourtec_agent_config.EMAIL_USERNAME,
            email_auth_json_path=os.path.join(SECRETS_PATH,'email_auth.json'),
            email_start_end_async_derivations=vapourtec_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return vapourtec_agent
    return _create_vapourtec_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def host_docker_internal_to_localhost(endpoint: str):
    return endpoint.replace("host.docker.internal:", "localhost:")

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
