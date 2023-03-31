from testcontainers.core.container import DockerContainer
from pathlib import Path
from flask import Flask
import requests
import logging
import pytest
import shutil
import time
import uuid
import os

from pyderivationagent.conf import config_derivation_agent
from pyderivationagent import PyDerivationClient

from doeagent.kg_operations import ChemistryAndRobotsSparqlClient
from doeagent.agent import DoEAgent
from doeagent.data_model import *


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
RESOURCE_DIR = os.path.join(str(Path(__file__).absolute().parent),'resources')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')
TEST_TRIPLES_DIR = os.path.join(THIS_DIR, 'test_triples')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"

# Configuration env files
# NOTE the triple store URL provided in the agent.*.env files are the URL to access blazegraph container WITHIN the docker stack
DOEAGENT_ENV = os.path.join(THIS_DIR,'agent.doe.env.test')

DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(DOEAGENT_ENV).DERIVATION_INSTANCE_BASE_URL

DOE_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1'
DERIVATION_INPUTS = [DOE_IRI]

DOE_NO_PRIOR_EXPERIMENT_IRI = 'https://www.example.com/triplestore/ontodoe/DoE_no_prior_data/DoE_1'

# ----------------------------------------------------------------------------------
# Pytest session related functions
# ----------------------------------------------------------------------------------

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
def initialise_clients(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(
        sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd
    )

    # Clear triple store before any usage
    sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

    # Create DerivationClient for creating derivation instances
    derivation_client = PyDerivationClient(
        DERIVATION_INSTANCE_BASE_URL,
        sparql_endpoint, sparql_endpoint,
    )

    yield sparql_client, derivation_client

    # Clear logger at the end of the test
    clear_loggers()


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_doe_agent():
    def _create_doe_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
    ):
        doe_agent_config = config_derivation_agent(DOEAGENT_ENV)
        doe_agent = DoEAgent(
            register_agent=doe_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=doe_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=doe_agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=doe_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(doe_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(doe_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=doe_agent_config.KG_USERNAME,
            kg_password=doe_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(doe_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=doe_agent_config.FILE_SERVER_USERNAME,
            fs_password=doe_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=doe_agent_config.ONTOAGENT_OPERATION_HTTP_URL, # we keep this as it is for now (start with http://host.docker.internal)
            max_thread_monitor_async_derivations=doe_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=doe_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=doe_agent_config.EMAIL_SUBJECT_PREFIX+' WSL2',
            email_username=doe_agent_config.EMAIL_USERNAME,
            email_auth_json_path=os.path.join(SECRETS_PATH,'email_auth.json'),
            email_start_end_async_derivations=doe_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
            app=Flask(__name__)
        )
        return doe_agent
    return _create_doe_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def host_docker_internal_to_localhost(endpoint: str):
    return endpoint.replace("host.docker.internal:", "localhost:")


def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
