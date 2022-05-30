from testcontainers.core.container import DockerContainer
from pathlib import Path
from flask import Flask
import logging
import pytest
import shutil
import time
import uuid
import os

from pyderivationagent.conf import config_derivation_agent
from vtexeagent.agent import VapourtecExecutionAgent

from vtexeagent.kg_operations import ChemistryAndRobotsSparqlClient

logging.getLogger("py4j").setLevel(logging.INFO)


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR,'env_files')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"

# Configuration env files
# NOTE the triple store URL provided in the agent.*.env files are the URL to access blazegraph container via host.docker.internal
DOEAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.doe.env.test')
VTEXEAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vtexe.env.test')
POSTPROCAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.postproc.env.test')

DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(DOEAGENT_ENV).DERIVATION_INSTANCE_BASE_URL
DOEAGENT_SERVICE_IRI = config_derivation_agent(DOEAGENT_ENV).ONTOAGENT_SERVICE_IRI
VTEXEAGENT_SERVICE_IRI = config_derivation_agent(VTEXEAGENT_ENV).ONTOAGENT_SERVICE_IRI
POSTPROCAGENT_SERVICE_IRI = config_derivation_agent(POSTPROCAGENT_ENV).ONTOAGENT_SERVICE_IRI

DERIVATION_INPUTS = ['https://www.example.com/triplestore/ontodoe/DoE_1/DoE_1']
DOE_IRI = DERIVATION_INPUTS[0]


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
def generate_random_download_path():
    def _generate_random_download_path(filename_extension):
        return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)
    return _generate_random_download_path

# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

# NOTE the scope is set as "module", i.e., all triples (pure inputs, TBox, OntoAgent instances) will only be initialised once
@pytest.fixture(scope="module")
def initialise_clients(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(
        sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd
    )

    # Create DerivationClient for creating derivation instances
    derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        sparql_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    yield sparql_client, derivation_client

    # Clear logger at the end of the test
    clear_loggers()


# @pytest.fixture(scope="module")
# def initialise_triple_store():
#     # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
#     # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
#     blazegraph = DockerContainer(
#         'docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
#     # the port is set as 9999 to match with the value set in the docker image
#     blazegraph.with_exposed_ports(9999)
#     yield blazegraph


# @pytest.fixture(scope="module")
# def initialise_agent(initialise_triple_store):
#     with initialise_triple_store as container:
#         # Wait some arbitrary time until container is reachable
#         time.sleep(3)

#         # Retrieve SPARQL endpoint
#         endpoint = get_endpoint(container)

#         # Create SparqlClient for testing
#         sparql_client = ChemistryAndRobotsSparqlClient(endpoint, endpoint)

#         # Create DerivationClient for creating derivation instances
#         derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
#             sparql_client.kg_client,
#             DERIVATION_INSTANCE_BASE_URL
#         )

#         # Initialise Async Agent with temporary docker container endpoint
#         doe_agent = create_doe_agent(DOEAGENT_ENV, endpoint)

#         yield sparql_client, derivation_client, doe_agent

#         # Tear down scheduler of doe agent
#         doe_agent.scheduler.shutdown()

#         # Clear logger at the end of the test
#         clear_loggers()


# ----------------------------------------------------------------------------------
# Agents create functions
# ----------------------------------------------------------------------------------

def create_vtexe_agent(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        agent_config = config_derivation_agent()
    else:
        agent_config = config_derivation_agent(env_file)
    return VapourtecExecutionAgent(
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=agent_config.SPARQL_QUERY_ENDPOINT if sparql_endpoint is None else sparql_endpoint,
        kg_update_url=agent_config.SPARQL_UPDATE_ENDPOINT if sparql_endpoint is None else sparql_endpoint,
        kg_user=agent_config.KG_USERNAME,
        kg_password=agent_config.KG_PASSWORD,
        fs_url=agent_config.FILE_SERVER_ENDPOINT,
        fs_user=agent_config.FILE_SERVER_USERNAME,
        fs_password=agent_config.FILE_SERVER_PASSWORD,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL
    )


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

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
