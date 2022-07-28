import shutil
import tempfile
from flask import Flask
from testcontainers.core.container import DockerContainer

from typing import get_type_hints
from pathlib import Path
import logging
import pytest
import random
import uuid
import time
import os

from pyderivationagent.agent import FlaskConfig
from pyderivationagent.conf import config_derivation_agent
from pyderivationagent.conf import config_generic
from pyderivationagent.conf import AgentConfig
from pyderivationagent.conf import Config

from tests.agents.sparql_client_for_test import PySparqlClientForTest
from tests.agents.agents_for_test import RNGAgent
from tests.agents.agents_for_test import MaxValueAgent
from tests.agents.agents_for_test import MinValueAgent
from tests.agents.agents_for_test import DifferenceAgent
from tests.agents.agents_for_test import UpdateEndpoint

logging.getLogger("py4j").setLevel(logging.INFO)


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
RESOURCE_DIR = os.path.join(str(Path(__file__).absolute().parent),'resources')
ENV_FILES_DIR = os.path.join(THIS_DIR,'env_files')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
TEMP_ENV_FILE_DIR = os.path.join(THIS_DIR,'_temp_env_file')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
UPDATE_ENDPOINT_SERVICE = "update_endpoint"
UPDATE_ENDPOINT_ROUTE = "update"

DERIVATION_INSTANCE_BASE_URL = 'http://www.asyncagent.com/triplestore/repository/'
DERIVATION_PERIODIC_TIMESCALE = 3

# Configuration env files
# NOTE the triple store URL provided in the agent.*.env.test files are the URL to access blazegraph container WITHIN the docker stack
RNGAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.rng.env.test')
MAXAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.max.env.test')
MINAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.min.env.test')
DIFFAGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.diff.env.test')
UPDATEENDPOINT_ENV = os.path.join(ENV_FILES_DIR,'endpoint.update.env.test')


# ----------------------------------------------------------------------------------
# Helper classes
# ----------------------------------------------------------------------------------

class FlaskConfigTest(FlaskConfig):
    # NOTE this to prevent below Exception when instantiating the all four agents in the integration test cases:
    # "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    SCHEDULER_API_ENABLED = False


class AllInstances():
    RNGAGENT_SERVICE: str = config_derivation_agent(RNGAGENT_ENV).ONTOAGENT_SERVICE_IRI
    MAXAGENT_SERVICE: str = config_derivation_agent(MAXAGENT_ENV).ONTOAGENT_SERVICE_IRI
    MINAGENT_SERVICE: str = config_derivation_agent(MINAGENT_ENV).ONTOAGENT_SERVICE_IRI
    DIFFAGENT_SERVICE: str = config_derivation_agent(DIFFAGENT_ENV).ONTOAGENT_SERVICE_IRI

    IRI_UPPER_LIMIT: str = None
    IRI_LOWER_LIMIT: str = None
    IRI_NUM_OF_PTS: str = None
    IRI_LST_PTS: str = None
    IRI_MAX: str = None
    IRI_MIN: str = None
    IRI_DIFF: str = None

    VAL_UPPER_LIMIT: int = None
    VAL_LOWER_LIMIT: int = None
    VAL_NUM_OF_PTS: int = None
    VAL_MAX: int = None
    VAL_MIN: int = None
    VAL_DIFF: int = None

    DERIV_RNG: str = None
    DERIV_MAX: str = None
    DERIV_MIN: str = None
    DERIV_DIFF: str = None


class Config4Test1(Config):
    STR_1: str
    INT_1: int
    BOOL_1_1: bool
    BOOL_1_2: bool


class Config4Test2(Config4Test1):
    STR_2: str
    INT_2: int
    BOOL_2_1: bool
    BOOL_2_2: bool


# ----------------------------------------------------------------------------------
# Pytest session related functions
# ----------------------------------------------------------------------------------

def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(TEMP_ENV_FILE_DIR):
        shutil.rmtree(TEMP_ENV_FILE_DIR)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(TEMP_ENV_FILE_DIR):
        shutil.rmtree(TEMP_ENV_FILE_DIR)


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


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

# NOTE the scope is set as "module", i.e., all triples (pure inputs, TBox, OntoAgent instances) will only be initialised once
@pytest.fixture(scope="module")
def initialise_clients(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Retrieve endpoint for update_endpoint
    update_endpoint = get_service_url(UPDATE_ENDPOINT_SERVICE, url_route=UPDATE_ENDPOINT_ROUTE)

    # Create SparqlClient for testing
    sparql_client = PySparqlClientForTest(
        sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd
    )

    # Create DerivationClient for creating derivation instances
    derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        sparql_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    yield sparql_client, derivation_client, update_endpoint

    # Clear logger at the end of the test
    clear_loggers()


@pytest.fixture(scope="module")
def initialise_triple_store():
    # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer(
        'docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    # the port is set as 9999 to match with the value set in the docker image
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


@pytest.fixture(scope="module")
def initialise_agent(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(3)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)

        # Create SparqlClient for testing
        sparql_client = PySparqlClientForTest(endpoint, endpoint)

        # Create DerivationClient for creating derivation instances
        derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
            sparql_client.kg_client,
            DERIVATION_INSTANCE_BASE_URL
        )

        # Delete all triples before registering agents
        sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

        # Initialise derivation agents with temporary docker container endpoint
        rng_agent = create_rng_agent(RNGAGENT_ENV, endpoint)
        min_agent = create_min_agent(MINAGENT_ENV, endpoint)
        max_agent = create_max_agent(MAXAGENT_ENV, endpoint)
        diff_agent = create_diff_agent(DIFFAGENT_ENV, endpoint)

        yield sparql_client, derivation_client, rng_agent, min_agent, max_agent, diff_agent

        # Tear down scheduler of derivation agents
        rng_agent.scheduler.shutdown()
        min_agent.scheduler.shutdown()
        max_agent.scheduler.shutdown()
        diff_agent.scheduler.shutdown()

        # Clear logger at the end of the test
        clear_loggers()


# ----------------------------------------------------------------------------------
# Function-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="function")
def generate_random_conf():
    def _generate_random_conf(cls: Config):
        conf_test_dct = {}
        for field in cls.all_annotations():
            var_type = get_type_hints(cls)[field]
            if var_type == bool:
                conf_test_dct[field] = bool(random.getrandbits(1))
            elif var_type == str:
                conf_test_dct[field] = str(uuid.uuid4())
            elif var_type == int:
                conf_test_dct[field] = random.randint(0, 100)
            else:
                raise ValueError('Unsupported type: {}'.format(var_type))
        return conf_test_dct
    return _generate_random_conf


@pytest.fixture(scope="function")
def generate_random_env_file(generate_random_conf):
    def _generate_random_env_file(cls: Config):
        if not os.path.exists(TEMP_ENV_FILE_DIR):
            os.makedirs(TEMP_ENV_FILE_DIR)

        conf_test_dct = generate_random_conf(cls)
        env_file_path = os.path.join(TEMP_ENV_FILE_DIR, str(uuid.uuid4()) + ".env.test")
        env_file = open(env_file_path, "x")
        env_content = "\n".join(["{}={}".format(k, v) for k, v in conf_test_dct.items()])
        env_file.write(env_content)
        env_file.close()
        return env_file_path, conf_test_dct
    return _generate_random_env_file


# ----------------------------------------------------------------------------------
# Agents create functions
# ----------------------------------------------------------------------------------

def create_rng_agent(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        agent_config = config_derivation_agent()
    else:
        agent_config = config_derivation_agent(env_file)
    return RNGAgent(
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint if sparql_endpoint is not None else agent_config.SPARQL_QUERY_ENDPOINT,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent=True,
        app=Flask(__name__)
    )


def create_max_agent(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        agent_config = config_derivation_agent()
    else:
        agent_config = config_derivation_agent(env_file)
    return MaxValueAgent(
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint if sparql_endpoint is not None else agent_config.SPARQL_QUERY_ENDPOINT,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent=True,
        app=Flask(__name__)
    )


def create_min_agent(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        agent_config = config_derivation_agent()
    else:
        agent_config = config_derivation_agent(env_file)
    return MinValueAgent(
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint if sparql_endpoint is not None else agent_config.SPARQL_QUERY_ENDPOINT,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent=True,
        app=Flask(__name__)
    )


def create_diff_agent(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        agent_config = config_derivation_agent()
    else:
        agent_config = config_derivation_agent(env_file)
    return DifferenceAgent(
        agent_iri=agent_config.ONTOAGENT_SERVICE_IRI,
        time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint if sparql_endpoint is not None else agent_config.SPARQL_QUERY_ENDPOINT,
        agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
        register_agent=True,
        app=Flask(__name__)
    )


def create_update_endpoint(env_file: str = None, sparql_endpoint: str = None):
    if env_file is None:
        endpoint_config = config_derivation_agent()
    else:
        endpoint_config = config_derivation_agent(env_file)
    return UpdateEndpoint(
        agent_iri=endpoint_config.ONTOAGENT_SERVICE_IRI, # just placeholder value, not used by anything
        time_interval=endpoint_config.DERIVATION_PERIODIC_TIMESCALE, # just placeholder value, not used by anything
        derivation_instance_base_url=endpoint_config.DERIVATION_INSTANCE_BASE_URL, # just placeholder value, not used by anything
        kg_url=sparql_endpoint if sparql_endpoint is not None else endpoint_config.SPARQL_QUERY_ENDPOINT,
        register_agent=False # the default value is True, so here we set it to False as we don't want to register the endpoint
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
