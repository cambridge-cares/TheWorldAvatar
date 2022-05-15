from flask import Flask
from testcontainers.core.container import DockerContainer

from pathlib import Path
import logging
import pytest
import time
import os

from pyderivationagent.agent import FlaskConfig

from tests.agents.sparql_client_for_test import PySparqlClientForTest
from tests.agents.agents_for_test import RNGAgent
from tests.agents.agents_for_test import MaxValueAgent
from tests.agents.agents_for_test import MinValueAgent
from tests.agents.agents_for_test import DifferenceAgent
from tests.agents.agents_for_test import UpdateEndpoint

logging.getLogger("py4j").setLevel(logging.INFO)


# ----------------------------------------------------------------------------------
# Constant
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
RESOURCE_DIR = os.path.join(str(Path(__file__).absolute().parent),'resources')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
UPDATE_ENDPOINT_SERVICE = "update_endpoint"
UPDATE_ENDPOINT_ROUTE = "update"

DERIVATION_INSTANCE_BASE_URL = 'http://www.asyncagent.com/triplestore/repository/'
DERIVATION_PERIODIC_TIMESCALE = 3

# should match one defined in Service__Random.ttl
RNGAGENT_ONTOAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Random#Service'
# should match one defined in Service__Max.ttl
MAXAGENT_ONTOAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Max#Service'
# should match one defined in Service__Min.ttl
MINAGENT_ONTOAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Min#Service'
# should match one defined in Service__Diff.ttl
DIFFAGENT_ONTOAGENT_SERVICE = 'http://www.asyncagent.com/resource/agents/Service__Diff#Service'

RNGAGENT_ENDPOINT = '/Random'
MAXAGENT_ENDPOINT = '/Max'
MINAGENT_ENDPOINT = '/Min'
DIFFAGENT_ENDPOINT = '/Diff'

# NOTE this is the URL to access blazegraph container WITHIN the docker stack
BLAZEGRAPH_ENDPOINT_WITHIN_DOCKER = "http://blazegraph:8080/blazegraph/namespace/kb/sparql"


# ----------------------------------------------------------------------------------
# Helper classes
# ----------------------------------------------------------------------------------

class FlaskConfigTest(FlaskConfig):
    # NOTE this to prevent below Exception when instantiating the all four agents in the integration test cases:
    # "AssertionError: View function mapping is overwriting an existing endpoint function: scheduler.get_scheduler_info"
    SCHEDULER_API_ENABLED = False


class AllInstances():
    RNGAGENT_SERVICE: str = RNGAGENT_ONTOAGENT_SERVICE
    MAXAGENT_SERVICE: str = MAXAGENT_ONTOAGENT_SERVICE
    MINAGENT_SERVICE: str = MINAGENT_ONTOAGENT_SERVICE
    DIFFAGENT_SERVICE: str = DIFFAGENT_ONTOAGENT_SERVICE

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


# ----------------------------------------------------------------------------------
# Pytest session related functions
# ----------------------------------------------------------------------------------

def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)


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

        # Initialise Async Agent with temporary docker container endpoint
        rng_agent = create_rng_agent(endpoint)
        min_agent = create_min_agent(endpoint)
        max_agent = create_max_agent(endpoint)
        diff_agent = create_diff_agent(endpoint)

        yield sparql_client, derivation_client, rng_agent, min_agent, max_agent, diff_agent

        # Tear down scheduler of doe agent
        rng_agent.scheduler.shutdown()
        min_agent.scheduler.shutdown()
        max_agent.scheduler.shutdown()
        diff_agent.scheduler.shutdown()

        # Clear logger at the end of the test
        clear_loggers()


# ----------------------------------------------------------------------------------
# Agents create functions
# ----------------------------------------------------------------------------------

def create_rng_agent(sparql_endpoint):
    return RNGAgent(
        agent_iri=RNGAGENT_ONTOAGENT_SERVICE,
        time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint,
        agent_endpoint=RNGAGENT_ENDPOINT,
        app=Flask(__name__)
    )


def create_max_agent(sparql_endpoint):
    return MaxValueAgent(
        agent_iri=MAXAGENT_ONTOAGENT_SERVICE,
        time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint,
        agent_endpoint=MAXAGENT_ENDPOINT,
        app=Flask(__name__)
    )


def create_min_agent(sparql_endpoint):
    return MinValueAgent(
        agent_iri=MINAGENT_ONTOAGENT_SERVICE,
        time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint,
        agent_endpoint=MINAGENT_ENDPOINT,
        app=Flask(__name__)
    )


def create_diff_agent(sparql_endpoint):
    return DifferenceAgent(
        agent_iri=DIFFAGENT_ONTOAGENT_SERVICE,
        time_interval=DERIVATION_PERIODIC_TIMESCALE,
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        kg_url=sparql_endpoint,
        agent_endpoint=DIFFAGENT_ENDPOINT,
        app=Flask(__name__)
    )


def create_update_endpoint(sparql_endpoint):
    return UpdateEndpoint(
        agent_iri="http://update", # just placeholder value, not used by anything
        time_interval=60, # just placeholder value, not used by anything
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL, # just placeholder value, not used by anything
        kg_url=sparql_endpoint
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
