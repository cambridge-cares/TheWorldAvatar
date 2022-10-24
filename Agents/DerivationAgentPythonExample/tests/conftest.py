# Avoid unnecessary logging information from py4j package
import logging
logging.getLogger("py4j").setLevel(logging.INFO)

from pathlib import Path
from rdflib import Graph
from flask import Flask
import requests
import pytest
import time
import uuid
import os

from derivationagentpythonexample.kg_operations import ExampleSparqlClient
from derivationagentpythonexample.conf import config_example_agent
from derivationagentpythonexample.agent import ExampleAgent
import derivationagentpythonexample.data_model as dm


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
TEST_TRIPLES_DIR = os.path.join(THIS_DIR, 'test_triples')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"

# Configuration env file
# NOTE the triple store URL provided in the agent.env.test files are the URL to access blazegraph container WITHIN the docker stack
# If the blazegraph is accessed from outside the docker stack, i.e. accessed by agent instance created in memory for localised agent test
# Then the "host.docker.internal" should be replaced with "localhost", which can be done by host_docker_internal_to_localhost function
# For external blazegraph deployed on a server, e.g. http://www.theworldavatar.com/blazegraph, the exact URL should be provided in the agent.env.test file
EXAMPLEAGENT_ENV = os.path.join(THIS_DIR,'agent.env.test')

DERIVATION_INSTANCE_BASE_URL = config_example_agent(EXAMPLEAGENT_ENV).DERIVATION_INSTANCE_BASE_URL

# IRIs of derivation inputs, should be the same as the ones in test_triples/example_abox.ttl
TEST_TRIPLES_BASE_IRI = 'http://www.example.org/derivationagentpythonexample/data/'
MAXVALUE_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'maxValue'
MINVALUE_INSTANCE_IRI = TEST_TRIPLES_BASE_IRI + 'minValue'
DERIVATION_INPUTS = [MAXVALUE_INSTANCE_IRI, MINVALUE_INSTANCE_IRI]


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
def initialise_clients(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ExampleSparqlClient(
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


# ----------------------------------------------------------------------------------
# Module-scoped test fixtures
# ----------------------------------------------------------------------------------

@pytest.fixture(scope="module")
def create_example_agent():
    def _create_example_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
    ):
        agent_config = config_example_agent(EXAMPLEAGENT_ENV)
        agent = ExampleAgent(
            example_conf_param=agent_config.EXAMPLE_CONF_PARAM,
            register_agent=agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=agent_config.KG_USERNAME,
            kg_password=agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(agent_config.FILE_SERVER_ENDPOINT),
            fs_user=agent_config.FILE_SERVER_USERNAME,
            fs_password=agent_config.FILE_SERVER_PASSWORD,
            # NOTE For agent endpoint, we keep this as it is for now (i.e. start with http://host.docker.internal)
            # As the agent endpoint is not accessed from outside the docker network
            # However, one may need to change this in the agent registration process if synchronous derivations are used
            agent_endpoint=agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            max_thread_monitor_async_derivations=agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=agent_config.EMAIL_SUBJECT_PREFIX+' WSL2',
            email_username=agent_config.EMAIL_USERNAME,
            email_auth_json_path=os.path.join(SECRETS_PATH,'email_auth.json'),
            email_start_end_async_derivations=agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
            app=Flask(__name__),
            logger_name='dev'
        )
        return agent
    return _create_example_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def initialise_triples(sparql_client, derivation_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

	# Upload all relevant example triples provided in the test_triples folder
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)

    # Add timestamp to pure inputs
    for input in DERIVATION_INPUTS:
        derivation_client.addTimeInstance(input)
        derivation_client.updateTimestamp(input)


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


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = f"""
        SELECT ?time WHERE {{
            <{derivation_iri}> <{dm.TIME_HASTIME}>/<{dm.TIME_INTIMEPOSITION}>/<{dm.TIME_NUMERICPOSITION}> ?time .
        }}"""
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])


def get_derivation_outputs(derivation_iri: str, sparql_client):
    query_output = f"""SELECT ?output ?output_type
        WHERE {{
            ?output <{dm.ONTODERIVATION_BELONGSTO}> <{derivation_iri}> .
            ?output a ?output_type .
        }}"""
    response = sparql_client.performQuery(query_output)
    if len(response) == 0:
        return None
    else:
        key = set([x['output_type'] for x in response])
        return {k: [x['output'] for x in response if x['output_type'] == k] for k in key}
