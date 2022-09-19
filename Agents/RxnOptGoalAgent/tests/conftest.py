from testcontainers.core.container import DockerContainer
from testcontainers.compose import DockerCompose
from pathlib import Path
from flask import Flask
import logging
import pkgutil
import pytest
import shutil
import time
import uuid
# import xlwt
import os

from pyderivationagent.conf import config_derivation_agent
from pyderivationagent.kg_operations import PySparqlClient
from chemistry_and_robots.kg_operations import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.kg_operations.dict_and_list as dal
from chemistry_and_robots.data_model import *

from doeagent.agent import DoEAgent
from vtexeagent.agent import VapourtecExecutionAgent
from vtexeagent.conf import config_vapourtec_execution_agent
from hplcpostproagent.agent import HPLCPostProAgent
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.conf import config_vapourtec_agent
from hplcagent.agent import HPLCAgent
from hplcagent.conf import config_hplc_agent

# RxnOptGoal Agent related imports
from rxnoptgoalagent.conf.rxn_opt_goal_agent_conf import config_rxn_opt_goal_agent
from rxnoptgoalagent.agent import RxnOptGoalAgent
from rxnoptgoalagent.data_model import *

try:
    import rxnoptgoaliteragent.tests.conftest as rogi_cf
except ImportError:
    raise ImportError("""If rxnoptgoaliteragent is not installed, then please install it first.
                      Otherwise, it's possible due to tests and tests.* are not packaged in the rxnoptgoaliteragent package.
                      One way to fix this is to change the following line in RxnOptGoalIterAgent/setup.py:
                      ``packages=find_packages(exclude=['tests','tests.*']),``
                      to: ``packages=find_packages(),``.
                      Then reinstall the dev version of rxnoptgoaliteragent via:
                      ``cd <path_to_twa_agents>/Agents/RxnOptGoalIterAgent && python -m pip install -e .``
                      A better way of designing tests across multiple agents will be provided in the future.""")

from rxnoptgoaliteragent.agent import RxnOptGoalIterAgent

logging.getLogger("py4j").setLevel(logging.INFO)
logging.getLogger("numba").setLevel(logging.WARNING)


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR,'env_files')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
TEST_TRIPLES_DIR = os.path.join(THIS_DIR,'test_triples')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'_downloaded_files_for_test')
HPLC_REPORT_LOCAL_TEST_DIR = os.path.join(THIS_DIR,'_generated_hplc_report_for_test')
FCEXP_FILE_DIR = os.path.join(THIS_DIR,'_generated_vapourtec_input_file_for_test')
DOCKER_INTEGRATION_DIR = os.path.join(THIS_DIR,'_for_docker_integration_test')
HTML_TEMPLATE_DIR = os.path.join(os.path.dirname(THIS_DIR), 'templates')

# Raw HPLC report sample data in the test_triples folder
HPLC_REPORT_XLS_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_xls.xls')
HPLC_REPORT_TXT_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_txt.txt')


KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
KG_EXPOSED_PORT = 8080 # specified in docker-compose.test.kg.yml
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"
FS_EXPOSED_PORT = 8080 # specified in docker-compose.test.kg.yml
DOCKER_COMPOSE_TEST_KG = 'docker-compose.test.kg.yml'

# Configuration env files
# NOTE the triple store and file server URL ("localhost") provided in the agent.*.env files are made possible via:
# "extra_hosts: - localhost:host-gateway" in the docker-compose.test.yml
DOE_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.doe.env.test')
VAPOURTEC_EXECUTION_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.execution.env.test')
HPLC_POSTPRO_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.postpro.env.test')
# VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.env.test')
# HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.env.test')
ROG_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.env.test')
ROGI_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.iter.env.test')

LAB1_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.vapourtec.env.test')
LAB1_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.hplc.env.test')
LAB2_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.vapourtec.env.test')
LAB2_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.hplc.env.test')

DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(DOE_AGENT_ENV).DERIVATION_INSTANCE_BASE_URL

DOE_IRI = 'http://example.com/blazegraph/namespace/testlab/doe/DoE_1'
DERIVATION_INPUTS = [DOE_IRI]


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
    if os.path.exists(FCEXP_FILE_DIR):
        shutil.rmtree(FCEXP_FILE_DIR)
    if os.path.exists(HPLC_REPORT_LOCAL_TEST_DIR):
        shutil.rmtree(HPLC_REPORT_LOCAL_TEST_DIR)

    # Create folder for test hplc reports
    if not os.path.exists(HPLC_REPORT_LOCAL_TEST_DIR):
        os.mkdir(HPLC_REPORT_LOCAL_TEST_DIR)
    # Create folder for downloaded files
    if not os.path.exists(DOWNLOADED_DIR):
        os.mkdir(DOWNLOADED_DIR)
    # Create folder for downloaded files
    if not os.path.exists(FCEXP_FILE_DIR):
        os.mkdir(FCEXP_FILE_DIR)


def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)
    if os.path.exists(FCEXP_FILE_DIR):
        shutil.rmtree(FCEXP_FILE_DIR)
    if os.path.exists(HPLC_REPORT_LOCAL_TEST_DIR):
        shutil.rmtree(HPLC_REPORT_LOCAL_TEST_DIR)


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

@pytest.fixture(scope="session")
def retrieve_hplc_report():
    def _retrieve_hplc_report(report_extension, target_folder):
        if report_extension.endswith('xls'):
            report_path_in_folder = HPLC_REPORT_XLS_PATH_IN_FOLDER
            local_file_path = os.path.join(target_folder,f'{str(uuid.uuid4())}.xls')
        elif report_extension.endswith('txt'):
            report_path_in_folder = HPLC_REPORT_TXT_PATH_IN_FOLDER
            local_file_path = os.path.join(target_folder,f'{str(uuid.uuid4())}.txt')
        else:
            raise NotImplementedError("Handling HPLC raw report (%s) in the chemistry_and_robots package is NOT yet supported due to its file extension." % 
                report_extension)

        # copy the report to the target folder
        # this will give the last modified time as the current time
        shutil.copy(report_path_in_folder, local_file_path)
        timestamp_last_modified = os.path.getmtime(local_file_path)

        return local_file_path, timestamp_last_modified
    return _retrieve_hplc_report

@pytest.fixture(scope="module")
def initialise_clients(get_service_url, get_service_auth):
    # Retrieve endpoint and auth for triple store
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    # Retrieve endpoint and auth for file server
    fs_url = get_service_url(FS_SERVICE, url_route=FS_ROUTE)
    fs_user, fs_pwd = get_service_auth(FS_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(
        sparql_endpoint, sparql_endpoint,
        kg_user=sparql_user, kg_password=sparql_pwd,
        fs_url=fs_url, fs_user=fs_user, fs_pwd=fs_pwd
    )

    # Create folder for test hplc reports
    if not os.path.exists(HPLC_REPORT_LOCAL_TEST_DIR):
        os.mkdir(HPLC_REPORT_LOCAL_TEST_DIR)

    # Create folder for downloaded files
    if not os.path.exists(DOWNLOADED_DIR):
        os.mkdir(DOWNLOADED_DIR)

    # Create folder for downloaded files
    if not os.path.exists(FCEXP_FILE_DIR):
        os.mkdir(FCEXP_FILE_DIR)

    # Clear triple store before any usage
    sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

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
def initialise_triple_store():
    # NOTE: requires access to the docker.cmclinnovations.com registry from the machine the test is run on.
    # For more information regarding the registry, see: https://github.com/cambridge-cares/TheWorldAvatar/wiki/Docker%3A-Image-registry
    blazegraph = DockerContainer('docker.cmclinnovations.com/blazegraph_for_tests:1.0.0')
    # the port is set as 9999 to match with the value set in the docker image
    blazegraph.with_exposed_ports(9999)
    yield blazegraph


@pytest.fixture(scope="module")
def initialise_test_triples(initialise_triple_store):
    with initialise_triple_store as container:
        # Wait some arbitrary time until container is reachable
        time.sleep(8)

        # Retrieve SPARQL endpoint
        endpoint = get_endpoint(container)
        print(f"SPARQL endpoint: {endpoint}")

        # Create SparqlClient for testing
        sparql_client = PySparqlClient(endpoint, endpoint)

        # Clear triple store before any usage
        sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

        # TODO delete below
        # # Create DerivationClient for creating derivation instances
        # derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        #     sparql_client.kg_client,
        #     DERIVATION_INSTANCE_BASE_URL
        # )

        initialise_triples(sparql_client)

        yield endpoint

        # Clear logger at the end of the test
        clear_loggers()


@pytest.fixture(scope="module")
def initialise_blazegraph_and_fileserver():
    docker_compose = DockerCompose(THIS_DIR, DOCKER_COMPOSE_TEST_KG, pull=True)
    with docker_compose as containers:
        bg_host = containers.get_service_host(KG_SERVICE, KG_EXPOSED_PORT)
        bg_port = containers.get_service_port(KG_SERVICE, KG_EXPOSED_PORT)
        fs_host = containers.get_service_host(FS_SERVICE, FS_EXPOSED_PORT)
        fs_port = containers.get_service_port(FS_SERVICE, FS_EXPOSED_PORT)
        bg_url = f'http://{bg_host}:{bg_port}/{KG_ROUTE}'
        fs_url = f'http://{fs_host}:{fs_port}/{FS_ROUTE}'
        yield bg_url, fs_url


@pytest.fixture(scope="module")
def initialise_blazegraph_fileserver_with_test_triples(
    initialise_blazegraph_and_fileserver,
    get_service_auth
):
    bg_url, fs_url = initialise_blazegraph_and_fileserver

    # Wait some arbitrary time until container is reachable
    time.sleep(8)

    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)
    fs_user, fs_pwd = get_service_auth(FS_SERVICE)

    # Create SparqlClient for testing
    sparql_client = ChemistryAndRobotsSparqlClient(
        query_endpoint=bg_url,
        update_endpoint=bg_url,
        kg_user=sparql_user,
        kg_password=sparql_pwd,
        fs_url=fs_url,
        fs_user=fs_user,
        fs_pwd=fs_pwd,
    )

    # Clear triple store before any usage
    sparql_client.performUpdate("DELETE WHERE {?s ?p ?o.}")

    initialise_triples(sparql_client)

    # Create DerivationClient for creating derivation instances
    derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
        sparql_client.kg_client,
        DERIVATION_INSTANCE_BASE_URL
    )

    yield sparql_client, derivation_client

    # Clear logger at the end of the test
    clear_loggers()


@pytest.fixture(scope="module")
def create_rog_agent():
    def _create_rog_agent(
        goal_iter_agent_iri:str=None,
        kg_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
        kg_update_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
        # NOTE: it is assumed that the KG test container doesn't has authentication
        # also, we assume that we don't need file server for testing with the KG test container
    ):
        rog_agent_config = config_rxn_opt_goal_agent(ROG_AGENT_ENV)
        rog_agent = RxnOptGoalAgent(
            goal_agent_iri=rog_agent_config.GOAL_ONTOAGENT_SERVICE_IRI,
            goal_agent_endpoint=rog_agent_config.GOAL_ONTOAGENT_OPERATION_HTTP_URL,
            goal_monitor_time_interval=rog_agent_config.GOAL_MONITOR_PERIODIC_TIMESCALE,
            goal_iter_agent_iri=rog_agent_config.GOAL_ITER_AGENT_IRI if not goal_iter_agent_iri else goal_iter_agent_iri,
            derivation_instance_base_url=rog_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=rog_agent_config.SPARQL_QUERY_ENDPOINT if not kg_url else kg_url,
            kg_update_url=rog_agent_config.SPARQL_UPDATE_ENDPOINT if not kg_update_url else kg_update_url,
            kg_user=rog_agent_config.KG_USERNAME if not kg_url else None,
            kg_password=rog_agent_config.KG_PASSWORD if not kg_url else None,
            fs_url=rog_agent_config.FILE_SERVER_ENDPOINT if not kg_url else None,
            fs_user=rog_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
            fs_password=rog_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
            app=Flask(__name__, template_folder=HTML_TEMPLATE_DIR)
        )
        return rog_agent
    return _create_rog_agent

@pytest.fixture(scope="module")
def create_rogi_agent():
    def _create_rogi_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
        kg_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
        kg_update_url:str=None, # if none, use value in env file; else use value provided (url of KG test container)
        # NOTE: it is assumed that the KG test container doesn't has authentication
        # also, we assume that we don't need file server for testing with the KG test container
    ):
        rogi_agent_config = config_derivation_agent(ROGI_AGENT_ENV)
        rogi_agent = RxnOptGoalIterAgent(
            register_agent=rogi_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=rogi_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=rogi_agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=rogi_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=rogi_agent_config.SPARQL_QUERY_ENDPOINT if not kg_url else kg_url,
            kg_update_url=rogi_agent_config.SPARQL_UPDATE_ENDPOINT if not kg_update_url else kg_update_url,
            kg_user=rogi_agent_config.KG_USERNAME if not kg_url else None,
            kg_password=rogi_agent_config.KG_PASSWORD if not kg_url else None,
            fs_url=rogi_agent_config.FILE_SERVER_ENDPOINT if not kg_url else None,
            fs_user=rogi_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
            fs_password=rogi_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
            agent_endpoint=rogi_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
        )
        return rogi_agent
    return _create_rogi_agent

@pytest.fixture(scope="module")
def create_doe_agent():
    def _create_doe_agent(
        register_agent:bool=False,
        random_agent_iri:bool=False,
    ):
        doe_agent_config = config_derivation_agent(DOE_AGENT_ENV)
        doe_agent = DoEAgent(
            register_agent=doe_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=doe_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=doe_agent_config.DERIVATION_PERIODIC_TIMESCALE,
            derivation_instance_base_url=doe_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=doe_agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=doe_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=doe_agent_config.KG_USERNAME,
            kg_password=doe_agent_config.KG_PASSWORD,
            fs_url=doe_agent_config.FILE_SERVER_ENDPOINT,
            fs_user=doe_agent_config.FILE_SERVER_USERNAME,
            fs_password=doe_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=doe_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__)
        )
        return doe_agent
    return _create_doe_agent

@pytest.fixture(scope="module")
def create_vapourtec_execution_agent():
    def _create_vapourtec_execution_agent(
        maximum_concurrent_experiment:int=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        vapourtec_execution_agent_config = config_vapourtec_execution_agent(VAPOURTEC_EXECUTION_AGENT_ENV)
        vapourtec_execution_agent = VapourtecExecutionAgent(
            maximum_concurrent_experiment=vapourtec_execution_agent_config.MAXIMUM_CONCURRENT_EXPERIMENT if maximum_concurrent_experiment is None else maximum_concurrent_experiment,
            register_agent=vapourtec_execution_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=vapourtec_execution_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=vapourtec_execution_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=vapourtec_execution_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=vapourtec_execution_agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=vapourtec_execution_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=vapourtec_execution_agent_config.KG_USERNAME,
            kg_password=vapourtec_execution_agent_config.KG_PASSWORD,
            fs_url=vapourtec_execution_agent_config.FILE_SERVER_ENDPOINT,
            fs_user=vapourtec_execution_agent_config.FILE_SERVER_USERNAME,
            fs_password=vapourtec_execution_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=vapourtec_execution_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
        )
        return vapourtec_execution_agent
    return _create_vapourtec_execution_agent

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
            kg_url=hplc_postpro_agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=hplc_postpro_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=hplc_postpro_agent_config.KG_USERNAME,
            kg_password=hplc_postpro_agent_config.KG_PASSWORD,
            fs_url=hplc_postpro_agent_config.FILE_SERVER_ENDPOINT,
            fs_user=hplc_postpro_agent_config.FILE_SERVER_USERNAME,
            fs_password=hplc_postpro_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=hplc_postpro_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
        )
        return hplc_postpro_agent
    return _create_hplc_postpro_agent

@pytest.fixture(scope="module")
def create_vapourtec_agent():
    def _create_vapourtec_agent(
        env_file:str,
        vapourtec_digital_twin:str=None,
        vapourtec_state_periodic_timescale:int=None,
        fcexp_file_container_folder:str=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        vapourtec_agent_config = config_vapourtec_agent(env_file)
        vapourtec_agent = VapourtecAgent(
            vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN if vapourtec_digital_twin is None else vapourtec_digital_twin,
            vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE if vapourtec_state_periodic_timescale is None else vapourtec_state_periodic_timescale,
            vapourtec_ip_address=vapourtec_agent_config.VAPOURTEC_IP_ADDRESS,
            fcexp_file_container_folder=vapourtec_agent_config.FCEXP_FILE_CONTAINER_FOLDER if fcexp_file_container_folder is None else fcexp_file_container_folder,
            register_agent=vapourtec_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=vapourtec_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=vapourtec_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=vapourtec_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=vapourtec_agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=vapourtec_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=vapourtec_agent_config.KG_USERNAME,
            kg_password=vapourtec_agent_config.KG_PASSWORD,
            fs_url=vapourtec_agent_config.FILE_SERVER_ENDPOINT,
            fs_user=vapourtec_agent_config.FILE_SERVER_USERNAME,
            fs_password=vapourtec_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=vapourtec_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
        )
        return vapourtec_agent
    return _create_vapourtec_agent

@pytest.fixture(scope="module")
def create_hplc_agent():
    def _create_hplc_agent(
        env_file:str,
        hplc_digital_twin:str=None,
        hplc_report_periodic_timescale:int=None,
        hplc_report_container_dir:str=None,
        hplc_report_file_extension:str=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        hplc_agent_config = config_hplc_agent(env_file)
        hplc_agent = HPLCAgent(
            hplc_digital_twin=hplc_agent_config.HPLC_DIGITAL_TWIN if hplc_digital_twin is None else hplc_digital_twin,
            hplc_report_periodic_timescale=hplc_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE if hplc_report_periodic_timescale is None else hplc_report_periodic_timescale,
            hplc_report_container_dir=hplc_agent_config.HPLC_REPORT_CONTAINER_DIR if hplc_report_container_dir is None else hplc_report_container_dir,
            current_hplc_method=hplc_agent_config.CURRENT_HPLC_METHOD,
            hplc_report_file_extension=hplc_agent_config.HPLC_REPORT_FILE_EXTENSION if hplc_report_file_extension is None else hplc_report_file_extension,
            register_agent=hplc_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=hplc_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=hplc_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=hplc_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=hplc_agent_config.SPARQL_QUERY_ENDPOINT,
            kg_update_url=hplc_agent_config.SPARQL_UPDATE_ENDPOINT,
            kg_user=hplc_agent_config.KG_USERNAME,
            kg_password=hplc_agent_config.KG_PASSWORD,
            fs_url=hplc_agent_config.FILE_SERVER_ENDPOINT,
            fs_user=hplc_agent_config.FILE_SERVER_USERNAME,
            fs_password=hplc_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=hplc_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
        )
        return hplc_agent
    return _create_hplc_agent

# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])


def initialise_triples(sparql_client):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Upload the example triples for testing
    pathlist = Path(TEST_TRIPLES_DIR).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)

    # Upload all relevant example triples provided in the test_triples folder of 'rxnoptgoaliteragent' package to triple store
    for f in ['goal_iter.ttl', 'plan_step_agent.ttl']:
        data = pkgutil.get_data('rxnoptgoaliteragent', 'tests/test_triples/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        sparql_client.uploadGraph(g)

    # TODO delete below
    # # Add timestamp to pure inputs
    # for input in derivation_inputs:
    #     derivation_client.addTimeInstance(input)
    #     derivation_client.updateTimestamp(input)



# def initialise_triples(sparql_client):
#     # Delete all triples before initialising prepared triples
#     sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

# 	# Upload all relevant example triples provided in the resources folder of 'chemistry_and_robots' package to triple store
#     # NOTE 'sample_data/duplicate_ontorxn.ttl' is for adding <OntoReaction:ReactionVariation> <rdfs:subClassOf> <OntoReaction:ReactionExperiment>.
#     # So that the VapourtecExecution Derivation will be connected to the new instance of OntoReaction:ReactionVariation when DoEAgent cleaning up the Finished status
#     # of the DoE Derivation. This is a workaround for the current way of uploading ontology TBox to the triple store. (We don't upload at all!!!)
#     for f in [
#         'sample_data/rxn_data.ttl',
#         'sample_data/new_exp_data.ttl',
#         'sample_data/dummy_lab.ttl',
#         'sample_data/duplicate_ontorxn.ttl',
#         'sample_data/doe_template.ttl',
#     ]:
#         data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
#         g = Graph().parse(data=data)
#         sparql_client.uploadGraph(g)

#     # Upload all relevant example triples provided in the test_triples folder of 'rxnoptgoaliteragent' package to triple store
#     for f in ['goal_iter.ttl', 'plan_step_agent.ttl']:
#         data = pkgutil.get_data('rxnoptgoaliteragent', 'tests/test_triples/'+f).decode("utf-8")
#         g = Graph().parse(data=data)
#         sparql_client.uploadGraph(g)


def get_endpoint(docker_container):
    # Retrieve SPARQL endpoint for temporary testcontainer
    # endpoint acts as both Query and Update endpoint
    endpoint = 'http://' + docker_container.get_container_host_ip().replace('localnpipe', 'localhost') + ':' \
               + docker_container.get_exposed_port(9999)
    # 'kb' is default namespace in Blazegraph
    endpoint += '/blazegraph/namespace/kb/sparql'
    return endpoint


# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
# and https://github.com/pytest-dev/pytest/issues/5502#issuecomment-1190557648
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        if not hasattr(logger, "handlers"):
            continue
        for handler in logger.handlers[:]:
            logger.removeHandler(handler)


# ----------------------------------------------------------------------------------
# Sample data
# ----------------------------------------------------------------------------------
sample_goal_request = {
    "chem_rxn": "https://www.example.com/triplestore/ontorxn/ChemRxn_1/ChemRxn_1",
    "cycleAllowance": 6,
    "deadline": "2022-09-12T17:05",
    "first_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#Conversion",
    "first_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresGreaterThan",
    "first_goal_num_val": 20,
    "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
    "rxn_opt_goal_plan": "http://www.theworldavatar.com/resource/plans/RxnOpt/rxnoptplan",
    "second_goal_clz": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontoreaction/OntoReaction.owl#Yield",
    "second_goal_desires": "https://raw.githubusercontent.com/cambridge-cares/TheWorldAvatar/main/JPS_Ontology/ontology/ontogoal/OntoGoal.owl#desiresLessThan",
    "second_goal_num_val": 30,
    "second_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent"
}
