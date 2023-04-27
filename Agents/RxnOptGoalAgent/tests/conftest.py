from testcontainers.core.container import DockerContainer
from testcontainers.compose import DockerCompose
from pathlib import Path
from flask import Flask
from enum import Enum
import requests
import logging
import filecmp
import pytest
import shutil
import time
import uuid
import os

from pyderivationagent import config_derivation_agent
from pyderivationagent import PySparqlClient
from pyderivationagent import PyDerivationClient
from chemistry_and_robots.kg_operations import ChemistryAndRobotsSparqlClient
import chemistry_and_robots.kg_operations.dict_and_list as dal
from chemistry_and_robots.data_model import *

from doeagent.agent import DoEAgent
from vapourtecscheduleagent.agent import VapourtecScheduleAgent
from hplcpostproagent.agent import HPLCPostProAgent
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.conf import config_vapourtec_agent
from hplcagent.agent import HPLCAgent
from hplcagent.conf import config_hplc_agent
from rxnoptgoaliteragent.agent import RxnOptGoalIterAgent

# RxnOptGoal Agent related imports
from rxnoptgoalagent.conf.rxn_opt_goal_agent_conf import config_rxn_opt_goal_agent
from rxnoptgoalagent.agent import RxnOptGoalAgent
from rxnoptgoalagent.data_model import *
from rxnoptgoalagent.kg_operations import RxnOptGoalSparqlClient


logging.getLogger("numba").setLevel(logging.WARNING)


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
ENV_FILES_DIR = os.path.join(THIS_DIR,'env_files')
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
TEST_TRIPLES_DIR = os.path.join(THIS_DIR,'test_triples')
TEST_TRIPLES_SUZUKI_STEP1_DIR = os.path.join(THIS_DIR,'test_triples_suzuki_step1')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'_downloaded_files_for_test')
HPLC_REPORT_LOCAL_TEST_DIR = os.path.join(THIS_DIR,'_generated_hplc_report_for_test')
FCEXP_FILE_DIR = os.path.join(THIS_DIR,'_generated_vapourtec_input_file_for_test')
DOCKER_INTEGRATION_DIR = os.path.join(THIS_DIR,'_for_docker_integration_test')
HTML_TEMPLATE_DIR = os.path.join(os.path.dirname(THIS_DIR), 'templates')
EMAIL_AUTH_JSON_PATH = os.path.join(SECRETS_PATH, 'email_auth.json')
LAB1_DIR = os.path.join(THIS_DIR, '_lab1')
LAB2_DIR = os.path.join(THIS_DIR, '_lab2')

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
VAPOURTEC_SCHEDULE_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.schedule.env.test')
HPLC_POSTPRO_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.postpro.env.test')
ROG_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.env.test')
ROGI_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.goal.iter.env.test')

LAB1_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.vapourtec.env.test')
LAB1_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab1.hplc.env.test')
LAB2_VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.vapourtec.env.test')
LAB2_HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.lab2.hplc.env.test')

DERIVATION_INSTANCE_BASE_URL = config_derivation_agent(DOE_AGENT_ENV).DERIVATION_INSTANCE_BASE_URL


class IRIs(Enum):
    GOAL_ITER_BASE_IRI = 'https://www.example.com/triplestore/ontogoal/rxnopt/'
    GOALSET_1 = GOAL_ITER_BASE_IRI + 'GoalSet_1'

    GOAL_1 = GOAL_ITER_BASE_IRI + 'Goal_1'
    DESIRED_QUANTITY_1 = GOAL_ITER_BASE_IRI + 'Desired_Quantity_1'
    DESIRED_QUANTITY_1_TYPE = ONTOREACTION_YIELD
    DESIRED_QUANTITY_MEASURE_1 = GOAL_ITER_BASE_IRI + 'Desired_Quantity_Measure_1'
    DESIRED_QUANTITY_MEASURE_1_UNIT = OM_PERCENT
    DESIRED_QUANTITY_MEASURE_1_NUMVAL = 99.9

    GOAL_2 = GOAL_ITER_BASE_IRI + 'Goal_2'
    DESIRED_QUANTITY_2 = GOAL_ITER_BASE_IRI + 'Desired_Quantity_2'
    DESIRED_QUANTITY_2_TYPE = ONTOREACTION_RUNMATERIALCOST
    DESIRED_QUANTITY_MEASURE_2 = GOAL_ITER_BASE_IRI + 'Desired_Quantity_Measure_2'
    DESIRED_QUANTITY_MEASURE_2_UNIT = OM_POUNDSTERLINGPERLITRE
    DESIRED_QUANTITY_MEASURE_2_NUMVAL = 0.01

    RESTRICTION_1 = GOAL_ITER_BASE_IRI + 'Restriction_1'
    RESTRICTION_1_CYCLEALLOWANCE = 5
    RESTRICTION_1_DEADLINE = 4102444800.0

    PLAN_STEP_AGENT_BASE_IRI = 'https://www.theworldavatar.com/kg/plans/RxnOpt/'
    RXN_OPT_PLAN = PLAN_STEP_AGENT_BASE_IRI + 'rxnoptplan'
    STEP_DOE = PLAN_STEP_AGENT_BASE_IRI + 'doe'
    STEP_DOE_AGENT = 'https://www.theworldavatar.com/kg/agents/Service__DoE/Service'
    STEP_SCHEDULEEXE = PLAN_STEP_AGENT_BASE_IRI + 'schedule_exe'
    STEP_SCHEDULE_AGENT = 'https://www.theworldavatar.com/kg/agents/Service__VapourtecSchedule/Service'
    STEP_POSTPRO = PLAN_STEP_AGENT_BASE_IRI + 'postpro'
    STEP_POSTPRO_AGENT = 'https://www.theworldavatar.com/kg/agents/Service__HPLC_PostPro/Service'

    CHEMICAL_REACTION_IRI = 'https://www.theworldavatar.com/kg/lab_auto/chem_rxn/ChemRxn_1'

    DERIVATION_INPUTS = [GOALSET_1, CHEMICAL_REACTION_IRI]

    DERIVATION_INPUTS_NO_PRIOR_DATA = [GOALSET_1, CHEMICAL_REACTION_IRI]

    LAB1_IRI = 'https://www.theworldavatar.com/kg/lab_auto/lab1/Laboratory_Dummy'
    LAB2_IRI = 'https://www.theworldavatar.com/kg/lab_auto/lab2/Laboratory_Dummy'
    VAPOURTEC_ENV_FILE_DICT = {LAB1_IRI: LAB1_VAPOURTEC_AGENT_ENV, LAB2_IRI: LAB2_VAPOURTEC_AGENT_ENV}
    HPLC_ENV_FILE_DICT = {LAB1_IRI: LAB1_HPLC_AGENT_ENV, LAB2_IRI: LAB2_HPLC_AGENT_ENV}

    HPLC_METHOD_SUZUKI = 'https://www.theworldavatar.com/kg/lab_auto/suzuki/HPLCMethod_Suzuki'

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
    blazegraph = DockerContainer('ghcr.io/cambridge-cares/blazegraph_for_tests:1.0.0')
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
    get_service_auth,
):
    def _initialise_blazegraph_fileserver_with_test_triples(
        triples_dir: str = TEST_TRIPLES_DIR
    ):
        bg_url, fs_url = initialise_blazegraph_and_fileserver

        # Wait some arbitrary time until container is reachable
        time.sleep(8)

        sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)
        fs_user, fs_pwd = get_service_auth(FS_SERVICE)

        # Create SparqlClient for testing
        sparql_client = RxnOptGoalSparqlClient(
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

        initialise_triples(sparql_client, triples_dir)

        # Create DerivationClient for creating derivation instances
        derivation_client = sparql_client.jpsBaseLib_view.DerivationClient(
            sparql_client.kg_client,
            DERIVATION_INSTANCE_BASE_URL
        )
        return sparql_client, derivation_client
    yield _initialise_blazegraph_fileserver_with_test_triples

    # Clear logger at the end of the test
    clear_loggers()


@pytest.fixture(scope="module")
def initialise_all_dockerised_agent_and_triples(get_service_url, get_service_auth):
    bg_url = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    sparql_user, sparql_pwd = get_service_auth(KG_SERVICE)

    fs_url = get_service_url(FS_SERVICE, url_route=FS_ROUTE)
    fs_user, fs_pwd = get_service_auth(FS_SERVICE)

    # Create SparqlClient for testing
    sparql_client = RxnOptGoalSparqlClient(
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
    derivation_client = PyDerivationClient(
        derivation_instance_base_url=DERIVATION_INSTANCE_BASE_URL,
        query_endpoint=bg_url,
        update_endpoint=bg_url,
        kg_user=sparql_user,
        kg_password=sparql_pwd,
    )

    yield sparql_client, derivation_client


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
            kg_url=host_docker_internal_to_localhost(rog_agent_config.SPARQL_QUERY_ENDPOINT) if not kg_url else kg_url,
            kg_update_url=host_docker_internal_to_localhost(rog_agent_config.SPARQL_UPDATE_ENDPOINT) if not kg_update_url else kg_update_url,
            kg_user=rog_agent_config.KG_USERNAME if not kg_url else None,
            kg_password=rog_agent_config.KG_PASSWORD if not kg_url else None,
            fs_url=host_docker_internal_to_localhost(rog_agent_config.FILE_SERVER_ENDPOINT) if not kg_url else None,
            fs_user=rog_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
            fs_password=rog_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
            app=Flask(__name__, template_folder=HTML_TEMPLATE_DIR),
            email_recipient=rog_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=rog_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=rog_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_goal_iteration_progress=rog_agent_config.EMAIL_GOAL_ITERATION_PROGRESS,
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
            kg_url=host_docker_internal_to_localhost(rogi_agent_config.SPARQL_QUERY_ENDPOINT) if not kg_url else kg_url,
            kg_update_url=host_docker_internal_to_localhost(rogi_agent_config.SPARQL_UPDATE_ENDPOINT) if not kg_update_url else kg_update_url,
            kg_user=rogi_agent_config.KG_USERNAME if not kg_url else None,
            kg_password=rogi_agent_config.KG_PASSWORD if not kg_url else None,
            fs_url=host_docker_internal_to_localhost(rogi_agent_config.FILE_SERVER_ENDPOINT) if not kg_url else None,
            fs_user=rogi_agent_config.FILE_SERVER_USERNAME if not kg_url else None,
            fs_password=rogi_agent_config.FILE_SERVER_PASSWORD if not kg_url else None,
            agent_endpoint=rogi_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            max_thread_monitor_async_derivations=rogi_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=rogi_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=rogi_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=rogi_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=rogi_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
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
            kg_url=host_docker_internal_to_localhost(doe_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(doe_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=doe_agent_config.KG_USERNAME,
            kg_password=doe_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(doe_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=doe_agent_config.FILE_SERVER_USERNAME,
            fs_password=doe_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=doe_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            email_recipient=doe_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=doe_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=doe_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=doe_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return doe_agent
    return _create_doe_agent

@pytest.fixture(scope="module")
def create_vapourtec_schedule_agent():
    def _create_vapourtec_schedule_agent(
        maximum_concurrent_experiment:int=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        vapourtec_schedule_agent_config = config_derivation_agent(VAPOURTEC_SCHEDULE_AGENT_ENV)
        vapourtec_schedule_agent = VapourtecScheduleAgent(
            register_agent=vapourtec_schedule_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=vapourtec_schedule_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=vapourtec_schedule_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=vapourtec_schedule_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(vapourtec_schedule_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(vapourtec_schedule_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=vapourtec_schedule_agent_config.KG_USERNAME,
            kg_password=vapourtec_schedule_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(vapourtec_schedule_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=vapourtec_schedule_agent_config.FILE_SERVER_USERNAME,
            fs_password=vapourtec_schedule_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=vapourtec_schedule_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            max_thread_monitor_async_derivations=vapourtec_schedule_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS if maximum_concurrent_experiment is None else maximum_concurrent_experiment,
            email_recipient=vapourtec_schedule_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=vapourtec_schedule_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=vapourtec_schedule_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=vapourtec_schedule_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return vapourtec_schedule_agent
    return _create_vapourtec_schedule_agent

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
            email_recipient=hplc_postpro_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=hplc_postpro_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=hplc_postpro_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=hplc_postpro_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return hplc_postpro_agent
    return _create_hplc_postpro_agent

@pytest.fixture(scope="module")
def create_vapourtec_agent():
    def _create_vapourtec_agent(
        env_file:str,
        vapourtec_digital_twin:str=None,
        vapourtec_state_periodic_timescale:int=None,
        vapourtec_ip_address:str=None,
        fcexp_file_container_folder:str=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
        dry_run:bool=True,
    ):
        vapourtec_agent_config = config_vapourtec_agent(env_file)
        vapourtec_agent = VapourtecAgent(
            dry_run=dry_run,
            vapourtec_digital_twin=vapourtec_agent_config.VAPOURTEC_DIGITAL_TWIN if vapourtec_digital_twin is None else vapourtec_digital_twin,
            vapourtec_state_periodic_timescale=vapourtec_agent_config.VAPOURTEC_STATE_PERIODIC_TIMESCALE if vapourtec_state_periodic_timescale is None else vapourtec_state_periodic_timescale,
            vapourtec_ip_address=vapourtec_agent_config.VAPOURTEC_IP_ADDRESS if vapourtec_ip_address is None else vapourtec_ip_address,
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
            email_recipient=vapourtec_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=vapourtec_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=vapourtec_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=vapourtec_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
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
        dry_run:bool=True,
        hplc_method:str=None,
    ):
        hplc_agent_config = config_hplc_agent(env_file)
        hplc_agent = HPLCAgent(
            dry_run=dry_run,
            hplc_digital_twin=hplc_agent_config.HPLC_DIGITAL_TWIN if hplc_digital_twin is None else hplc_digital_twin,
            hplc_report_periodic_timescale=hplc_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE if hplc_report_periodic_timescale is None else hplc_report_periodic_timescale,
            hplc_report_container_dir=hplc_agent_config.HPLC_REPORT_CONTAINER_DIR if hplc_report_container_dir is None else hplc_report_container_dir,
            current_hplc_method=hplc_agent_config.CURRENT_HPLC_METHOD if not hplc_method else hplc_method,
            hplc_report_file_extension=hplc_agent_config.HPLC_REPORT_FILE_EXTENSION if hplc_report_file_extension is None else hplc_report_file_extension,
            register_agent=hplc_agent_config.REGISTER_AGENT if not register_agent else register_agent,
            agent_iri=hplc_agent_config.ONTOAGENT_SERVICE_IRI if not random_agent_iri else 'http://agent_' + str(uuid.uuid4()),
            time_interval=hplc_agent_config.DERIVATION_PERIODIC_TIMESCALE if derivation_periodic_timescale is None else derivation_periodic_timescale,
            derivation_instance_base_url=hplc_agent_config.DERIVATION_INSTANCE_BASE_URL,
            kg_url=host_docker_internal_to_localhost(hplc_agent_config.SPARQL_QUERY_ENDPOINT),
            kg_update_url=host_docker_internal_to_localhost(hplc_agent_config.SPARQL_UPDATE_ENDPOINT),
            kg_user=hplc_agent_config.KG_USERNAME,
            kg_password=hplc_agent_config.KG_PASSWORD,
            fs_url=host_docker_internal_to_localhost(hplc_agent_config.FILE_SERVER_ENDPOINT),
            fs_user=hplc_agent_config.FILE_SERVER_USERNAME,
            fs_password=hplc_agent_config.FILE_SERVER_PASSWORD,
            agent_endpoint=hplc_agent_config.ONTOAGENT_OPERATION_HTTP_URL,
            app=Flask(__name__),
            email_recipient=hplc_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=hplc_agent_config.EMAIL_SUBJECT_PREFIX + 'WSL2',
            email_username=hplc_agent_config.EMAIL_USERNAME,
            email_auth_json_path=EMAIL_AUTH_JSON_PATH,
            email_start_end_async_derivations=hplc_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return hplc_agent
    return _create_hplc_agent

# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------

def host_docker_internal_to_localhost(endpoint: str):
    return endpoint.replace("host.docker.internal:", "localhost:")


def get_timestamp(derivation_iri: str, sparql_client):
    query_timestamp = """SELECT ?time WHERE { <%s> <%s>/<%s>/<%s> ?time .}""" % (
        derivation_iri, TIME_HASTIME, TIME_INTIMEPOSITION, TIME_NUMERICPOSITION)
    # the queried results must be converted to int, otherwise it will not be comparable
    return int(sparql_client.performQuery(query_timestamp)[0]['time'])


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


def initialise_triples(sparql_client, triples_dir: str = TEST_TRIPLES_DIR):
    # Delete all triples before initialising prepared triples
    sparql_client.performUpdate("""DELETE WHERE {?s ?p ?o.}""")

    # Upload tbox
    sparql_client.upload_ontology_tbox(ONTODOE)
    sparql_client.upload_ontology_tbox(ONTOREACTION)

    # Upload the example triples for testing
    pathlist = Path(triples_dir).glob('*.ttl')
    for path in pathlist:
        g = Graph()
        g.parse(str(path), format='turtle')
        sparql_client.uploadGraph(g)


def assert_rxn_iterations(
    sparql_client: RxnOptGoalSparqlClient,
    doe_agent: DoEAgent,
    vapourtec_schedule_agent: VapourtecScheduleAgent,
    vapourtec_agent_lst: Union[VapourtecAgent, List[VapourtecAgent]],
    hplc_agent_lst: Union[HPLCAgent, List[HPLCAgent]],
    hplc_postpro_agent: HPLCPostProAgent,
    rogi_agent: RxnOptGoalIterAgent,
    goal_set_iri: str,
    dockerised_test: bool = False,
):
    if not isinstance(vapourtec_agent_lst, list):
        vapourtec_agent_lst = [vapourtec_agent_lst]
    if not isinstance(hplc_agent_lst, list):
        hplc_agent_lst = [hplc_agent_lst]

    # Check if the vapourtec agent is registered with the vapourtec hardware
    for vapourtec_agent in vapourtec_agent_lst:
        assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
            vapourtec_agent.vapourtec_digital_twin, ONTOLAB_ISMANAGEDBY, vapourtec_agent.agentIRI
        ))[0]['ASK']

    for hplc_agent in hplc_agent_lst:
        # Check if the hplc agent is registered with the hplc hardware
        assert sparql_client.performQuery("ASK {<%s> <%s> <%s>.}" % (
            hplc_agent.hplc_digital_twin, ONTOLAB_ISMANAGEDBY, hplc_agent.agentIRI
        ))[0]['ASK']

    #################################
    ## Obtain relevant information ##
    #################################
    # First get the goal set
    goal_set = sparql_client.get_goal_set_instance(goal_set_iri)
    # Then get all reaction experiment associated with the goal set
    rogi_derivation_lst = sparql_client.get_rogi_derivations_of_goal_set(
        goal_set_iri=goal_set_iri,
        rogi_agent_iri=rogi_agent.agentIRI,
    )
    assert len(rogi_derivation_lst) == len(vapourtec_agent_lst)
    rogi_derivation = rogi_derivation_lst[0]
    rxn_exp_iri_lst = sparql_client.get_all_rxn_exp_of_goal_set(goal_set_iri)
    assert rxn_exp_iri_lst is not None

    # Iterate through all reaction experiments and check for each one of them
    for rxn_exp_iri in rxn_exp_iri_lst:
        doe_query_response = sparql_client.performQuery(f"SELECT ?doe WHERE {{?doe <{ONTODOE_PROPOSESNEWEXPERIMENT}> <{rxn_exp_iri}>.}}")
        assert len(doe_query_response) == 1
        doe_iri = doe_query_response[0]['doe']

        ##########################
        ## Check DoE Derivation ##
        ##########################
        # Check if the DoE derivation is processed and generated the desired triples
        # Query the iri of the new proposed NewExperiment
        new_doe_instance = sparql_client.get_doe_instance(doe_iri)
        assert new_doe_instance.proposesNewExperiment is not None
        new_exp_instance = new_doe_instance.proposesNewExperiment
        new_exp_iri = new_exp_instance.instance_iri
        print(f"New experiment suggested successfully, suggested experiment instance: {new_exp_iri}")

        # Check if all the suggested conditions are within the DoE range
        for design_variable in new_doe_instance.hasDomain.hasDesignVariable:
            if isinstance(design_variable, ContinuousVariable):
                rxn_cond = new_exp_instance.get_reaction_condition(design_variable.refersToQuantity.clz, design_variable.positionalID)
                assert rxn_cond.hasValue.hasNumericalValue <= design_variable.upperLimit
                assert design_variable.lowerLimit <= rxn_cond.hasValue.hasNumericalValue
            else:
                # TODO add checks for CategoricalVariable
                pass
        # Check if all the fixed parameters are the same as the DoE instance
        if new_doe_instance.hasDomain.hasFixedParameter is not None:
            for fixed_parameter in new_doe_instance.hasDomain.hasFixedParameter:
                rxn_cond = new_exp_instance.get_reaction_condition(fixed_parameter.refersToQuantity.clz, fixed_parameter.positionalID)
                assert rxn_cond.hasValue.hasNumericalValue == fixed_parameter.refersToQuantity.hasValue.hasNumericalValue

        print(f"DoE Derivation checked successfully for reaction experiment {rxn_exp_iri}")

        ################################
        ## Check Vapourtec Derivation ##
        ################################
        ## Check if the derivation is processed and generated the desired triples
        # First, check if the file is generated and uploaded correctly
        vapourtec_derivation = get_vapourtec_derivation(
            new_exp_iri,
            [vapourtec_agent.agentIRI for vapourtec_agent in vapourtec_agent_lst],
            sparql_client
        )
        vapourtec_input_file_iri = get_vapourtec_input_file_iri(vapourtec_derivation, sparql_client)
        vapourtec_input_file = sparql_client.get_vapourtec_input_file(vapourtec_input_file_iri)
        # this is localised agent test, so localFilePath is already host machine path, not docker path
        local_file_path = vapourtec_input_file.localFilePath
        print(f"Uploaded Vapourtec input file localFilePath: {local_file_path}")
        remote_file_path = vapourtec_input_file.remoteFilePath
        print(f"Uploaded Vapourtec input file remoteFilePath: {remote_file_path}")
        # Genereate random download path
        full_downloaded_path = os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+'csv')
        print(f"Downloading file to {full_downloaded_path}")
        # Download the file and make sure all the content are the same
        sparql_client.downloadFile(host_docker_internal_to_localhost(remote_file_path), full_downloaded_path)
        if not dockerised_test:
            assert filecmp.cmp(local_file_path,full_downloaded_path)

        # Second, check if settings were generated for all reaction conditions
        rxn_exp_instance = sparql_client.getReactionExperiment(rxn_exp_iri)[0]
        assert all([condition.translateToParameterSetting is not None for condition in rxn_exp_instance.hasReactionCondition if condition.clz != ONTOREACTION_REACTIONPRESSURE])

        # Third, check there is chemical amount instance
        chemical_amount_iri = get_chemical_amount_iri(vapourtec_derivation, sparql_client)
        assert chemical_amount_iri is not None
        new_rs400_list = sparql_client.get_vapourtec_rs400(
            list_vapourtec_rs400_iri=[vapourtec_agent.vapourtec_digital_twin for vapourtec_agent in vapourtec_agent_lst],
        )
        assert len(new_rs400_list) == len(vapourtec_agent_lst)
        new_rs400 = new_rs400_list[0]
        new_autosampler = new_rs400.get_autosampler()
        new_autosampler_liquid_level = {s.holds.isFilledWith.instance_iri:s.holds.hasFillLevel.hasValue.hasNumericalValue for s in [site for site in new_autosampler.hasSite if site.holds.isFilledWith is not None]}
        # NOTE below is commented out as the reactor outlet is now send to the waste tank
        # assert chemical_amount_iri in new_autosampler_liquid_level
        # assert new_autosampler_liquid_level[chemical_amount_iri] >= 0

        print(f"Vapourtec Derivation checked successfully for reaction experiment {rxn_exp_iri}")

        ###########################
        ## Check HPLC Derivation ##
        ###########################
        ## Check if the derivation is processed and generated the desired triples
        lst_response = get_hplc_job(
            [hplc_agent.hplc_digital_twin for hplc_agent in hplc_agent_lst],
            new_exp_iri,
            chemical_amount_iri,
            sparql_client
        )
        assert len(lst_response) == 1
        lst_hplc_job_iri = dal.get_unique_values_in_list_of_dict(lst_response, 'hplc_job')
        hplc_derivation = get_hplc_derivation(
            new_exp_iri,
            [hplc_agent.agentIRI for hplc_agent in hplc_agent_lst],
            sparql_client
        )
        hplc_derivation_outputs = get_derivation_outputs(hplc_derivation, sparql_client)
        assert len(hplc_derivation_outputs) == 1
        assert lst_hplc_job_iri == hplc_derivation_outputs[ONTOHPLC_HPLCJOB]

        print(f"HPLC Derivation checked successfully for reaction experiment {rxn_exp_iri}")

        #########################################
        ## Check Vapourtec Schedule Derivation ##
        #########################################
        # Check Vapourtec Schedule Derivation
        # (1) reaction experiment should be assigned to a reactor
        lst_done_rxn_exp_instance = sparql_client.getReactionExperiment(new_exp_iri)
        assert len(lst_done_rxn_exp_instance) == 1
        assert lst_done_rxn_exp_instance[0].isAssignedTo is not None
        # (2) HPLCReport instance should added to the derivation outputs
        lst_hplc_report_iri = get_hplc_report_of_hplc_job(lst_hplc_job_iri[0], sparql_client)
        vapourtec_schedule_derivation = get_vapourtec_schedule_derivation(new_exp_iri, vapourtec_schedule_agent.agentIRI, sparql_client)
        vapourtec_schedule_derivation_outputs = get_derivation_outputs(vapourtec_schedule_derivation, sparql_client)
        assert len(lst_hplc_report_iri) == 1
        assert len(vapourtec_schedule_derivation_outputs) == 1
        assert lst_hplc_report_iri == vapourtec_schedule_derivation_outputs[ONTOHPLC_HPLCREPORT]
        # (3) The assigned reactor should be contained in the lab which the Vapourtec Schedule Derivation isDerivedFrom
        assigned_lab_iri = get_lab_as_constriant_of_vapourtec_schedule_derivation(vapourtec_schedule_derivation, sparql_client)
        assert sparql_client.check_if_triple_exist(assigned_lab_iri, ONTOLAB_CONTAINS, lst_done_rxn_exp_instance[0].isAssignedTo)
        print(f"Vapourtec Schedule Derivation checked successfully for reaction experiment {rxn_exp_iri}")

        ##################################
        ## Check HPLCPostPro Derivation ##
        ##################################
        # Query the new derived IRI
        hplc_postpro_derivation = get_hplc_postpro_derivation(lst_hplc_report_iri[0], hplc_postpro_agent.agentIRI, sparql_client)
        hplc_postpro_derivation_outputs = get_derivation_outputs(hplc_postpro_derivation, sparql_client)

        # Reload the ReactionExperiment instance and check all its information (OutputChemical and PerformanceIndicator) are uploaded and parsed correctly
        reload_rxn_rxp_instance = sparql_client.getReactionExperiment(rxn_exp_iri)[0]
        reload_pi_dct = {pi.instance_iri:pi.clz for pi in reload_rxn_rxp_instance.hasPerformanceIndicator}
        assert all([iri in hplc_postpro_derivation_outputs[reload_pi_dct[iri]] for iri in reload_pi_dct])
        for pi in reload_rxn_rxp_instance.hasPerformanceIndicator:
            if pi.hasValue is None:
                assert False, f"PerformanceIndicator {pi.instance_iri} has no value, complete rxn_exp: {str(reload_rxn_rxp_instance.dict())}"
            assert pi.hasValue.hasUnit is not None
            assert pi.hasValue.hasNumericalValue is not None
        reload_output_chemical_lst = reload_rxn_rxp_instance.hasOutputChemical
        for oc in reload_output_chemical_lst:
            assert oc.clz == ONTOREACTION_OUTPUTCHEMICAL
            assert oc.instance_iri is not None
            reload_phase_comp_lst = oc.thermodynamicBehaviour.isComposedOfSubsystem
            for phase_comp in reload_phase_comp_lst:
                assert phase_comp.representsOccurenceOf is not None
                assert phase_comp.hasProperty.hasValue.hasUnitOfMeasure is not None
                assert phase_comp.hasProperty.hasValue.numericalValue is not None
            reload_phase_comp_conc_lst = [pc.hasProperty for pc in oc.thermodynamicBehaviour.isComposedOfSubsystem]
            reload_conc_lst = oc.thermodynamicBehaviour.has_composition.comprisesDirectly
            assert all([conc in reload_phase_comp_conc_lst for conc in reload_conc_lst])
            assert all([conc in reload_conc_lst for conc in reload_phase_comp_conc_lst])

        print(f"HPLCPostPro Derivation checked successfully for reaction experiment {rxn_exp_iri}")


def get_hplc_derivation(rxn_exp_iri: str, hplc_agent_iri_lst: list, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?hplc_derivation
        WHERE {{
            VALUES ?hplc_agent {{ {' '.join([f'<{iri}>' for iri in hplc_agent_iri_lst])} }}
            ?hplc_derivation <{ONTODERIVATION_ISDERIVEDFROM}> <{rxn_exp_iri}>;
                             <{ONTODERIVATION_ISDERIVEDUSING}> ?hplc_agent.
        }}"""
    response = sparql_client.performQuery(query)
    return response[0]['hplc_derivation'] if len(response) > 0 else None


def get_vapourtec_derivation(rxn_exp_iri: str, vapourtec_agent_iri_lst: list, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?vapourtec_derivation
        WHERE {{
            VALUES ?vapourtec_agent {{ {' '.join([f'<{iri}>' for iri in vapourtec_agent_iri_lst])} }}
            ?vapourtec_derivation <{ONTODERIVATION_ISDERIVEDFROM}> <{rxn_exp_iri}>;
                                  <{ONTODERIVATION_ISDERIVEDUSING}> ?vapourtec_agent.
        }}"""
    response = sparql_client.performQuery(query)
    return response[0]['vapourtec_derivation'] if len(response) > 0 else None


def get_vapourtec_schedule_derivation(rxn_exp_iri: str, vapourtec_schedule_agent_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?vapourtec_schedule_derivation
        WHERE {{
            ?vapourtec_schedule_derivation <{ONTODERIVATION_ISDERIVEDFROM}> <{rxn_exp_iri}>;
                                  <{ONTODERIVATION_ISDERIVEDUSING}> <{vapourtec_schedule_agent_iri}>.
        }}"""
    response = sparql_client.performQuery(query)
    return response[0]['vapourtec_schedule_derivation'] if len(response) > 0 else None


def get_hplc_postpro_derivation(hplc_report_iri: str, hplc_postpro_agent_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?hplc_postpro_derivation
        WHERE {{
            ?hplc_postpro_derivation <{ONTODERIVATION_ISDERIVEDFROM}> <{hplc_report_iri}>;
                                     <{ONTODERIVATION_ISDERIVEDUSING}> <{hplc_postpro_agent_iri}>.
        }}"""
    response = sparql_client.performQuery(query)
    return response[0]['hplc_postpro_derivation'] if len(response) > 0 else None


def get_derivation_outputs(derivation_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?derivation_outputs ?derivation_outputs_type
        WHERE {{
            ?derivation_outputs <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}>.
            ?derivation_outputs a ?derivation_outputs_type.
        }}"""
    response = sparql_client.performQuery(query)
    return {
        rdf_type:dal.get_unique_values_in_list_of_dict(
            dal.get_sublist_in_list_of_dict_matching_key_value(
                response, 'derivation_outputs_type', rdf_type
            ),
            'derivation_outputs'
        ) for rdf_type in dal.get_unique_values_in_list_of_dict(response, 'derivation_outputs_type')
    }


def if_hplc_derivation_is_in_progress(derivation_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?status_type
        WHERE {{
            <{derivation_iri}> <{ONTODERIVATION_HASSTATUS}>/a ?status_type.
        }}"""
    response = sparql_client.performQuery(query)
    status_type = response[0]['status_type'] if len(response) > 0 else None
    if status_type == ONTODERIVATION_INPROGRESS:
        return True
    return False


def get_vapourtec_input_file_iri(derivation_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?vapourtec_input_file
        WHERE {{ ?vapourtec_input_file <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}>; a <{ONTOVAPOURTEC_VAPOURTECINPUTFILE}>. }}"""
    response = sparql_client.performQuery(query)
    return response[0]['vapourtec_input_file']


def get_chemical_amount_iri(derivation_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?chemical_amount
        WHERE {{
            ?chemical_amount <{ONTODERIVATION_BELONGSTO}> <{derivation_iri}>; a <{ONTOLAB_CHEMICALAMOUNT}>.
        }}"""
    return sparql_client.performQuery(query)[0]['chemical_amount']


def get_hplc_job(
    hplc_digital_twin_lst,
    rxn_exp_iri,
    chemical_amount_iri,
    sparql_client: PySparqlClient
):
    query = f"""
        SELECT ?hplc_job ?hplc_digital_twin
        WHERE {{
            VALUES ?hplc_digital_twin {{ <{'> <'.join(hplc_digital_twin_lst)}> }}
            ?hplc_job ^<{ONTOHPLC_HASJOB}> ?hplc_digital_twin;
                      a <{ONTOHPLC_HPLCJOB}>;
                      <{ONTOHPLC_CHARACTERISES}> <{rxn_exp_iri}>;
                      <{ONTOHPLC_HASREPORT}>/<{ONTOHPLC_GENERATEDFOR}> <{chemical_amount_iri}>.
        }}"""
    return sparql_client.performQuery(query)


def get_hplc_report_of_hplc_job(hplc_job_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?hplc_report
        WHERE {{ <{hplc_job_iri}> <{ONTOHPLC_HASREPORT}> ?hplc_report. }}"""
    response = sparql_client.performQuery(query)
    return [response[i]['hplc_report'] for i in range(len(response))]


def get_lab_as_constriant_of_vapourtec_schedule_derivation(vapourtec_schedule_derivation_iri: str, sparql_client: PySparqlClient):
    query = f"""
        SELECT ?lab
        WHERE {{
            <{vapourtec_schedule_derivation_iri}> <{ONTODERIVATION_ISDERIVEDFROM}> ?lab.
            ?lab a <{ONTOLAB_LABORATORY}>.
        }}"""
    response = sparql_client.performQuery(query)
    return response[0]['lab'] if len(response) > 0 else None

# ----------------------------------------------------------------------------------
# Sample data
# ----------------------------------------------------------------------------------
sample_goal_request = {
    "chem_rxn": "https://www.theworldavatar.com/kg/lab_auto/chem_rxn/ChemRxn_1",
    "cycleAllowance": 6,
    "deadline": str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat()),
    "first_goal_clz": "https://www.theworldavatar.com/kg/ontoreaction/Yield",
    "first_goal_desires": "https://www.theworldavatar.com/kg/ontogoal/desiresGreaterThan",
    "first_goal_num_val": 99,
    "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
    "rxn_opt_goal_plan": "https://www.theworldavatar.com/kg/plans/RxnOpt/rxnoptplan",
    "second_goal_clz": "https://www.theworldavatar.com/kg/ontoreaction/RunMaterialCost",
    "second_goal_desires": "https://www.theworldavatar.com/kg/ontogoal/desiresLessThan",
    "second_goal_num_val": 0.001,
    "second_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/poundSterlingPerLitre",
    "labs": [
        'https://www.theworldavatar.com/kg/lab_auto/lab1/Laboratory_Dummy',
        'https://www.theworldavatar.com/kg/lab_auto/lab2/Laboratory_Dummy'
    ]
}

suzuki_goal_request = {
    "chem_rxn": "https://www.theworldavatar.com/kg/lab_auto/suzuki/ChemRxn_1",
    "cycleAllowance": 6,
    "deadline": str(datetime.fromtimestamp(int(time.time()) + 2 * 60 * 60).isoformat()),
    "first_goal_clz": "https://www.theworldavatar.com/kg/ontoreaction/Yield",
    "first_goal_desires": "https://www.theworldavatar.com/kg/ontogoal/desiresGreaterThan",
    "first_goal_num_val": 99,
    "first_goal_unit": "http://www.ontology-of-units-of-measure.org/resource/om-2/percent",
    "rxn_opt_goal_plan": "https://www.theworldavatar.com/kg/plans/RxnOpt/rxnoptplan",
    "labs": [
        'https://www.theworldavatar.com/kg/lab_auto/lab1/Laboratory_Dummy',
    ]
}
