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
from chemistry_and_robots.kg_operations import ChemistryAndRobotsSparqlClient
from chemistry_and_robots.data_model import *

from doeagent.agent import DoEAgent
from vtexeagent.agent import VapourtecExecutionAgent
from vtexeagent.conf import config_vapourtec_execution_agent
from hplcpostproagent.agent import HPLCPostProAgent
from vapourtecagent.agent import VapourtecAgent
from vapourtecagent.conf import config_vapourtec_agent
from hplcagent.agent import HPLCAgent
from hplcagent.conf import config_hplc_agent


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

# Raw HPLC report sample data in the test_triples folder
HPLC_REPORT_XLS_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_xls.xls')
HPLC_REPORT_TXT_PATH_IN_FOLDER = os.path.join(TEST_TRIPLES_DIR,'raw_hplc_report_txt.txt')


KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"


# Configuration env files
# NOTE the triple store and file server URL ("localhost") provided in the agent.*.env files are made possible via:
# "extra_hosts: - localhost:host-gateway" in the docker-compose.test.yml
DOE_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.doe.env.test')
VAPOURTEC_EXECUTION_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.execution.env.test')
HPLC_POSTPRO_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.postpro.env.test')
VAPOURTEC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.vapourtec.env.test')
HPLC_AGENT_ENV = os.path.join(ENV_FILES_DIR,'agent.hplc.env.test')


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

# @pytest.fixture(scope="session")
# def create_test_report():
#     def _create_test_report(filename_extension, docker_integration:bool=False):
#         if filename_extension == XLSFILE_EXTENSION:
#             file_path = create_hplc_xls_report(docker_integration)
#         elif filename_extension == TXTFILE_EXTENSION:
#             file_path = create_hplc_txt_report(docker_integration)
#         else:
#             raise NotImplementedError("HPLC raw report with a filename extension (%s) is NOT yet supported." % filename_extension)

#         return file_path
#     return _create_test_report

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

        with open(local_file_path, 'w') as outfile, open(report_path_in_folder, 'r', encoding='utf-8') as infile:
            for line in infile:
                outfile.write(line)
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
        hplc_digital_twin:str=None,
        hplc_report_periodic_timescale:int=None,
        hplc_report_container_dir:str=None,
        hplc_report_file_extension:str=None,
        register_agent:bool=False,
        random_agent_iri:bool=False,
        derivation_periodic_timescale:int=None,
    ):
        hplc_agent_config = config_hplc_agent(HPLC_AGENT_ENV)
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
# def create_hplc_xls_report(docker_integration:bool=False):
#     if docker_integration:
#         file_path = os.path.join(DOCKER_INTEGRATION_DIR,f'{str(uuid.uuid4())}.xls')
#     else:
#         file_path = os.path.join(HPLC_REPORT_LOCAL_TEST_DIR,f'{str(uuid.uuid4())}.xls')
#     if not os.path.exists(file_path):
#         wb = xlwt.Workbook()
#         ws = wb.add_sheet("Test Sheet")
#         for i in range(0,10):
#             for j in range(0,10):
#                 ws.write(i,j,"Placeholder")
#         wb.save(file_path)
#     return file_path

# def create_hplc_txt_report(docker_integration:bool=False):
#     if docker_integration:
#         file_path = os.path.join(DOCKER_INTEGRATION_DIR,f'{str(uuid.uuid4())}.txt')
#     else:
#         file_path = os.path.join(HPLC_REPORT_LOCAL_TEST_DIR,f'{str(uuid.uuid4())}.txt')
#     if not os.path.exists(file_path):
#         with open(file_path, "w") as file:
#             file.truncate(10 ** 3)
#     return file_path

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
