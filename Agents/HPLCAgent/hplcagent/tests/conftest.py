# NOTE courtesy of Daniel (dln22), this file is adapted from https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_uploader/tests/conftest.py
from rdflib import URIRef
from rdflib import Graph
from rdflib import RDF
from flask import Flask
import requests
import logging
import pytest
import shutil
import time
import uuid
import xlwt
import os

from hplcagent.kg_operations import ChemistryAndRobotsSparqlClient
from hplcagent.data_model import *
from hplcagent.agent import HPLCAgent
from hplcagent.conf import config_hplc_agent


# ----------------------------------------------------------------------------------
# Constant and configuration
# ----------------------------------------------------------------------------------

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
RESOURCE_DIR = os.path.join(THIS_DIR,'resources')
HPLC_REPORT_DIR = os.path.join(THIS_DIR,'_generated_hplc_report_for_test')
DOCKER_INTEGRATION_DIR = os.path.join(THIS_DIR,'_for_docker_integration_test')
DOWNLOADED_DIR = os.path.join(THIS_DIR,'_downloaded_files_for_test')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"

HPLC_AGENT_ENV = os.path.join(THIS_DIR,'agent.hplc.env.test')


def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(HPLC_REPORT_DIR):
        shutil.rmtree(HPLC_REPORT_DIR)
    if os.path.exists(DOWNLOADED_DIR):
        shutil.rmtree(DOWNLOADED_DIR)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(HPLC_REPORT_DIR):
        shutil.rmtree(HPLC_REPORT_DIR)
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
def create_test_report():
    def _create_test_report(filename_extension, docker_integration:bool=False):
        if filename_extension == XLSFILE_EXTENSION:
            file_path = create_hplc_xls_report(docker_integration)
        elif filename_extension == TXTFILE_EXTENSION:
            file_path = create_hplc_txt_report(docker_integration)
        else:
            raise NotImplementedError("HPLC raw report with a filename extension (%s) is NOT yet supported." % filename_extension)

        return file_path
    return _create_test_report

@pytest.fixture(scope="session")
def generate_random_download_path():
    def _generate_random_download_path(filename_extension):
        return os.path.join(DOWNLOADED_DIR,f'{str(uuid.uuid4())}.'+filename_extension)
    return _generate_random_download_path

@pytest.fixture(scope="session")
def generate_random_hplc_digital_twin():
    def _generate_random_hplc_digital_twin(hplc_report_file_extension):
        return f'http://www.example.com/placeholder/HPLC_{hplc_report_file_extension}_{str(uuid.uuid4())}'
    return _generate_random_hplc_digital_twin

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
    if not os.path.exists(HPLC_REPORT_DIR):
        os.mkdir(HPLC_REPORT_DIR)
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
def initialise_hplc_digital_twin_triples(generate_random_hplc_digital_twin):
    def _initialise_hplc_digital_twin_triples(sparql_client, hplc_report_file_extension, predefined_hplc_digital_twin:str=None):
        if hplc_report_file_extension == 'xls':
            filename_extension = DBPEDIA_XLSFILE
        elif hplc_report_file_extension == 'txt':
            filename_extension = DBPEDIA_TXTFILE
        else:
            raise NotImplementedError(f"HPLC report file extention {hplc_report_file_extension} not supported yet.")

        if predefined_hplc_digital_twin is None:
            hplc_digital_twin = generate_random_hplc_digital_twin(hplc_report_file_extension)
        else:
            hplc_digital_twin = predefined_hplc_digital_twin

        g = Graph()
        g.add((URIRef(hplc_digital_twin), RDF.type, URIRef(ONTOHPLC_HIGHPERFORMANCELIQUIDCHROMATOGRAPHY)))
        g.add((URIRef(hplc_digital_twin), URIRef(ONTOHPLC_REPORTEXTENSION), URIRef(filename_extension)))
        sparql_client.uploadGraph(g)
        return hplc_digital_twin
    return _initialise_hplc_digital_twin_triples


@pytest.fixture(scope="module")
def initialise_hplc_derivation_input_triples():
    def _initialise_hplc_derivation_input_triples(sparql_client):
        chemical_solution_iri = 'http://www.example.com/placeholder/ChemicalSolution_' + str(uuid.uuid4())
        rxn_exp_iri = 'http://www.example.com/placeholder/ReactionExperiment_' + str(uuid.uuid4())
        g = Graph()
        g.add((URIRef(chemical_solution_iri), RDF.type, URIRef(ONTOLAB_CHEMICALSOLUTION)))
        g.add((URIRef(rxn_exp_iri), RDF.type, URIRef(ONTOREACTION_REACTIONEXPERIMENT)))
        sparql_client.uploadGraph(g)
        return rxn_exp_iri, chemical_solution_iri
    return _initialise_hplc_derivation_input_triples


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
        dry_run:bool=False,
    ):
        hplc_agent_config = config_hplc_agent(HPLC_AGENT_ENV)
        hplc_agent = HPLCAgent(
            hplc_digital_twin=hplc_agent_config.HPLC_DIGITAL_TWIN if hplc_digital_twin is None else hplc_digital_twin,
            hplc_report_periodic_timescale=hplc_agent_config.HPLC_REPORT_PERIODIC_TIMESCALE if hplc_report_periodic_timescale is None else hplc_report_periodic_timescale,
            hplc_report_container_dir=hplc_agent_config.HPLC_REPORT_CONTAINER_DIR if hplc_report_container_dir is None else hplc_report_container_dir,
            current_hplc_method=hplc_agent_config.CURRENT_HPLC_METHOD,
            hplc_report_file_extension=hplc_agent_config.HPLC_REPORT_FILE_EXTENSION if hplc_report_file_extension is None else hplc_report_file_extension,
            dry_run=dry_run,
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
            agent_endpoint=hplc_agent_config.ONTOAGENT_OPERATION_HTTP_URL, # we keep this as it is for now (start with http://host.docker.internal)
            app=Flask(__name__),
            max_thread_monitor_async_derivations=hplc_agent_config.MAX_THREAD_MONITOR_ASYNC_DERIVATIONS,
            email_recipient=hplc_agent_config.EMAIL_RECIPIENT,
            email_subject_prefix=hplc_agent_config.EMAIL_SUBJECT_PREFIX+' WSL2',
            email_username=hplc_agent_config.EMAIL_USERNAME,
            email_auth_json_path=os.path.join(SECRETS_PATH,'email_auth.json'),
            email_start_end_async_derivations=hplc_agent_config.EMAIL_START_END_ASYNC_DERIVATIONS,
        )
        return hplc_agent
    return _create_hplc_agent


# ----------------------------------------------------------------------------------
# Helper functions
# ----------------------------------------------------------------------------------


def host_docker_internal_to_localhost(endpoint: str):
    return endpoint.replace("host.docker.internal:", "localhost:")

def create_hplc_xls_report(docker_integration:bool=False):
    if docker_integration:
        file_path = os.path.join(DOCKER_INTEGRATION_DIR,f'{str(uuid.uuid4())}.xls')
    else:
        file_path = os.path.join(HPLC_REPORT_DIR,f'{str(uuid.uuid4())}.xls')
    if not os.path.exists(file_path):
        wb = xlwt.Workbook()
        ws = wb.add_sheet("Test Sheet")
        for i in range(0,10):
            for j in range(0,10):
                ws.write(i,j,"Placeholder")
        wb.save(file_path)
    return file_path

def create_hplc_txt_report(docker_integration:bool=False):
    if docker_integration:
        file_path = os.path.join(DOCKER_INTEGRATION_DIR,f'{str(uuid.uuid4())}.txt')
    else:
        file_path = os.path.join(HPLC_REPORT_DIR,f'{str(uuid.uuid4())}.txt')
    if not os.path.exists(file_path):
        with open(file_path, "w") as file:
            file.truncate(10 ** 3)
    return file_path

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
