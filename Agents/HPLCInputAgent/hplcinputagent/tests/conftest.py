# NOTE courtesy of Daniel (dln22), this file is adapted from https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_uploader/tests/conftest.py
from testcontainers.core.container import DockerContainer
from pathlib import Path
from rdflib import Graph
import logging
import pkgutil
import pytest
import time
import uuid
import os

logging.getLogger("py4j").setLevel(logging.INFO)

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
HPLC_TXT_REPORT_FILE = os.path.join(THIS_DIR, 'hplc_txt_file')
HPLC_XLS_REPORT_FILE = os.path.join(THIS_DIR, 'hplc_xls_file')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/upload"

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
def get_service_auth_file_path():
    def _get_service_auth_file_path(service_name):
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

        with open(SECRETS_FILE_PATH, 'w') as f:
            f.write(f"{username}:{password}")

        return SECRETS_FILE_PATH

    return _get_service_auth_file_path

@pytest.fixture(scope="session")
def initialise_triples(get_service_url):
    # Create SparqlClient for testing
    sparql_endpoint = get_service_url(KG_SERVICE, url_route=KG_ROUTE)
    file_service_url = get_service_url(FS_SERVICE, url_route=FS_ROUTE)
    sparql_client = ChemistryAndRobotsSparqlClient(sparql_endpoint, sparql_endpoint)

    # Upload the example triples for testing
    for f in ['sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl', 'sample_data/dummy_lab.ttl', 'sample_data/rxn_data.ttl']:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = f'{str(uuid.uuid4())}.ttl'
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)
        os.remove(filePath)

    yield sparql_client, sparql_endpoint, file_service_url

    # Clear logger at the end of the test
    clear_loggers()

# method adopted from https://github.com/pytest-dev/pytest/issues/5502#issuecomment-647157873
def clear_loggers():
    """Remove handlers from all loggers"""
    import logging
    loggers = [logging.getLogger()] + list(logging.Logger.manager.loggerDict.values())
    for logger in loggers:
        handlers = getattr(logger, 'handlers', [])
        for handler in handlers:
            logger.removeHandler(handler)
