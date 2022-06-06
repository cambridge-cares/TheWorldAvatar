# NOTE courtesy of Daniel (dln22), this file is adapted from https://github.com/cambridge-cares/TheWorldAvatar/blob/develop/JPS_BASE_LIB/python_uploader/tests/conftest.py
from chemistry_and_robots.data_model.ontohplc import TXTFILE_EXTENSION, XLSFILE_EXTENSION
from pathlib import Path
from rdflib import Graph
import logging
import pkgutil
import pytest
import shutil
import time
import uuid
import xlwt
import os

logging.getLogger("py4j").setLevel(logging.INFO)

from chemistry_and_robots.kg_operations.sparql_client import ChemistryAndRobotsSparqlClient

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
HPLC_REPORT_DIR = '/home/jb2197/CHEM32/Placeholder_Test/'
DOWNLOADED_DIR = os.path.join(THIS_DIR,'downloaded_files_for_test')

KG_SERVICE = "blazegraph"
KG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_SERVICE = "fileserver"
FS_ROUTE = "FileServer/"

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
def create_test_report():
    def _create_test_report(filename_extension):
        if filename_extension == XLSFILE_EXTENSION:
            file_path = create_hplc_xls_report()
        elif filename_extension == TXTFILE_EXTENSION:
            file_path = create_hplc_txt_report()
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
def initialise_triples(get_service_url, get_service_auth, generate_random_download_path):
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

    # Upload the example triples for testing
    for f in ['sample_data/new_exp_data.ttl', 'sample_data/duplicate_ontorxn.ttl', 'sample_data/dummy_lab.ttl',
        'sample_data/rxn_data.ttl', 'sample_data/dummy_post_proc.ttl']:
        data = pkgutil.get_data('chemistry_and_robots', 'resources/'+f).decode("utf-8")
        g = Graph().parse(data=data)
        filePath = generate_random_download_path('ttl')
        g.serialize(filePath, format='ttl')
        sparql_client.uploadOntology(filePath)

    yield sparql_client, sparql_endpoint, fs_url, fs_user, fs_pwd

    # Clear logger at the end of the test
    clear_loggers()

def create_hplc_xls_report():
    file_path = os.path.join(HPLC_REPORT_DIR,f'{str(uuid.uuid4())}.xls')
    if not os.path.exists(file_path):
        wb = xlwt.Workbook()
        ws = wb.add_sheet("Test Sheet")
        for i in range(0,10):
            for j in range(0,10):
                ws.write(i,j,"Placeholder")
        wb.save(file_path)
    return file_path

def create_hplc_txt_report():
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
