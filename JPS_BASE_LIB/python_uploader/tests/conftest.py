import pytest
import time
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')
SECRETS_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_auth')
URL_FILE_PATH = os.path.join(THIS_DIR,'dummy_services_secrets', 'dummy_test_url')
LARGE_TXT_FILE = os.path.join(THIS_DIR, 'large_txt_file')
LARGE_OWL_FILE = os.path.join(THIS_DIR, 'large_owl_file')

def pytest_sessionstart(session):
    """ This will run before all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(LARGE_TXT_FILE):
        os.remove(LARGE_TXT_FILE)
    if os.path.exists(LARGE_OWL_FILE):
        os.remove(LARGE_OWL_FILE)

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    if os.path.exists(SECRETS_FILE_PATH):
        os.remove(SECRETS_FILE_PATH)
    if os.path.exists(URL_FILE_PATH):
        os.remove(URL_FILE_PATH)
    if os.path.exists(LARGE_TXT_FILE):
        os.remove(LARGE_TXT_FILE)
    if os.path.exists(LARGE_OWL_FILE):
        os.remove(LARGE_OWL_FILE)

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
def write_service_url_to_file(get_service_url):
    def _write_service_url_to_file(service_name, url_route):
        service_url = get_service_url(service_name, url_route)
        with open(URL_FILE_PATH, 'w') as f:
            f.write(service_url)
        return URL_FILE_PATH
    return _write_service_url_to_file

@pytest.fixture(scope="session")
def create_large_file():
    def _create_large_file(uploader_type):
        if uploader_type == 'ts_uploader':
            large_file_path = create_large_owl_file()
        else:
            large_file_path = create_large_txt_file()

        return large_file_path
    return _create_large_file

def create_large_txt_file():
    if not os.path.exists(LARGE_TXT_FILE):
        with open(LARGE_TXT_FILE, "w") as file:
            file.truncate(10 ** 8) # ~ 100MB
    return LARGE_TXT_FILE

def create_large_owl_file():
    if not os.path.exists(LARGE_OWL_FILE):
        with open(LARGE_OWL_FILE, "w") as file:
            file.write("""<?xml version="1.0"?>
                    <rdf:RDF xmlns="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#"
                        xml:base="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90"
                        xmlns:owl="http://www.w3.org/2002/07/owl#"
                        xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                        xmlns:xml="http://www.w3.org/XML/1998/namespace"
                        xmlns:xsd="http://www.w3.org/2001/XMLSchema#"
                        xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
                        <owl:Ontology rdf:about="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90"/>
            """)
            for i in range(7*10**5):
                file.write(
                    """<!-- http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#A$i -->
                    <owl:Class rdf:about="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#A$i"/>
                    <!-- http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#B$i -->
                    <owl:Class rdf:about="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#B$i">
                        <rdfs:subClassOf rdf:resource="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#A$i"/>
                    </owl:Class>
                    <!-- http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#C$i -->
                    <owl:Class rdf:about="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#C$i">
                        <rdfs:subClassOf rdf:resource="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#B$i"/>
                    </owl:Class>
                    <!-- http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#D$i -->
                    <owl:Class rdf:about="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#D$i">
                        <rdfs:subClassOf rdf:resource="http://www.semanticweb.org/msff2/ontologies/2021/9/untitled-ontology-90#C$i"/>
                    </owl:Class>
                    """.replace('$i', str(i))
                )
            file.write("</rdf:RDF>") # ~ 1GB

    return LARGE_OWL_FILE