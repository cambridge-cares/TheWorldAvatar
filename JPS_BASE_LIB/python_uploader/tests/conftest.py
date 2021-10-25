import pytest
import time
import os

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
SECRETS_PATH = os.path.join(THIS_DIR,'dummy_services_secrets')

def pytest_sessionfinish(session):
    """ This will run after all the tests"""
    dummy_auth_file = os.path.join(THIS_DIR, "dummy_test_auth.txt")
    dummy_url_file = os.path.join(THIS_DIR, "dummy_test_url.txt")
    if os.path.exists(dummy_auth_file):
        os.remove(dummy_auth_file)
    if os.path.exists(dummy_url_file):
        os.remove(dummy_url_file)

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
        return f"{username}:{password}"
    return _get_service_auth


@pytest.fixture(scope="session")
def write_service_auth_to_file(get_service_auth):
    def _write_service_auth_to_file(service_name):
        auth = get_service_auth(service_name)

        auth_file = os.path.join(THIS_DIR, "dummy_test_auth.txt")
        with open(auth_file, 'w') as f:
            f.write(auth)
        return auth_file
    return _write_service_auth_to_file

@pytest.fixture(scope="session")
def write_service_url_to_file(get_service_url):
    def _write_service_url_to_file(service_name, url_route):
        service_url = get_service_url(service_name, url_route)
        url_file = os.path.join(THIS_DIR, "dummy_test_url.txt")
        with open(url_file, 'w') as f:
            f.write(service_url)
        return url_file
    return _write_service_url_to_file