import pytest
import os
from pyuploader.triplestore.upload import upload_to_triple_store
from pyuploader.webserver.upload import upload_to_web_server
from pyuploader.errorhandling.appexceptions import EnvironmentVarError

pytest_plugins = ["docker_compose"]

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
# ----------------------------------------------------------------------------------
# Tests
# ----------------------------------------------------------------------------------
def test_blazegraph(get_service_url, get_service_auth):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth = get_service_auth(service_name)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    upload_to_triple_store(test_owl_file, service_url, auth)

def test_blazegraph_wrong_file_ext(get_service_url, get_service_auth):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth = get_service_auth(service_name)
    #---------------------------
    test_files = os.path.join(THIS_DIR, 'test_files')
    with pytest.raises(NotImplementedError):
        upload_to_triple_store(test_files, service_url, auth, file_ext='ttl')

def test_blazegraph_auth_from_env(get_service_url, write_service_auth_to_file, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = write_service_auth_to_file(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SECRETS", auth_file)
    #---------------------------
    test_files = os.path.join(THIS_DIR, 'test_files')
    upload_to_triple_store(test_files, service_url)

def test_blazegraph_auth_from_env_fail(get_service_url, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    monkeypatch.delenv("TRIPLE_STORE_SECRETS")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(EnvironmentVarError):
        upload_to_triple_store(test_owl_file, service_url)

def test_blazegraph_url_from_env(write_service_url_to_file, get_service_auth, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url_file = write_service_url_to_file(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth = get_service_auth(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SPECS", service_url_file)
    #---------------------------
    test_files = os.path.join(THIS_DIR, 'test_files')
    upload_to_triple_store(test_files, auth_str=auth, file_ext='owl')

def test_blazegraph_url_from_env_fail(get_service_auth, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    auth = get_service_auth(service_name)
    monkeypatch.delenv("TRIPLE_STORE_SPECS")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(EnvironmentVarError):
        upload_to_triple_store(test_owl_file, auth_str=auth)

def test_blazegraph_auth_url_from_env(write_service_url_to_file, write_service_auth_to_file, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url_file = write_service_url_to_file(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = write_service_auth_to_file(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SPECS", service_url_file)
    monkeypatch.setenv("TRIPLE_STORE_SECRETS", auth_file)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    upload_to_triple_store(test_owl_file)

def test_blazegraph_geo(get_service_url):
    # service url setup
    service_name = "blazegraph-geo"
    service_url = get_service_url(service_name, url_route=f"/blazegraph/namespace/kb/sparql")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    upload_to_triple_store(test_owl_file, service_url, no_auth=True)

# rdf4j is currently not accessible for some reason.
#def test_rdf4j(rdf4j_test_port, rdf4j_password):
#    test_owl_file = os.path.join(THIS_DIR, 'test.owl')
#    sparql_end_point = f"http://localhost:{rdf4j_test_port}/rdf4j-workbench"
#    upload_to_triple_store(test_owl_file, sparql_end_point, f"rdf_user:{rdf4j_password}")

def test_fileserver(get_service_url, get_service_auth):
    # service url setup
    service_name = "fileserver"
    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
    auth = get_service_auth(service_name)
    #---------------------------
    test_file = os.path.join(THIS_DIR, 'test_files', 'test.log')
    file_location = upload_to_web_server(test_file, service_url, auth)
    assert len(file_location) == 1
    assert file_location[test_file].endswith('.log') == True

def test_fileserver_auth_from_env(get_service_url, write_service_auth_to_file, monkeypatch):
    # service url, auth setup
    service_name = "fileserver"
    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
    auth_file = write_service_auth_to_file(service_name)
    monkeypatch.setenv("KG_FILE_SERVER_SECRETS", auth_file)
    #---------------------------
    test_files = os.path.join(THIS_DIR, 'test_files')
    file_location = upload_to_web_server(test_files, service_url, file_ext='json')
    assert len(file_location) == 1
    assert file_location[os.path.join(test_files,'test.json')].endswith('.json') == True

def test_fileserver_auth_from_env_fail(get_service_url, monkeypatch):
    # service url, auth setup
    service_name = "fileserver"
    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
    monkeypatch.delenv("KG_FILE_SERVER_SECRETS")
    #---------------------------
    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(EnvironmentVarError):
        upload_to_web_server(test_file, service_url)

def test_fileserver_url_from_env(write_service_url_to_file, get_service_auth, monkeypatch):
    # service url, auth setup
    service_name = "fileserver"
    service_url_file = write_service_url_to_file(service_name, url_route=f"/FileServer/upload")
    auth = get_service_auth(service_name)
    monkeypatch.setenv("KG_FILE_SERVER_SPECS", service_url_file)
    #---------------------------
    test_files = os.path.join(THIS_DIR, 'test_files')
    file_location = upload_to_web_server(test_files, auth_str=auth, file_ext='json,log')
    assert len(file_location) == 2
    assert file_location[os.path.join(test_files,'test.json')].endswith('.json') == True
    assert file_location[os.path.join(test_files,'test.log')].endswith('.log') == True

def test_fileserver_url_from_env_fail(get_service_auth, monkeypatch):
    # service url, auth setup
    service_name = "fileserver"
    auth = get_service_auth(service_name)
    monkeypatch.delenv("KG_FILE_SERVER_SPECS")
    #---------------------------
    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(EnvironmentVarError):
        upload_to_web_server(test_file, auth_str=auth)

def test_fileserver_auth_url_from_env(write_service_url_to_file, write_service_auth_to_file, monkeypatch):
    # service url, auth setup
    service_name = "fileserver"
    service_url_file = write_service_url_to_file(service_name, url_route=f"/FileServer/upload")
    auth_file = write_service_auth_to_file(service_name)
    monkeypatch.setenv("KG_FILE_SERVER_SPECS", service_url_file)
    monkeypatch.setenv("KG_FILE_SERVER_SECRETS", auth_file)
    #---------------------------
    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    file_location = upload_to_web_server(test_file)
    assert len(file_location) == 1
    assert file_location[test_file].endswith('.owl') == True