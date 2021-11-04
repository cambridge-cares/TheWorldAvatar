import pytest
import os
import pyuploader.app as app
from pyuploader.uploaders.uploader_factory import get_uploader
from pyuploader.common.utils import get_credentials_from_file
import pyuploader.errorhandling.appexceptions as appexcept

pytest_plugins = ["docker_compose"]

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
# ----------------------------------------------------------------------------------
# Tests
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize("file_or_dir",
[
(os.path.join('test_files', 'test.owl')), # file input
(os.path.join('test_files')),             # dir input
]
)
def test_blazegraph(file_or_dir, get_service_url, get_service_auth_file_path):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    file_or_dir = os.path.join(THIS_DIR, file_or_dir)
    #---------------------------
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=file_or_dir,
            file_ext='owl',
            url=service_url,
            auth_file=auth_file,
            no_file_logging=True
        )

def test_blazegraph_stress_test(get_service_url, get_service_auth_file_path):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    auth = get_credentials_from_file(auth_file)

    ts_uploader = get_uploader(uploader_type='ts_uploader')
    for _ in range(1000):
      ts_uploader._upload_file(service_url, auth, test_owl_file)

def test_blazegraph_wrong_file_ext(get_service_url, get_service_auth_file_path):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(NotImplementedError):
        _ = app.upload(
                uploader_type='ts_uploader',
                file_or_dir=test_owl_file,
                file_ext='ttl',
                url=service_url,
                auth_file=auth_file,
                no_file_logging=True
            )

@pytest.mark.parametrize("file_or_dir",
[
(os.path.join('test_files', 'test.owl')), # file input
(os.path.join('test_files')),             # dir input
]
)
def test_blazegraph_auth_from_env(file_or_dir, get_service_url, get_service_auth_file_path, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SECRETS", auth_file)
    #---------------------------
    file_or_dir = os.path.join(THIS_DIR, file_or_dir)
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=file_or_dir,
            file_ext='owl',
            url=service_url,
            no_file_logging=True
        )

def test_blazegraph_auth_from_env_fail(get_service_url, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    monkeypatch.delenv("TRIPLE_STORE_SECRETS")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = app.upload(
                uploader_type='ts_uploader',
                file_or_dir=test_owl_file,
                file_ext='owl',
                url=service_url,
                no_file_logging=True
            )

def test_blazegraph_url_from_env(write_service_url_to_file, get_service_auth_file_path, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url_file = write_service_url_to_file(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SPECS", service_url_file)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=test_owl_file,
            file_ext='owl',
            auth_file=auth_file,
            no_file_logging=True
        )

def test_blazegraph_url_from_env_fail(get_service_auth_file_path, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    auth_file = get_service_auth_file_path(service_name)
    monkeypatch.delenv("TRIPLE_STORE_SPECS")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = app.upload(
                uploader_type='ts_uploader',
                file_or_dir=test_owl_file,
                file_ext='owl',
                auth_file=auth_file,
                no_file_logging=True
            )

def test_blazegraph_url_and_auth_from_env(write_service_url_to_file, get_service_auth_file_path, monkeypatch):
    # service url, auth setup
    service_name = "blazegraph"
    service_url_file = write_service_url_to_file(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    auth_file = get_service_auth_file_path(service_name)
    monkeypatch.setenv("TRIPLE_STORE_SPECS", service_url_file)
    monkeypatch.setenv("TRIPLE_STORE_SECRETS", auth_file)
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=test_owl_file,
            file_ext='owl',
            no_file_logging=True
        )

# ISSUE, upload succeds even though no auth is used
def test_blazegraph_no_auth(get_service_url):
    # service url, auth setup
    service_name = "blazegraph"
    service_url = get_service_url(service_name, url_route=f"{service_name}/namespace/kb/sparql")
    #---------------------------
    test_owl_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=test_owl_file,
            file_ext='owl',
            url=service_url,
            no_auth=True,
            no_file_logging=True
        )

@pytest.mark.parametrize("file_or_dir",
[
(os.path.join('test_files', 'test.owl')), # file input
(os.path.join('test_files')),             # dir input
]
)
def test_blazegraph_geo_no_auth(file_or_dir, get_service_url, get_service_auth_file_path):
    # service url, auth setup
    service_name = "blazegraph-geo"
    service_url = get_service_url(service_name, url_route=f"/blazegraph/namespace/kb/sparql")
    file_or_dir = os.path.join(THIS_DIR, file_or_dir)
    #---------------------------
    _ = app.upload(
            uploader_type='ts_uploader',
            file_or_dir=file_or_dir,
            file_ext='owl',
            url=service_url,
            no_auth=True,
            no_file_logging=True
        )

## rdf4j is currently not accessible for some reason.
##def test_rdf4j(rdf4j_test_port, rdf4j_password):
##    test_owl_file = os.path.join(THIS_DIR, 'test.owl')
##    sparql_end_point = f"http://localhost:{rdf4j_test_port}/rdf4j-workbench"
##    upload_to_triple_store(test_owl_file, sparql_end_point, f"rdf_user:{rdf4j_password}")
#
#def test_fileserver(get_service_url, get_service_auth):
#    # service url setup
#    service_name = "fileserver"
#    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
#    auth = get_service_auth(service_name)
#    #---------------------------
#    test_file = os.path.join(THIS_DIR, 'test_files', 'test.log')
#    file_location = upload_to_web_server(test_file, service_url, auth)
#    assert len(file_location) == 1
#    assert file_location[test_file].endswith('.log') == True
#
#def test_fileserver_auth_from_env(get_service_url, write_service_auth_to_file, monkeypatch):
#    # service url, auth setup
#    service_name = "fileserver"
#    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
#    auth_file = write_service_auth_to_file(service_name)
#    monkeypatch.setenv("KG_FILE_SERVER_SECRETS", auth_file)
#    #---------------------------
#    test_files = os.path.join(THIS_DIR, 'test_files')
#    file_location = upload_to_web_server(test_files, service_url, file_ext='json')
#    assert len(file_location) == 1
#    assert file_location[os.path.join(test_files,'test.json')].endswith('.json') == True
#
#def test_fileserver_auth_from_env_fail(get_service_url, monkeypatch):
#    # service url, auth setup
#    service_name = "fileserver"
#    service_url = get_service_url(service_name, url_route=f"/FileServer/upload")
#    monkeypatch.delenv("KG_FILE_SERVER_SECRETS")
#    #---------------------------
#    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
#    with pytest.raises(EnvironmentVarError):
#        upload_to_web_server(test_file, service_url)
#
#def test_fileserver_url_from_env(write_service_url_to_file, get_service_auth, monkeypatch):
#    # service url, auth setup
#    service_name = "fileserver"
#    service_url_file = write_service_url_to_file(service_name, url_route=f"/FileServer/upload")
#    auth = get_service_auth(service_name)
#    monkeypatch.setenv("KG_FILE_SERVER_SPECS", service_url_file)
#    #---------------------------
#    test_files = os.path.join(THIS_DIR, 'test_files')
#    file_location = upload_to_web_server(test_files, auth_str=auth, file_ext='json,log')
#    assert len(file_location) == 2
#    assert file_location[os.path.join(test_files,'test.json')].endswith('.json') == True
#    assert file_location[os.path.join(test_files,'test.log')].endswith('.log') == True
#
#def test_fileserver_url_from_env_fail(get_service_auth, monkeypatch):
#    # service url, auth setup
#    service_name = "fileserver"
#    auth = get_service_auth(service_name)
#    monkeypatch.delenv("KG_FILE_SERVER_SPECS")
#    #---------------------------
#    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
#    with pytest.raises(EnvironmentVarError):
#        upload_to_web_server(test_file, auth_str=auth)
#
#def test_fileserver_auth_url_from_env(write_service_url_to_file, write_service_auth_to_file, monkeypatch):
#    # service url, auth setup
#    service_name = "fileserver"
#    service_url_file = write_service_url_to_file(service_name, url_route=f"/FileServer/upload")
#    auth_file = write_service_auth_to_file(service_name)
#    monkeypatch.setenv("KG_FILE_SERVER_SPECS", service_url_file)
#    monkeypatch.setenv("KG_FILE_SERVER_SECRETS", auth_file)
#    #---------------------------
#    test_file = os.path.join(THIS_DIR, 'test_files', 'test.owl')
#    file_location = upload_to_web_server(test_file)
#    assert len(file_location) == 1
#    assert file_location[test_file].endswith('.owl') == True