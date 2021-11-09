import pytest
import os
import pyuploader.app as app
from pyuploader.uploaders.uploader_factory import get_uploader
from pyuploader.common.utils import get_credentials_from_file
import pyuploader.errorhandling.appexceptions as appexcept


pytest_plugins = ["docker_compose"]

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
BG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_ROUTE = "FileServer/upload"
TEST_DIR = os.path.join(THIS_DIR, 'test_files')
TEST_FILE = os.path.join(TEST_DIR, 'test.owl')

# ----------------------------------------------------------------------------------
# Tests
# ----------------------------------------------------------------------------------


# Generic uploader test calling the app.upload function.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
      file_ext, subdirs, dry_run, uploaded_files_nr""",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None,    False, 1),
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None,    True,  0),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None,    False, 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None,    True,  0),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', False, 2),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', True,  0)
]
)
def test_uploader(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        subdirs,
        dry_run,
        uploaded_files_nr,
        get_service_url,
        get_service_auth_file_path):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    service_url = 'http://aa'
    #---------------------------
    uploaded_files = app.upload(
            uploader_type=uploader_type,
            file_or_dir=file_or_dir,
            file_ext=file_ext,
            subdirs=subdirs,
            url=service_url,
            auth_file=auth_file,
            dry_run=dry_run,
            no_file_logging=True
        )
    assert uploaded_files_nr == len(uploaded_files)
    if subdirs is not None and not dry_run:
        for locations in uploaded_files.values():
            assert subdirs in locations
# ----------------------------------------------------------------------------------


# Uploader stress test calling the app.upload function a large nr of times.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    "service_name, uploader_type, url_route, file_or_dir",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_FILE),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_FILE),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_FILE)
]
)
def test_uploader_stress_test(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        get_service_url,
        get_service_auth_file_path):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    auth = get_credentials_from_file(auth_file)
    #---------------------------

    uploader = get_uploader(uploader_type=uploader_type)
    for _ in range(1000):
      uploader._upload_file(service_url, auth, file_or_dir)
# ----------------------------------------------------------------------------------

# Checks if an appropriate exception is raised in case of unsupported file extension.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    "service_name, uploader_type, url_route, file_or_dir, file_ext",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_FILE, "ttl"),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_FILE, "ttl")
]
)
def test_uploader_wrong_file_ext(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        get_service_url,
        get_service_auth_file_path):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    with pytest.raises(NotImplementedError):
        _ = app.upload(
                uploader_type=uploader_type,
                file_or_dir=file_or_dir,
                file_ext=file_ext,
                url=service_url,
                auth_file=auth_file,
                no_file_logging=True
            )

# Tests reading the auth details from env variables.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, subdirs, uploaded_files_nr""",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None, 1),
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None, 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_FILE, "owl",     None, 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR,  "owl",     None, 1),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_FILE, "owl",     None, 1),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', 2),
]
)
def test_uploader_auth_from_env(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        subdirs,
        uploaded_files_nr,
        get_service_url,
        get_service_auth_file_path,
        monkeypatch):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    uploader = get_uploader(
        uploader_type=uploader_type,
        default_url=service_url
    )
    monkeypatch.setenv(uploader.get_auth_env_var_value(), auth_file)
    #---------------------------
    uploaded_files = uploader.upload(
            uploader_type=uploader_type,
            file_or_dir=file_or_dir,
            file_ext=file_ext,
            subdirs=subdirs
        )
    assert uploaded_files_nr == len(uploaded_files)
    if subdirs is not None:
        for locations in uploaded_files.values():
            assert subdirs in locations
# ----------------------------------------------------------------------------------


# Tests if an appropriate exception is raised if reading the auth details
# from env variables fails.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    "service_name, uploader_type, url_route, file_or_dir, file_ext",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    ),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    ),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR, "owl,log")
]
)
def test_uploader_auth_from_env_fail(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        get_service_url,
        monkeypatch):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    uploader = get_uploader(
        uploader_type=uploader_type,
        default_url=service_url
    )
    monkeypatch.delenv(uploader.get_auth_env_var_value())
    #---------------------------
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = uploader.upload(
                uploader_type=uploader_type,
                file_or_dir=file_or_dir,
                file_ext=file_ext
            )

# Tests reading the url details from env variables fails.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, uploaded_files_nr""",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR, "owl,log", 2)
]
)
def test_uploader_url_from_env(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        uploaded_files_nr,
        write_service_url_to_file,
        get_service_auth_file_path,
        monkeypatch):
    # service url, auth setup
    service_url_file = write_service_url_to_file(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    uploader = get_uploader(
        uploader_type=uploader_type,
        default_auth_file=auth_file
    )
    monkeypatch.setenv(uploader.get_url_env_var_value(), service_url_file)
    #---------------------------
    uploaded_files = uploader.upload(
            uploader_type=uploader_type,
            file_or_dir=file_or_dir,
            file_ext=file_ext
        )
    assert uploaded_files_nr == len(uploaded_files)
# ----------------------------------------------------------------------------------

# Tests if an appropriate exception is raised if reading the url details
# from env variables fails.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, file_or_dir, file_ext""",
[
    ("blazegraph",     "ts_uploader", TEST_DIR, "owl"    ),
    ("blazegraph-geo", "ts_uploader", TEST_DIR, "owl"    ),
    ("fileserver",     "fs_uploader", TEST_DIR, "owl,log")
]
)
def test_uploader_url_from_env_fail(
        service_name,
        uploader_type,
        file_or_dir,
        file_ext,
        get_service_auth_file_path,
        monkeypatch):
    # service url, auth setup
    auth_file = get_service_auth_file_path(service_name)
    uploader = get_uploader(
        uploader_type=uploader_type,
        default_auth_file=auth_file
    )
    monkeypatch.delenv(uploader.get_url_env_var_value())
    #---------------------------
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = app.upload(
                uploader_type=uploader_type,
                file_or_dir=file_or_dir,
                file_ext=file_ext
            )

# Tests reading both the url and auth details from env variables fails.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, uploaded_files_nr""",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("fileserver",     "fs_uploader", FS_ROUTE, TEST_DIR, "owl,log", 2)
]
)
def test_uploader_url_and_auth_from_env(
        service_name,
        uploader_type,
        url_route,
        file_or_dir,
        file_ext,
        uploaded_files_nr,
        write_service_url_to_file,
        get_service_auth_file_path,
        monkeypatch):
    # service url, auth setup
    service_url_file = write_service_url_to_file(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    uploader = get_uploader(
        uploader_type=uploader_type
    )
    monkeypatch.setenv(uploader.get_url_env_var_value(), service_url_file)
    monkeypatch.setenv(uploader.get_auth_env_var_value(), auth_file)
    #---------------------------
    uploaded_files = app.upload(
            uploader_type=uploader_type,
            file_or_dir=file_or_dir,
            file_ext=file_ext,
            no_file_logging=True
        )
    assert uploaded_files_nr == len(uploaded_files)



# Generic uploader test calling the app.upload function.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_ext""",
[
    ("blazegraph",     "ts_uploader", BG_ROUTE, "owl"),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, "owl"),
]
)
def test_uploader_wrong_file_type(
        service_name,
        uploader_type,
        url_route,
        file_ext,
        get_service_url,
        get_service_auth_file_path):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    test_file = os.path.join(TEST_DIR, 'test.log')
    #---------------------------
    with pytest.raises(appexcept.FileUploadError):
        _ = app.upload(
                uploader_type=uploader_type,
                file_or_dir=test_file,
                file_ext=file_ext,
                url=service_url,
                auth_file=auth_file,
                no_file_logging=True
            )

# ----------------------------------------------------------------------------------

# Tests reading both the url and auth details from env variables fails.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_ext""",
[
    ("fileserver",     "fs_uploader", FS_ROUTE, "log"),
    ("blazegraph",     "ts_uploader", BG_ROUTE, "owl"),
    ("blazegraph-geo", "ts_uploader", BG_ROUTE, "owl"),
]
)
def test_uploader_large_file(
        service_name,
        uploader_type,
        url_route,
        file_ext,
        get_service_url,
        get_service_auth_file_path,
        create_large_file):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    uploader = get_uploader(
        uploader_type=uploader_type,
        default_url=service_url,
        default_auth_file=auth_file
    )
    #---------------------------
    test_file = create_large_file(uploader_type)
    uploader.upload(
        uploader_type=uploader_type,
        file_or_dir=test_file,
        file_ext=file_ext,
        url=service_url,
        auth_file=auth_file,
        no_file_logging=True
    )
    #uploaded_files = app.upload(
    #        uploader_type=uploader_type,
    #        file_or_dir=file_or_dir,
    #        file_ext=file_ext,
    #        no_file_logging=True
    #    )
    #assert uploaded_files_nr == len(uploaded_files)

## Triple store tests, wont test anything atm as the current blazegraph image does not have
## the authorisation enabled
#@pytest.mark.parametrize(
#    "service_name, uploader_type, url_route, file_or_dir, file_ext",
#[
#    ("fileserver", "fs_uploader", FS_ROUTE, TEST_FILE, 'log')
#]
#)
#def test_uploader_no_auth_fail(
#   service_name,
#   uploader_type,
#   url_route,
#   file_or_dir,
#   file_ext,
#   get_service_url):
#    # service url, auth setup
#    service_url = get_service_url(service_name, url_route=url_route)
#    #---------------------------
#    # TODO: Add an appropriate exception
#    #
#    _ = app.upload(
#            uploader_type=uploader_type,
#            file_or_dir=file_or_dir,
#            file_ext=file_ext,
#            url=service_url,
#            no_auth=True
#        )
#
## rdf4j is currently not accessible for some reason.
##def test_rdf4j(rdf4j_test_port, rdf4j_password):
##    test_owl_file = os.path.join(THIS_DIR, 'test.owl')
##    sparql_end_point = f"http://localhost:{rdf4j_test_port}/rdf4j-workbench"
##    upload_to_triple_store(test_owl_file, sparql_end_point, f"rdf_user:{rdf4j_password}")
#