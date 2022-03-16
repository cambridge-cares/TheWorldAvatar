import pytest
import os
import pyuploader.uploaders as uploaders
from pyuploader.uploaders.uploader_factory import get_uploader
import pyuploader.errorhandling.appexceptions as appexcept


pytest_plugins = ["docker_compose"]

UPLOADER_IMP_STR="pyuploader.uploaders.uploader"
THIS_DIR = os.path.dirname(os.path.abspath(__file__))
BG_ROUTE = "blazegraph/namespace/kb/sparql"
FS_ROUTE = "FileServer/"
TEST_DIR = os.path.join(THIS_DIR, 'test_files')
TEST_FILE = os.path.join(TEST_DIR, 'test.owl')

# ----------------------------------------------------------------------------------
# Tests
# ----------------------------------------------------------------------------------


# Generic uploader test calling the uploader.upload method.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
      file_ext, subdirs, dry_run, uploaded_files_nr""",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None,    False, 1),
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None,    True,  0),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None,    False, 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None,    True,  0),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_FILE, "owl",     None,    False, 1),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', False, 2),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_FILE, "owl",     None,    True,  0),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', True,  0)
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
    #---------------------------
    uploader = get_uploader(
            uploader_type=uploader_type,
            url=service_url,
            auth_file = auth_file,
            subdirs = subdirs,
            )
    #---------------------------
    uploaded_files = uploader.upload(
            file_or_dir=file_or_dir,
            file_ext=file_ext,
            dry_run=dry_run
        )
    assert uploaded_files_nr == len(uploaded_files)
    if subdirs is not None and not dry_run:
        for locations in uploaded_files.values():
            assert subdirs in locations
# ----------------------------------------------------------------------------------


# Uploader stress test calling the uploader.upload method a large nr of times.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    "service_name, uploader_type, url_route, test_file, file_ext",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, 'owl'),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, 'owl'),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_FILE, 'owl')
]
)
def test_uploader_stress_test(
        service_name,
        uploader_type,
        url_route,
        test_file,
        file_ext,
        get_service_url,
        get_service_auth_file_path):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    uploader = get_uploader(uploader_type=uploader_type, url=service_url, auth_file=auth_file)
    for _ in range(1000):
      uploader.upload(file_ext=file_ext,file_or_dir=test_file)
# ----------------------------------------------------------------------------------

# Checks if an appropriate exception is raised in case of unsupported file extension.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    "service_name, uploader_type, url_route, file_or_dir, file_ext",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "ttl"),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "ttl")
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
    uploader = get_uploader(
            uploader_type=uploader_type,
            url=service_url,
            auth_file = auth_file
            )
    #---------------------------
    with pytest.raises(NotImplementedError):
        _ = uploader.upload(
                file_or_dir=file_or_dir,
                file_ext=file_ext,
            )

# Tests reading the auth details from env variables.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, subdirs, uploaded_files_nr""",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None, 1),
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None, 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_FILE, "owl",     None, 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR,  "owl",     None, 1),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_FILE, "owl",     None, 1),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_DIR,  "owl,log", 'a/b/c', 2),
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
    #---------------------------
    test_auth_env_var = "TEST_AUTH_ENV_VAR"
    monkeypatch.setenv(test_auth_env_var, auth_file)
    #---------------------------
    uploader = get_uploader(
        uploader_type=uploader_type,
        url=service_url,
        subdirs=subdirs,
        auth_file_env_var=test_auth_env_var
    )
    #---------------------------
    uploaded_files = uploader.upload(
            uploader_type=uploader_type,
            file_or_dir=file_or_dir,
            file_ext=file_ext
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
    "service_name, uploader_type, url_route",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE)
]
)
def test_uploader_auth_from_env_fail(
        service_name,
        uploader_type,
        url_route,
        get_service_url,
        ):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    #---------------------------
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = get_uploader(
            uploader_type=uploader_type,
            url=service_url,
            auth_file_env_var="TEST_AUTH_ENV_VAR"
        )

# Tests reading the url details from env variables.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, uploaded_files_nr""",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_DIR, "owl,log", 2)
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
    test_url_env_var = "TEST_URL_ENV_VAR"
    monkeypatch.setenv(test_url_env_var, service_url_file)
    #---------------------------
    uploader = get_uploader(
        uploader_type=uploader_type,
        auth_file=auth_file,
        url_env_var=test_url_env_var
    )
    #---------------------------
    uploaded_files = uploader.upload(
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
    """service_name, uploader_type""",
[
    ("blazegraph",     uploaders.TS_UPLOADER),
    ("blazegraph-geo", uploaders.TS_UPLOADER),
    ("fileserver",     uploaders.FS_UPLOADER)
]
)
def test_uploader_url_from_env_fail(
        service_name,
        uploader_type,
        get_service_auth_file_path):
    # service url, auth setup
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    with pytest.raises(appexcept.EnvironmentVarError):
        _ = get_uploader(
            uploader_type=uploader_type,
            auth_file=auth_file,
            url_env_var="TEST_URL_ENV_VAR"
        )

# Tests reading both the url and auth details from env variables.
# Parameterized to test various input variations.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_or_dir,
       file_ext, uploaded_files_nr""",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, TEST_DIR, "owl"    , 1),
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, TEST_DIR, "owl,log", 2)
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
    test_url_env_var = "TEST_URL_ENV_VAR"
    test_auth_env_var = "TEST_AUTH_ENV_VAR"
    monkeypatch.setenv(test_url_env_var, service_url_file)
    monkeypatch.setenv(test_auth_env_var, auth_file)
    #---------------------------
    uploader = get_uploader(
        uploader_type=uploader_type,
        url_env_var=test_url_env_var,
        auth_file_env_var=test_auth_env_var
    )
    #---------------------------
    uploaded_files = uploader.upload(
            file_or_dir=file_or_dir,
            file_ext=file_ext,
        )
    assert uploaded_files_nr == len(uploaded_files)


# Tests for a wrong file type error.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_ext""",
[
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, "owl"),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, "owl"),
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
    uploader = get_uploader(
        uploader_type=uploader_type,
        url=service_url,
        auth_file=auth_file
    )
    #---------------------------
    with pytest.raises(appexcept.FileUploadError):
        _ = uploader.upload(
                file_or_dir=test_file,
                file_ext=file_ext,
            )

# ----------------------------------------------------------------------------------

# Tests for uploading large files.
# ----------------------------------------------------------------------------------
@pytest.mark.parametrize(
    """service_name, uploader_type, url_route, file_ext, uploaded_files_nr""",
[
    ("fileserver",     uploaders.FS_UPLOADER, FS_ROUTE, "log", 1),
    ("blazegraph",     uploaders.TS_UPLOADER, BG_ROUTE, "owl", 1),
    ("blazegraph-geo", uploaders.TS_UPLOADER, BG_ROUTE, "owl", 1),
]
)
def test_uploader_large_file(
        service_name,
        uploader_type,
        url_route,
        file_ext,
        uploaded_files_nr,
        get_service_url,
        get_service_auth_file_path,
        create_large_file):
    # service url, auth setup
    service_url = get_service_url(service_name, url_route=url_route)
    auth_file = get_service_auth_file_path(service_name)
    #---------------------------
    test_file = create_large_file(uploader_type)
    #---------------------------
    uploader = get_uploader(
        uploader_type=uploader_type,
        url=service_url,
        auth_file=auth_file
    )
    #---------------------------
    uploaded_files = uploader.upload(
            file_or_dir=test_file,
            file_ext=file_ext,
        )
    assert uploaded_files_nr == len(uploaded_files)