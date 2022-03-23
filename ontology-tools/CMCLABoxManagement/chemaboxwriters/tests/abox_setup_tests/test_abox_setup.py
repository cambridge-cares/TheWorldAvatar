import pytest
import os
import chemaboxwriters.common.assemble_pipeline as asp
import chemaboxwriters.common.aboxconfig as abconf
from chemaboxwriters.common.uploaders import UploaderClient
from chemaboxwriters.kgoperations.remotestore_client import RemoteStoreClientContainer
from typing import Optional, Dict

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_CONFIG_FILES = os.path.join(THIS_DIR, "..", "refData", "test_config_files")
TEST_RESULTS = "test_results"


@pytest.mark.parametrize(
    "endpoints_config_file",
    [
        ("test_config_1.yml"),
        ("test_config_2.yml"),
    ],
)
def test_abox_writer_cascade(endpoints_config_file: str):
    print("========================================================")
    print("TEST CONFIG FILE: ", endpoints_config_file)
    print()
    print()

    config_file = os.path.join(TEST_CONFIG_FILES, endpoints_config_file)

    test_config = abconf.read_config_file(config_file=config_file)
    test_results = test_config.pop("test_results")

    abconf.cascade_configs(configs=test_config)

    for key, value in test_config.items():
        assert key in test_results
        assert value == test_results[key]

    print("")

    print("========================================================")
    print()
    print()


@pytest.mark.parametrize(
    "endpoints_config_file",
    [
        ("test_config_1.yml"),
    ],
)
def test_abox_writer_handlers_setup(endpoints_config_file: str):
    print("========================================================")
    print("TEST CONFIG FILE: ", endpoints_config_file)
    print()
    print()

    config_file = os.path.join(TEST_CONFIG_FILES, endpoints_config_file)
    test_config = abconf.read_config_file(config_file=config_file)
    _ = test_config.pop("test_results")
    abconf.cascade_configs(configs=test_config)

    for pipeline_name in [
        item for item in test_config if item not in abconf.CONFIG_GROUPS
    ]:
        pipeline = asp.assemble_pipeline(
            pipeline_type=pipeline_name, config_dict=test_config
        )

        pipeline_config = test_config.get(pipeline_name, {})
        for handler in pipeline._handlers.values():
            handler_configs: Dict = pipeline_config.get(
                handler.name.lower(), pipeline_config
            )

            check_uploader_connection_configs(
                uploader=handler._file_server_uploader,
                configs=handler_configs,
                uploader_conf_key=abconf.FS_UPLOAD_SETTINGS_KEY,
            )

            check_uploader_connection_configs(
                uploader=handler._triple_store_uploader,
                configs=handler_configs,
                uploader_conf_key=abconf.TS_UPLOAD_SETTINGS_KEY,
            )

            check_query_configs(
                store_client_container=handler._remote_store_client,
                configs=handler_configs,
            )

            check_prefixes_configs(
                handler_prefixes=handler._handler_prefixes,
                configs=handler_configs,
            )

            check_handler_kwargs_configs(
                handler_kwargs=handler._handler_params,
                configs=handler_configs,
            )

    print("")

    print("========================================================")
    print()
    print()


def check_uploader_connection_configs(
    uploader: Optional[UploaderClient], configs: Dict, uploader_conf_key: str
) -> None:

    uploader_configs = configs.get(uploader_conf_key)
    if uploader_configs is None:
        return

    assert uploader is not None

    if abconf.URL_ENDPOINT_KEY in uploader_configs:
        assert uploader._url == uploader_configs[abconf.URL_ENDPOINT_KEY]

    if abconf.AUTH_FILE_KEY in uploader_configs:
        assert uploader._auth_file == uploader_configs[abconf.AUTH_FILE_KEY]

    if abconf.NO_AUTH_KEY in uploader_configs:
        assert uploader._no_auth == uploader_configs[abconf.NO_AUTH_KEY]

    if (
        abconf.FS_SUBDIRS_KEY in uploader_configs
        and uploader._uploader_type == "file server"
    ):
        assert uploader._subdirs == uploader_configs[abconf.FS_SUBDIRS_KEY]

    if abconf.UPL_FILE_TYPES in uploader_configs:
        assert uploader._upload_file_types == uploader_configs[abconf.UPL_FILE_TYPES]


def check_query_configs(
    store_client_container: Optional[RemoteStoreClientContainer], configs: Dict
) -> None:

    handler_query_configs = configs.get(abconf.QUERY_SETTINGS_KEY)

    if handler_query_configs is None:
        return

    assert store_client_container is not None

    for query_prefix, query_endpoint in handler_query_configs.items():
        assert store_client_container.query_endpoints[query_prefix] == query_endpoint


def check_prefixes_configs(handler_prefixes: Dict, configs: Dict) -> None:

    handler_prefix_configs = configs.get(abconf.WRITERS_PREFIXES_KEY)
    if handler_prefix_configs is None:
        return

    for prefix_name, prefix_value in handler_prefix_configs.items():
        assert prefix_name in handler_prefixes

        assert handler_prefixes[prefix_name]["value"] == prefix_value


def check_handler_kwargs_configs(handler_kwargs: Dict, configs: Dict) -> None:
    handler_kwargs_configs = configs.get(abconf.HANDLER_KWARGS)
    if handler_kwargs_configs is None:
        return

    for kwarg_name, kwarg_value in handler_kwargs_configs.items():
        assert kwarg_name in handler_kwargs

        assert handler_kwargs[kwarg_name]["value"] == kwarg_value
