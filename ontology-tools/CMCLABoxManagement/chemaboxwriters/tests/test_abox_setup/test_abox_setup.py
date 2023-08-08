import pytest
import os
import chemaboxwriters.common.assemble_pipeline as asp
import chemaboxwriters.common.aboxconfig as abconf
from chemaboxwriters.common.uploaders import UploaderClient
from chemaboxwriters.kgoperations.remotestore_client import RemoteStoreClientContainer
from typing import Optional, Dict

THIS_DIR = os.path.dirname(os.path.abspath(__file__))
TEST_CONFIG_FILES = os.path.join(THIS_DIR, "test_config_files")
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

    test_results = abconf.read_config_file(config_file=config_file)
    ref_results = test_results.pop("test_results")

    abconf.cascade_configs(configs=test_results)

    _compare_config_items(config_item1=test_results, config_item2=ref_results)
    print("")

    print("========================================================")
    print()
    print()


def _compare_config_items(config_item1, config_item2) -> None:
    if isinstance(config_item1, dict):
        for key, value in config_item1.items():
            assert key in config_item2
            _compare_config_items(value, config_item2[key])
    else:
        assert config_item1 == config_item2


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

            check_handler_kwargs_configs(
                handler_kwargs=handler._handler_params._parameters,
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


def check_handler_kwargs_configs(handler_kwargs: Dict, configs: Dict) -> None:
    handler_kwargs_configs = configs.get(abconf.HANDLER_KWARGS)
    if handler_kwargs_configs is None:
        return

    for kwarg_name, kwarg_value in handler_kwargs_configs.items():
        assert kwarg_name in handler_kwargs

        assert handler_kwargs[kwarg_name]["value"] == kwarg_value
