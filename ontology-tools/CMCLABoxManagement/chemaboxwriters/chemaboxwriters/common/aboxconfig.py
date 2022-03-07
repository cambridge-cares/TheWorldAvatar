from typing import Dict, Tuple
import yaml
import logging

logger = logging.getLogger(__name__)

FILE_SERVER = "file server"
TRIPLE_STORE = "triple store"

UPLOAD_SETTINGS_KEY = "upload_settings"
QUERY_SETTINGS_KEY = "query_settings"
WRITERS_PREFIXES_KEY = "prefixes"
HANDLER_KWARGS = "handler_kwargs"

TRIPLE_STORE_SPARQL_ENDPOINT_KEY = "triple_store_sparql_endpoint"
TRIPLE_STORE_SECRETS_FILE_KEY = "triple_store_secrets_file"
TRIPLE_STORE_NO_AUTH_KEY = "triple_store_no_auth"
FILE_SERVER_UPLOAD_ENDPOINT_KEY = "file_server_upload_endpoint"
FILE_SERVER_SECRETS_FILE_KEY = "file_server_secrets_file"
FILE_SERVER_SUBDIR_KEY = "file_server_subdir"
FILE_SERVER_NO_AUTH_KEY = "file_server_no_auth"
OSPECIES_QUERY_ENDPOINT_KEY = "ospecies_query_endpoint"
OMOPS_QUERY_ENDPOINT_KEY = "omops_query_endpoint"
OPSSCAN_QUERY_ENDPOINT_KEY = "opsscan_query_endpoint"
OCOMPCHEM_QUERY_ENDPOINT_KEY = "ocompchem_query_endpoint"
UPLOAD_TO_FILE_SERVER_KEY = "upload_to_file_server"
UPLOAD_TO_TRIPLE_STORE_KEY = "upload_to_triple_store"

DEFAULT_CONFIG_KEYS = [
    TRIPLE_STORE_SPARQL_ENDPOINT_KEY,
    TRIPLE_STORE_SECRETS_FILE_KEY,
    TRIPLE_STORE_NO_AUTH_KEY,
    FILE_SERVER_UPLOAD_ENDPOINT_KEY,
    FILE_SERVER_SECRETS_FILE_KEY,
    FILE_SERVER_SUBDIR_KEY,
    FILE_SERVER_NO_AUTH_KEY,
    OCOMPCHEM_QUERY_ENDPOINT_KEY,
    OSPECIES_QUERY_ENDPOINT_KEY,
    OMOPS_QUERY_ENDPOINT_KEY,
    OPSSCAN_QUERY_ENDPOINT_KEY,
    UPLOAD_TO_FILE_SERVER_KEY,
    UPLOAD_TO_TRIPLE_STORE_KEY,
]
HANDLERS_CONFIG_KEY = "handlers"


CONFIG_GROUPS = [
    UPLOAD_SETTINGS_KEY,
    QUERY_SETTINGS_KEY,
    WRITERS_PREFIXES_KEY,
    HANDLER_KWARGS,
]


def get_pipeline_handler_configs(pipeline_name: str, config: Dict) -> Tuple[Dict, Dict]:

    if HANDLERS_CONFIG_KEY in config:
        logger.warning(
            (
                f"Found '{HANDLERS_CONFIG_KEY}' key in the default config section. "
                "This will be omitted."
            )
        )
        config.pop(HANDLERS_CONFIG_KEY)

    pipeline_configs = config.get(pipeline_name, {})

    _merge_config_groups(
        merge_from=config,
        merge_to=pipeline_configs,
    )

    handlers_config = pipeline_configs.pop(HANDLERS_CONFIG_KEY, {})
    handlers = handlers_config.keys()
    for handler in handlers:
        _merge_config_groups(
            merge_from=pipeline_configs, merge_to=handlers_config[handler]
        )

    return pipeline_configs, handlers_config


def _merge_config_groups(merge_from: Dict, merge_to: Dict) -> None:
    for key in CONFIG_GROUPS:
        _merge_config_field(merge_from=merge_from, merge_to=merge_to, merge_on=key)


def _merge_config_field(merge_from: Dict, merge_to: Dict, merge_on: str) -> None:
    merge_from_configs = merge_from.get(merge_on, {})
    merge_to_configs = merge_to.get(merge_on, {})

    merge_to_configs = _merge_configs(
        merge_into=merge_to_configs,
        merge_from=merge_from_configs,
    )

    merge_to[merge_on] = merge_to_configs


def _merge_configs(merge_into: Dict, merge_from: Dict) -> Dict:
    return {**merge_from, **merge_into}


def read_config_file(config_file: str) -> Dict:
    config_dict = {}
    if config_file is not None:
        with open(config_file, "r") as stream:
            config_dict = yaml.safe_load(stream)

    return config_dict
