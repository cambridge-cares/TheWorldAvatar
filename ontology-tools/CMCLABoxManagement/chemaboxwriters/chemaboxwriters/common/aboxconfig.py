from typing import Dict, List
import yaml
import logging

__doc__ = """
This module stores all allowed keys that are used in the abox config file
together with the code that processes it.
"""

logger = logging.getLogger(__name__)

FILE_SERVER = "file server"
TRIPLE_STORE = "triple store"
QUERY_SETTINGS_KEY = "kg_query_endpoints"
HANDLER_KWARGS = "handler_kwargs"
FS_UPLOAD_SETTINGS_KEY = "file_server_upload_settings"
TS_UPLOAD_SETTINGS_KEY = "triple_store_upload_settings"
URL_ENDPOINT_KEY = "url"
AUTH_FILE_KEY = "auth_file"
NO_AUTH_KEY = "no_auth"
FS_SUBDIRS_KEY = "subdirs"
UPL_FILE_TYPES = "upload_file_types"

CONFIG_GROUPS = [
    FS_UPLOAD_SETTINGS_KEY,
    TS_UPLOAD_SETTINGS_KEY,
    QUERY_SETTINGS_KEY,
    HANDLER_KWARGS,
]


def cascade_configs(configs: Dict, cascade_fields: List[str] = CONFIG_GROUPS) -> None:
    """Performs config cascade going from global->pipeline->handler levels"""
    subconfigs = [name for name in configs.keys() if name not in cascade_fields]

    for configs_group in subconfigs:
        if configs[configs_group] is not None:
            _merge_config_groups(
                merge_from=configs,
                merge_to=configs[configs_group],
                merge_fields=cascade_fields,
            )
            cascade_configs(
                configs=configs[configs_group], cascade_fields=cascade_fields
            )

def _merge_config_groups(
    merge_from: Dict, merge_to: Dict, merge_fields: List[str]
) -> None:
    for key in merge_fields:
        _merge_config_field(merge_from=merge_from, merge_to=merge_to, merge_on=key)

def _merge_config_field(merge_from: Dict, merge_to: Dict, merge_on: str) -> None:
    merge_from_configs = merge_from.get(merge_on)
    merge_to_configs = merge_to.get(merge_on, {})

    if merge_from_configs is None:
        return

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
