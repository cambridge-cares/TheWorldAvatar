import yaml
from typing import Dict, Optional
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional, Tuple
from enum import Enum
from pyuploader.uploaders.uploader import Uploader
from pyuploader import get_uploader
import logging

logger = logging.getLogger(__name__)

FILE_SERVER = "file server"
TRIPLE_STORE = "triple store"

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


class Endpoints_proxy:
    def __init__(
        self,
        file_server_uploader: Optional[Uploader] = None,
        triple_store_uploader: Optional[Uploader] = None,
    ) -> None:
        self._file_server_uploader = (
            file_server_uploader
            if file_server_uploader is not None
            else get_uploader("fs_uploader")
        )
        self._triple_store_uploader = (
            triple_store_uploader
            if triple_store_uploader is not None
            else get_uploader("fs_uploader")
        )

    def do_uploads(
        self,
        inputs: List[str],
        input_type: Enum,
        endpoints_config: Dict,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> None:

        if not endpoints_config:
            return

        if self._do_file_server_upload(
            input_type=input_type, endpoints_config=endpoints_config
        ):
            self._upload_to_file_server(
                inputs=inputs,
                input_type=input_type,
                dry_run=dry_run,
                endpoints_config=endpoints_config,
                file_server_uploads=file_server_uploads,
            )
        if self._do_triple_store_upload(
            input_type=input_type, endpoints_config=endpoints_config
        ):
            self._upload_to_triple_store(
                inputs=inputs,
                input_type=input_type,
                dry_run=dry_run,
                endpoints_config=endpoints_config,
                triple_store_uploads=triple_store_uploads,
            )

    @staticmethod
    def _check_connection_configs(
        url: Optional[str],
        auth_file: Optional[str],
        dry_run: bool,
        input_type: Enum,
        upload_type: str,
    ) -> None:
        if url is None:
            logger.warning(
                f"No {upload_type} upload endpoint specified for the {input_type.name.lower()} stage."
            )
            if not dry_run:
                raise app_exceptions.MissingUploadConfigs
            return

        if auth_file is None:
            logger.warning(
                f"No {upload_type} upload secrets file specified for the {input_type.name.lower()} stage."
            )
            if not dry_run:
                raise app_exceptions.MissingUploadConfigs
            return

    def _upload_to_file_server(
        self,
        inputs: List[str],
        input_type: Enum,
        endpoints_config: Dict,
        dry_run: bool,
        file_server_uploads: Optional[Dict] = None,
    ) -> None:

        (
            url,
            auth_file,
            no_auth,
        ) = self._get_file_server_connection_configs(endpoints_config=endpoints_config)
        self._check_connection_configs(
            url=url,
            auth_file=auth_file,
            dry_run=dry_run,
            input_type=input_type,
            upload_type=FILE_SERVER,
        )
        subirs = endpoints_config.get(FILE_SERVER_SUBDIR_KEY, "")

        for inp_file in inputs:
            uploaded_files_locations = self._file_server_uploader.upload(
                file_or_dir=inp_file,
                url=url,
                auth_file=auth_file,
                no_auth=no_auth,
                subdirs=subirs,
                file_ext="all",
                dry_run=dry_run,
            )

            if file_server_uploads is not None:
                file_server_uploads.update(uploaded_files_locations)

    def _upload_to_triple_store(
        self,
        inputs: List[str],
        input_type: Enum,
        endpoints_config: Dict,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
    ) -> None:

        (
            url,
            auth_file,
            no_auth,
        ) = self._get_triple_store_connection_configs(endpoints_config=endpoints_config)
        self._check_connection_configs(
            url=url,
            auth_file=auth_file,
            dry_run=dry_run,
            input_type=input_type,
            upload_type=TRIPLE_STORE,
        )

        for inp_file in inputs:
            uploaded_files_locations = self._triple_store_uploader.upload(
                file_or_dir=inp_file,
                url=url,
                auth_file=auth_file,
                no_auth=no_auth,
                file_ext="owl",
                dry_run=dry_run,
            )

            if triple_store_uploads is not None:
                triple_store_uploads.update(uploaded_files_locations)

    def _get_file_server_connection_configs(self, endpoints_config: Dict) -> Tuple:
        return self._get_endpoint_connection_configs(
            endpoints_config=endpoints_config,
            url_key=FILE_SERVER_UPLOAD_ENDPOINT_KEY,
            auth_key=FILE_SERVER_SECRETS_FILE_KEY,
            no_auth_key=FILE_SERVER_NO_AUTH_KEY,
        )

    def _get_triple_store_connection_configs(self, endpoints_config: Dict) -> Tuple:
        return self._get_endpoint_connection_configs(
            endpoints_config=endpoints_config,
            url_key=TRIPLE_STORE_SPARQL_ENDPOINT_KEY,
            auth_key=TRIPLE_STORE_SECRETS_FILE_KEY,
            no_auth_key=TRIPLE_STORE_NO_AUTH_KEY,
        )

    def _get_endpoint_connection_configs(
        self, endpoints_config: Dict, url_key: str, auth_key: str, no_auth_key: str
    ) -> Tuple:
        url = endpoints_config.get(url_key)
        auth_file = endpoints_config.get(auth_key)
        no_auth = endpoints_config.get(no_auth_key)

        return url, auth_file, no_auth

    def _do_file_server_upload(self, input_type: Enum, endpoints_config: Dict) -> bool:
        upload_stages = endpoints_config.get(UPLOAD_TO_FILE_SERVER_KEY, [])
        return input_type.name.lower() in upload_stages

    def _do_triple_store_upload(self, input_type: Enum, endpoints_config: Dict) -> bool:
        upload_stages = endpoints_config.get(UPLOAD_TO_TRIPLE_STORE_KEY, [])
        return input_type.name.lower() in upload_stages


def get_endpoints_proxy(
    file_server_uploader: Optional[Uploader] = None,
    triple_store_uploader: Optional[Uploader] = None,
) -> Endpoints_proxy:
    return Endpoints_proxy(
        file_server_uploader=file_server_uploader,
        triple_store_uploader=triple_store_uploader,
    )


def get_endpoints_config_file(config_file: str) -> Dict:
    with open(config_file, "r") as stream:
        endpoints_config = yaml.safe_load(stream)
    return endpoints_config


def _read_config_settings(endpoints_config: Dict, config_keys: List[str]) -> Dict:
    return {key: value for key, value in endpoints_config.items() if key in config_keys}


def pre_process_endpoints_config(endpoints_config: Dict, config_key: str) -> Dict:
    # get default configs
    default_configs = {
        key: value
        for key, value in endpoints_config.items()
        if key in DEFAULT_CONFIG_KEYS
    }

    # get pipeline level configs, and merge in the defaults one
    pipeline_configs = _merge_endpoints_configs(
        merge_into=endpoints_config.get(config_key, {}), merge_from=default_configs
    )

    # get handler level configs, and merge in the pipeline configs
    handlers_config = pipeline_configs.pop(HANDLERS_CONFIG_KEY, {})
    if handlers_config:
        for handler_name, configs in handlers_config.items():
            if configs is None:
                configs = {}
            handlers_config[handler_name] = _merge_endpoints_configs(
                merge_into=configs, merge_from=pipeline_configs
            )

        pipeline_configs[HANDLERS_CONFIG_KEY] = handlers_config
    return handlers_config


def _merge_endpoints_configs(merge_into: Dict, merge_from: Dict) -> Dict:
    return {**merge_from, **merge_into}
