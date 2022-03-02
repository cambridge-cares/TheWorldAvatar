import yaml
from typing import Dict, Optional
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional, Tuple
from enum import Enum
from pyuploader.uploaders.uploader import Uploader
import chemaboxwriters.common.aboxconfig as abconf
from pyuploader import get_uploader
import logging

logger = logging.getLogger(__name__)


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
        no_auth: bool,
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

        if auth_file is None and not no_auth:
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
            no_auth=no_auth,
            dry_run=dry_run,
            input_type=input_type,
            upload_type=abconf.FILE_SERVER,
        )
        upload_configs = endpoints_config.get(abconf.UPLOAD_SETTINGS_KEY, {})
        subdirs = upload_configs.get(abconf.FILE_SERVER_SUBDIR_KEY, "")

        for inp_file in inputs:
            uploaded_files_locations = self._file_server_uploader.upload(
                file_or_dir=inp_file,
                url=url,
                auth_file=auth_file,
                no_auth=no_auth,
                subdirs=subdirs,
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
            no_auth=no_auth,
            dry_run=dry_run,
            input_type=input_type,
            upload_type=abconf.TRIPLE_STORE,
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
            url_key=abconf.FILE_SERVER_UPLOAD_ENDPOINT_KEY,
            auth_key=abconf.FILE_SERVER_SECRETS_FILE_KEY,
            no_auth_key=abconf.FILE_SERVER_NO_AUTH_KEY,
        )

    def _get_triple_store_connection_configs(self, endpoints_config: Dict) -> Tuple:
        return self._get_endpoint_connection_configs(
            endpoints_config=endpoints_config,
            url_key=abconf.TRIPLE_STORE_SPARQL_ENDPOINT_KEY,
            auth_key=abconf.TRIPLE_STORE_SECRETS_FILE_KEY,
            no_auth_key=abconf.TRIPLE_STORE_NO_AUTH_KEY,
        )

    def _get_endpoint_connection_configs(
        self, endpoints_config: Dict, url_key: str, auth_key: str, no_auth_key: str
    ) -> Tuple:
        upload_configs = endpoints_config.get(abconf.UPLOAD_SETTINGS_KEY, {})
        url = upload_configs.get(url_key)
        auth_file = upload_configs.get(auth_key)
        no_auth = upload_configs.get(no_auth_key, False)

        return url, auth_file, no_auth

    def _do_file_server_upload(self, input_type: Enum, endpoints_config: Dict) -> bool:
        upload_configs = endpoints_config.get(abconf.UPLOAD_SETTINGS_KEY, {})
        upload_stages = upload_configs.get(abconf.UPLOAD_TO_FILE_SERVER_KEY, [])
        return input_type.name.lower() in upload_stages

    def _do_triple_store_upload(self, input_type: Enum, endpoints_config: Dict) -> bool:
        upload_configs = endpoints_config.get(abconf.UPLOAD_SETTINGS_KEY, {})
        upload_stages = upload_configs.get(abconf.UPLOAD_TO_TRIPLE_STORE_KEY, [])
        return input_type.name.lower() in upload_stages


def get_endpoints_proxy(
    file_server_uploader: Optional[Uploader] = None,
    triple_store_uploader: Optional[Uploader] = None,
) -> Endpoints_proxy:
    return Endpoints_proxy(
        file_server_uploader=file_server_uploader,
        triple_store_uploader=triple_store_uploader,
    )
