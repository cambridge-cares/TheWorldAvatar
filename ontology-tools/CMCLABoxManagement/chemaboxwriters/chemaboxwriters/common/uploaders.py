import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional
from enum import Enum
import chemaboxwriters.common.globals as globals
import os
import yaml
from pyuploader.uploaders.uploader import Uploader
from pyuploader import get_uploader
import logging


logger = logging.getLogger(__name__)


class Uploaders:
    def __init__(
        self,
        config_file: Optional[str] = None,
        file_server_uploader: Optional[Uploader] = None,
        triple_store_uploader: Optional[Uploader] = None,
    ):
        self.file_server_uploader = (
            file_server_uploader
            if file_server_uploader is not None
            else get_uploader("fs_uploader")
        )
        self.triple_store_uploader = (
            triple_store_uploader
            if triple_store_uploader is not None
            else get_uploader("ts_uploader")
        )
        self.file_server_uploads = {}
        self.triple_store_uploads = {}
        self.upload_config = self.get_upload_config(config_file=config_file)

    def get_upload_config(self, config_file: Optional[str] = None) -> Dict:
        upload_configs = {}
        if config_file is None:
            config_file = os.environ[globals.CONFIG_FILE_ENV_VAR]
        with open(config_file, "r") as stream:
            upload_configs = yaml.safe_load(stream)

        self._set_default_upload_specs(upload_configs=upload_configs)
        self._propagate_default_upload_specs(
            upload_configs=upload_configs, upload_to=globals.UPLOAD_TO_FILE_SERVER
        )
        self._propagate_default_upload_specs(
            upload_configs=upload_configs, upload_to=globals.UPLOAD_TO_TRIPLE_STORE
        )
        return upload_configs

    def _set_default_upload_specs(self, upload_configs: Dict) -> None:
        for default_key in globals.CONFIG_FILE_KEYS:
            if default_key not in upload_configs:
                upload_configs[default_key] = None

    def _propagate_default_upload_specs(
        self, upload_configs: Dict, upload_to: str
    ) -> None:

        if upload_to == globals.UPLOAD_TO_FILE_SERVER:
            upload_to_keys = globals.FILE_SERVER_KEYS
        else:
            upload_to_keys = globals.TRIPLE_STORE_KEYS

        upload_spec = upload_configs.get(upload_to)
        if upload_spec is not None:
            for stages in upload_spec.keys():
                for key in upload_to_keys:
                    if key not in upload_spec[stages]:
                        upload_spec[stages][key] = upload_configs[key]
        else:
            upload_configs[upload_to] = {}

    def do_uploads(self, inputs: List[str], input_type: Enum, dry_run: bool) -> None:
        if self._upload_input_type_to_file_server(input_type):
            self._upload_to_file_server(
                inputs=inputs, input_type=input_type, dry_run=dry_run
            )
        if self._upload_input_type_to_triple_store(input_type):
            self._upload_to_triple_store(
                inputs=inputs, input_type=input_type, dry_run=dry_run
            )

    def _upload_input_type_to_file_server(self, input_type: Enum) -> bool:
        return (
            input_type.name.lower() in self.upload_config[globals.UPLOAD_TO_FILE_SERVER]
        )

    def _upload_input_type_to_triple_store(self, input_type: Enum) -> bool:
        return (
            input_type.name.lower()
            in self.upload_config[globals.UPLOAD_TO_TRIPLE_STORE]
        )

    def _upload_to_file_server(
        self, inputs: List[str], input_type: Enum, dry_run: bool
    ):
        this_upload_configs = self.upload_config[globals.UPLOAD_TO_FILE_SERVER].get(
            input_type.name.lower()
        )

        if this_upload_configs is None:
            raise app_exceptions.MissingUploadConfigs(
                f"No file server upload configs specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )

        upload_url = this_upload_configs[globals.FILE_SERVER_UPLOAD_ENDPOINT_KEY]
        upload_auth_file = this_upload_configs[globals.FILE_SERVER_SECRETS_FILE_KEY]
        upload_subirs = this_upload_configs.get(globals.FILE_SERVER_SUBDIR_KEY, "")

        if upload_url is None:
            logger.warning(
                f"No file server upload endpoint specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )
            return

        if upload_auth_file is None:
            logger.warning(
                f"No file server upload secrets file specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )
            return

        for inp_file in inputs:
            uploaded_files_locations = self.file_server_uploader.upload(
                file_or_dir=inp_file,
                url=upload_url,
                auth_file=upload_auth_file,
                subdirs=upload_subirs,
                file_ext="all",
                dry_run=dry_run,
            )

            self.file_server_uploads.update(uploaded_files_locations)

    def _upload_to_triple_store(
        self, inputs: List[str], input_type: Enum, dry_run: bool
    ):

        this_upload_configs = self.upload_config[globals.UPLOAD_TO_TRIPLE_STORE].get(
            input_type.name.lower()
        )

        if this_upload_configs is None:
            logger.warning(
                f"No triple store upload configs specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )
            return

        upload_url = this_upload_configs[globals.TRIPLE_STORE_SPARQL_ENDPOINT_KEY]
        upload_auth_file = this_upload_configs[globals.TRIPLE_STORE_SECRETS_FILE_KEY]

        if upload_url is None:
            logger.warning(
                f"No triple store upload endpoint specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )
            return

        if upload_auth_file is None:
            logger.warning(
                f"No triple store upload secrets file specified for the {input_type.name.lower()} stage. Skipping the upload!"
            )
            return

        for inp_file in inputs:
            uploaded_files_locations = self.triple_store_uploader.upload(
                file_or_dir=inp_file,
                url=upload_url,
                auth_file=upload_auth_file,
                file_ext="owl",
                dry_run=dry_run,
            )
            self.triple_store_uploads.update(uploaded_files_locations)
