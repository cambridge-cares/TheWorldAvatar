import chemaboxwriters.common.uploaders as uploaders
import chemaboxwriters.kgoperations.remotestore_client as rsc
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from abc import ABC, abstractmethod
from pprint import pformat
from typing import List, Tuple, Dict, Optional, Any, Union
import logging

__doc__ = """
This module stores the base Handler implementation.
"""

logger = logging.getLogger(__name__)


class Handler_Parameters:
    """Helper / support class handling Handler parameters."""

    def __init__(self, name: str, handler_name: str) -> None:
        self.name = name
        self.handler_name = handler_name
        self._parameters = {}

    def add_parameter(
        self,
        name: str,
        value: Optional[str] = None,
        required: bool = False,
    ) -> None:
        if name in self._parameters:
            logger.warning(
                f"{self.handler_name} handler {self.name}: {name} already exists."
            )
            return
        self._parameters[name] = {"value": value, "required": required}

    def add_parameters_from_dict(self, parameters_dict: Dict) -> None:
        for param_name, param_settings in parameters_dict.items():
            self.add_parameter(
                name=param_name,
                required=param_settings.get("required"),
                value=param_settings.get("value"),
            )

    def get_parameter_value(
        self,
        name: str,
    ) -> Optional[str]:

        if name not in self._parameters:
            return

        return self._parameters[name]["value"]

    def set_parameter_value(self, name: str, value: Optional[str]) -> None:
        if name not in self._parameters:
            self.add_parameter(name=name, value=value, required=False)
            return

        self._parameters[name]["value"] = value

    def is_parameter_required(self, name: str) -> bool:
        if name not in self._parameters:
            return False
        return self._parameters[name]["required"]

    def check_configs(self) -> None:
        for parameter in self._parameters:
            if (
                self.is_parameter_required(name=parameter)
                and self.get_parameter_value(name=parameter) is None
            ):

                raise app_exceptions.MissingRequiredInput(
                    (f"Missing required {self.name} in {self.handler_name} handler.")
                )

    def info(self) -> None:
        print(pformat(self._parameters))


class Handler(ABC):
    """
    Base Handler implementation. Each specific handler should inherit from it
    and override the handle_input method.
    """

    def __init__(
        self,
        name: str,
        in_stage: str,
        out_stage: str,
        handler_params: Optional[Dict] = None,
    ) -> None:

        self.name = name
        self.written_files = []
        self._in_stage = in_stage
        self._out_stage = out_stage
        self._handler_params = Handler_Parameters(
            name="parameter", handler_name=self.name
        )
        if handler_params is not None:
            self._handler_params.add_parameters_from_dict(
                parameters_dict=handler_params
            )

        self._file_server_uploader: Optional[uploaders.UploaderClient] = None
        self._triple_store_uploader: Optional[uploaders.UploaderClient] = None
        self._remote_store_client: Optional[rsc.RemoteStoreClientContainer] = None

    def set_file_server_uploader(
        self, file_server_uploader: uploaders.UploaderClient
    ) -> None:
        self._file_server_uploader = file_server_uploader

    def set_triple_store_uploader(
        self, triple_store_uploader: uploaders.UploaderClient
    ) -> None:
        self._triple_store_uploader = triple_store_uploader

    def set_remote_store_client(
        self, remote_store_client: rsc.RemoteStoreClientContainer
    ) -> None:
        self._remote_store_client = remote_store_client

    def get_file_server_upload_configs(self) -> Optional[Dict]:
        if self._file_server_uploader is None:
            return

        return self._file_server_uploader.get_upload_configs()

    def get_triple_store_upload_configs(self) -> Optional[Dict]:
        if self._triple_store_uploader is None:
            return

        return self._triple_store_uploader.get_upload_configs()

    def get_parameter_value(
        self, name: str, default: Union[str, bool, None] = None
    ) -> Union[str, bool, None]:
        value = self._handler_params.get_parameter_value(name=name)
        if value is not None:
            return value
        return default

    def set_parameter_value(self, name: str, value: Optional[str] = None) -> None:
        self._handler_params.set_parameter_value(name=name, value=value)

    def is_parameter_required(self, name: str) -> bool:
        return self._handler_params.is_parameter_required(name=name)

    def check_configs(self) -> None:
        """Checks if required handler configs been set.
        Raises the MissingRequiredInput exception if that is not the case.
        """
        self._handler_params.check_configs()

    def init_fs_uploads_history(
        self,
        uploads_history: Dict,
    ) -> None:
        """This sets the handlers file server upload_history dict. Once set, all handlers
        uploads will be added to that dictionary. This method is currently called
        when pipeline is assembled passing pipeline upload_history dict. This allows
        for all handlers to write the uploads to a common place so that each handler
        can see the uploads done by all other handlers.
        """
        self._init_uploads_history(
            uploader=self._file_server_uploader, uploads_history=uploads_history
        )

    def init_ts_uploads_history(
        self,
        uploads_history: Dict,
    ) -> None:
        """This sets the handlers triple store upload_history dict. Once set, all handlers
        uploads will be added to that dictionary. This method is currently called
        when pipeline is assembled passing pipeline upload_history dict. This allows
        for all handlers to write the uploads to a common place so that each handler
        can see the uploads done by all other handlers.
        """
        self._init_uploads_history(
            uploader=self._triple_store_uploader, uploads_history=uploads_history
        )

    @staticmethod
    def _init_uploads_history(
        uploader: Optional[uploaders.UploaderClient],
        uploads_history: uploaders.Upload_History,
    ) -> None:
        if uploader is None or uploads_history is None:
            return

        uploader.init_uploads_history(uploads_history=uploads_history)

    def get_fs_upload_location(
        self, upload_file: str, upload_stage: Optional[str] = None
    ) -> Optional[str]:
        """Retrieves the file server upload location of a given file
        taking its name and stage info.
        """
        location = self._get_upload_location(
            uploader=self._file_server_uploader,
            upload_file=upload_file,
            upload_stage=upload_stage,
        )
        return location

    def get_ts_upload_location(
        self, upload_file: str, upload_stage: Optional[str] = None
    ) -> Optional[str]:
        """Retrieves the triple store upload location of a given file
        taking its name and stage info.
        """

        return self._get_upload_location(
            uploader=self._triple_store_uploader,
            upload_file=upload_file,
            upload_stage=upload_stage,
        )

    @staticmethod
    def _get_upload_location(
        uploader: Optional[uploaders.UploaderClient],
        upload_file: str,
        upload_stage: Optional[str] = None,
    ) -> Optional[str]:

        if uploader is None:
            return

        return uploader.get_upload_location(
            upload_file=upload_file, upload_stage=upload_stage
        )

    def run(
        self,
        inputs: List[str],
        input_type: str,
        out_dir: str,
        dry_run: bool,
    ) -> Tuple[List[str], str]:
        """Runs the handler on a given set of inputs."""

        # check if all required configs are set, if not, this will raise an exception
        self.check_configs()

        # attempt to upload inputs to file server and triple store
        # do_fs/ts_uploads function checks if files should be uploaded
        self.do_fs_uploads(inputs=inputs, input_type=input_type, dry_run=dry_run)
        self.do_ts_uploads(inputs=inputs, input_type=input_type, dry_run=dry_run)

        # main call that handles inputs
        outputs = self.handle_input(inputs=inputs, out_dir=out_dir, dry_run=dry_run)
        # add produced files to the handlers written_files log
        self.written_files.extend(outputs)

        # attempt to upload outputs to file server and triple store
        # do_fs/ts_uploads function checks if files should be uploaded
        self.do_fs_uploads(inputs=outputs, input_type=self._out_stage, dry_run=dry_run)
        self.do_ts_uploads(inputs=outputs, input_type=self._out_stage, dry_run=dry_run)

        return outputs, self._out_stage

    def do_all_uploads(
        self,
        inputs: List[str],
        input_type: str,
        dry_run: bool,
    ) -> Tuple[uploaders.Upload_History, uploaders.Upload_History]:

        fs_uploads = self.do_fs_uploads(
            inputs=inputs, input_type=input_type, dry_run=dry_run
        )
        ts_uploads = self.do_ts_uploads(
            inputs=inputs, input_type=input_type, dry_run=dry_run
        )
        return fs_uploads, ts_uploads

    def do_fs_uploads(
        self,
        inputs: List[str],
        input_type: str,
        dry_run: bool,
    ) -> Dict[str, Dict[str, str]]:

        uploads = {}
        if self._file_server_uploader is not None:
            uploads = self._file_server_uploader.do_uploads(
                inputs=inputs,
                input_type=input_type,
                dry_run=dry_run,
            )
        return uploads

    def do_ts_uploads(
        self,
        inputs: List[str],
        input_type: str,
        dry_run: bool,
    ) -> Dict[str, Dict[str, str]]:

        uploads = {}
        if self._triple_store_uploader is not None:
            uploads = self._triple_store_uploader.do_uploads(
                inputs=inputs,
                input_type=input_type,
                dry_run=dry_run,
            )
        return uploads

    def info(self) -> None:
        """Prints handlers info."""

        print("----------------------------------------------------------------------")
        print(f"handler: {self.name}")
        print(f"in_stages: {self._in_stage}")
        print(f"out_stage: {self._out_stage}")
        print("handler_kwargs:")
        print(pformat(self._handler_params.info()))
        if self._file_server_uploader is not None:
            self._file_server_uploader.info()
        if self._triple_store_uploader is not None:
            self._triple_store_uploader.info()
        if self._remote_store_client is not None:
            self._remote_store_client.info()

    def clean_written_files(self) -> None:
        """Resets handlers written_files log."""
        self.written_files = []

    def do_remote_store_query(
        self,
        endpoint_prefix: str,
        query_str: str,
        store_client_class: rsc.TRemoteStoreClient = rsc.JPSRemoteStoreClient,
    ) -> List[Dict[str, Any]]:
        """Proxy that handles any KG queries. It is possible to pass the type
           of the remote store client. By default it uses the JPS client, but
           some queries require Python's SparqlWrapper client.
        """

        results = []
        if self._remote_store_client is None:
            return results

        try:
            store_client = self._remote_store_client.get_store_client(
                endpoint_prefix=endpoint_prefix,
                store_client_class=store_client_class,
            )
            results = store_client.execute_query(
                query_str=query_str,
            )
        except app_exceptions.MissingQueryEndpoint:
            logger.warning(
                (
                    f"Query unsuccessful. Query endpoint "
                    f"{endpoint_prefix} is not defined."
                )
            )
        finally:
            return results

    @abstractmethod
    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        dry_run: bool,
        *args,
        **kwargs,
    ) -> List[str]:
        pass

    def configure_from_dict(self, config_dict: Dict) -> None:
        """Configre handler params from dictionary"""

        file_server_upload_settings = config_dict.get("file_server_upload_settings")
        triple_store_upload_settings = config_dict.get("triple_store_upload_settings")
        query_endpoints = config_dict.get("kg_query_endpoints")
        handler_kwargs = config_dict.get("handler_kwargs")

        if file_server_upload_settings is not None:
            self._configure_file_server_uploader_from_dict(
                config_dict=file_server_upload_settings
            )

        if triple_store_upload_settings is not None:
            self._configure_triple_store_uploader_from_dict(
                config_dict=triple_store_upload_settings
            )

        if query_endpoints is not None:
            self._configure_remote_store_client_from_dict(
                query_endpoints=query_endpoints
            )

        if handler_kwargs is not None:
            for name, value in handler_kwargs.items():
                self.set_parameter_value(name=name, value=value)

    def _configure_file_server_uploader_from_dict(self, config_dict: Dict) -> None:
        url = config_dict.get("url")
        auth_file = config_dict.get("auth_file")
        no_auth = config_dict.get("no_auth", False)
        subdirs = config_dict.get("subdirs")
        upload_file_types = config_dict.get("upload_file_types", [])

        if url is None:
            raise app_exceptions.MissingUploadEndpoint(
                "Missing file server uploader url setting."
            )
        if no_auth is False and auth_file is None:
            raise app_exceptions.MissingUploadEndpointAuthorisation(
                "Missing file server secrets file path."
            )
        fs_uploader = uploaders.get_file_server_uploader(
            upload_file_types=upload_file_types,
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
            subdirs=subdirs,
        )
        self.set_file_server_uploader(fs_uploader)

    def _configure_triple_store_uploader_from_dict(self, config_dict: Dict) -> None:
        url = config_dict.get("url")
        auth_file = config_dict.get("auth_file")
        no_auth = config_dict.get("no_auth", False)
        upload_file_types = config_dict.get("upload_file_types", [])

        if url is None:
            raise app_exceptions.MissingUploadEndpoint(
                "Missing triple store uploader url setting."
            )
        if no_auth is False and auth_file is None:
            raise app_exceptions.MissingUploadEndpointAuthorisation(
                "Missing triple store secrets file path."
            )
        fs_uploader = uploaders.get_triple_store_uploader(
            upload_file_types=upload_file_types,
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
        )
        self.set_triple_store_uploader(fs_uploader)

    def _configure_remote_store_client_from_dict(self, query_endpoints: Dict) -> None:
        store_client = rsc.get_store_client_container(query_endpoints=query_endpoints)
        self.set_remote_store_client(store_client)
