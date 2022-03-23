from enum import Enum
from typing import List, Tuple, Dict, Optional, Literal, Callable
from chemaboxwriters.common.uploaders import (
    UploaderClient,
    get_file_server_uploader,
    get_triple_store_uploader,
)
from chemaboxwriters.kgoperations.remotestore_client import (
    RemoteStoreClientContainer,
    get_store_client_container,
)
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from abc import ABC, abstractmethod

from pprint import pformat
import logging

logger = logging.getLogger(__name__)

Handler_Arg = Literal["prefix", "parameter"]


class Handler(ABC):
    """
    The Handler interface provides methods to handle file inputs.
    """

    def __init__(
        self,
        name: str,
        in_stage: Enum,
        out_stage: Enum,
        prefixes: Optional[Dict] = None,
        handler_params: Optional[Dict] = None,
    ) -> None:

        self.name = name
        self.written_files = []
        self._in_stage = in_stage
        self._out_stage = out_stage
        self._handler_prefixes: Dict = {}
        if prefixes is not None:
            self.register_handler_prefixes_from_dict(prefixes=prefixes)
        self._handler_params: Dict = {}
        if handler_params is not None:
            self.register_handler_parameters_from_dict(paramaters=handler_params)

        self._file_server_uploader: Optional[UploaderClient] = None
        self._triple_store_uploader: Optional[UploaderClient] = None
        self._remote_store_client: Optional[RemoteStoreClientContainer] = None

    def set_file_server_uploader(self, file_server_uploader: UploaderClient) -> None:
        self._file_server_uploader = file_server_uploader

    def set_triple_store_uploader(self, triple_store_uploader: UploaderClient) -> None:
        self._triple_store_uploader = triple_store_uploader

    def set_remote_store_client(
        self, remote_store_client: RemoteStoreClientContainer
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

    def register_handler_parameters_from_dict(self, paramaters: Dict) -> None:
        self._register_handler_args_from_dict(
            arg_type="parameter", args_dict=paramaters
        )

    def register_handler_prefixes_from_dict(self, prefixes: Dict) -> None:
        self._register_handler_args_from_dict(arg_type="prefix", args_dict=prefixes)

    def register_handler_parameter(
        self, name: str, value: Optional[str] = None, required: bool = False
    ) -> None:

        self._register_handler_arg(
            arg_type="parameter", name=name, value=value, required=required
        )

    def register_handler_prefix(
        self, name: str, value: Optional[str] = None, required: bool = False
    ) -> None:

        self._register_handler_arg(
            arg_type="prefix", name=name, value=value, required=required
        )

    def get_handler_parameter_value(self, name: str) -> str:
        return self._get_handler_arg_value(arg_type="parameter", name=name)

    def get_handler_prefix_value(self, name: str) -> str:
        return self._get_handler_arg_value(arg_type="prefix", name=name)

    def set_handler_parameter_value(
        self, name: str, value: Optional[str] = None
    ) -> None:

        self._set_handler_arg_value(arg_type="parameter", name=name, value=value)

    def set_handler_prefix_value(self, name: str, value: Optional[str] = None) -> None:
        self._set_handler_arg_value(arg_type="prefix", name=name, value=value)

    def is_handler_parameter_required(self, name: str) -> bool:
        return self._is_handler_arg_required(arg_type="parameter", name=name)

    def is_handler_prefix_required(self, name: str) -> bool:
        return self._is_handler_arg_required(arg_type="prefix", name=name)

    def check_configs(self) -> bool:
        for prefix in self._handler_prefixes:
            if (
                self.is_handler_prefix_required(name=prefix)
                and self.get_handler_prefix_value(name=prefix) is None
            ):
                return False
        for param in self._handler_params:
            if (
                self.is_handler_parameter_required(name=param)
                and self.get_handler_parameter_value(name=param) is None
            ):
                return False
        return True

    def _register_handler_args_from_dict(
        self, arg_type: Handler_Arg, args_dict: Dict
    ) -> None:
        register_method = self._get_handler_args_register_method(arg_type=arg_type)

        for param_name, param_settings in args_dict.items():
            register_method(
                name=param_name,
                required=param_settings["required"],
                value=param_settings.get("value"),
            )

    def _register_handler_arg(
        self,
        arg_type: Handler_Arg,
        name: str,
        value: Optional[str] = None,
        required: bool = False,
    ) -> None:
        arg_config = {"value": value, "required": required}
        if arg_type == "parameter":
            self._handler_params[name] = arg_config
        else:
            self._handler_prefixes[name] = arg_config

    def _set_handler_arg_value(
        self,
        arg_type: Handler_Arg,
        name: str,
        value: Optional[str] = None,
    ) -> None:

        handler_args_dict = self._get_handler_args_dict(arg_type=arg_type)
        if name not in handler_args_dict:
            handler_arg_register_method = self._get_handler_args_register_method(
                arg_type=arg_type
            )
            handler_arg_register_method(name)

        handler_args_dict[name]["value"] = value

    def _get_handler_arg_value(
        self,
        arg_type: Handler_Arg,
        name: str,
    ) -> str:

        handler_args_dict = self._get_handler_args_dict(arg_type=arg_type)

        if name not in handler_args_dict:
            raise app_exceptions.MissingHandlerConfig(
                (
                    f"Parameter {name} does not exist. "
                    f"Please register it first via register_handler_{arg_type} method."
                )
            )

        value = handler_args_dict[name]["value"]

        if value is None and self._is_handler_arg_required(
            arg_type=arg_type, name=name
        ):
            raise app_exceptions.MissingRequiredInput(
                (f"Parameter '[{arg_type}][{name}]' is required and must be provided.")
            )

        return handler_args_dict[name]["value"]

    def _get_handler_args_dict(self, arg_type: Handler_Arg) -> Dict:
        if arg_type == "parameter":
            return self._handler_params
        elif arg_type == "prefix":
            return self._handler_prefixes
        else:
            raise app_exceptions.WrongHandlerArgType(
                (
                    f"Wrong handler argument type '{arg_type}'. Choose between "
                    "'prefixes' and 'parameter' type."
                )
            )

    def _get_handler_args_register_method(self, arg_type: Handler_Arg) -> Callable:
        if arg_type == "parameter":
            return self.register_handler_parameter
        elif arg_type == "prefix":
            return self.register_handler_prefix
        else:
            raise app_exceptions.WrongHandlerArgType(
                (
                    f"Wrong handler argument type '{arg_type}'. Choose between "
                    "'prefixes' and 'parameter' type."
                )
            )

    def _is_handler_arg_required(self, arg_type: Handler_Arg, name: str) -> bool:
        handler_args_dict = self._get_handler_args_dict(arg_type=arg_type)
        if name not in handler_args_dict:
            raise app_exceptions.MissingHandlerConfig(
                (
                    f"Parameter {name} does not exist. "
                    f"Please register it first via register_handler_{arg_type} method."
                )
            )
        return handler_args_dict[name]["required"]

    def handle_input(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> Tuple[List[str], Enum]:

        self.do_uploads(
            inputs=inputs,
            input_type=input_type,
            dry_run=dry_run,
            triple_store_uploads=triple_store_uploads,
            file_server_uploads=file_server_uploads,
        )

        outputs = self._handle_input(
            inputs=inputs,
            out_dir=out_dir,
            input_type=input_type,
            dry_run=dry_run,
            triple_store_uploads=triple_store_uploads,
            file_server_uploads=file_server_uploads,
        )
        self.written_files.extend(outputs)

        self.do_uploads(
            inputs=outputs,
            input_type=self._out_stage,
            dry_run=dry_run,
            triple_store_uploads=triple_store_uploads,
            file_server_uploads=file_server_uploads,
        )

        return outputs, self._out_stage

    def do_uploads(
        self,
        inputs: List[str],
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> None:

        if self._file_server_uploader is not None:
            self._file_server_uploader.do_uploads(
                inputs=inputs,
                input_type=input_type.name.lower(),
                dry_run=dry_run,
                uploads=file_server_uploads,
            )
        if self._triple_store_uploader is not None:
            self._triple_store_uploader.do_uploads(
                inputs=inputs,
                input_type=input_type.name.lower(),
                dry_run=dry_run,
                uploads=triple_store_uploads,
            )

    def __str__(self) -> None:
        print("----------------------------------------------------------------------")
        print(f"handler: {self.name}")
        print(f"in_stages: {self._in_stage}")
        print(f"out_stage: {self._out_stage}")
        print("prefixes:")
        print(pformat(self._handler_prefixes))
        print("handler_kwargs:")
        print(pformat(self._handler_params))
        if self._file_server_uploader is not None:
            self._file_server_uploader.__str__()
        if self._triple_store_uploader is not None:
            self._triple_store_uploader.__str__()
        if self._remote_store_client is not None:
            self._remote_store_client.__str__()

    def clean_written_files(self) -> None:
        self.written_files = []

    @abstractmethod
    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        dry_run: bool,
        input_type: Enum,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> List[str]:
        pass

    def configure_from_dict(self, config_dict: Dict) -> None:
        file_server_upload_settings = config_dict.get("file_server_upload_settings")
        triple_store_upload_settings = config_dict.get("triple_store_upload_settings")
        query_endpoints = config_dict.get("kg_query_endpoints")
        prefixes = config_dict.get("prefixes")
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

        if prefixes is not None:
            for name, value in prefixes.items():
                self.set_handler_prefix_value(name=name, value=value)

        if handler_kwargs is not None:
            for name, value in handler_kwargs.items():
                self.set_handler_parameter_value(name=name, value=value)

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
        fs_uploader = get_file_server_uploader(
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
        fs_uploader = get_triple_store_uploader(
            upload_file_types=upload_file_types,
            url=url,
            auth_file=auth_file,
            no_auth=no_auth,
        )
        self.set_triple_store_uploader(fs_uploader)

    def _configure_remote_store_client_from_dict(self, query_endpoints: Dict) -> None:
        store_client = get_store_client_container(query_endpoints=query_endpoints)
        self.set_remote_store_client(store_client)
