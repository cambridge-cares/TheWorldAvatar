from enum import Enum
from typing import List, Tuple, Dict, Optional
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from abc import ABC, abstractmethod
from pprint import pformat
import logging

logger = logging.getLogger(__name__)


class Handler(ABC):
    """
    The Handler interface provides methods to handle file inputs.
    """

    def __init__(
        self,
        name: str,
        in_stage: Enum,
        out_stage: Enum,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
        required_endpoints_config: Optional[Dict] = None,
        required_handler_kwargs: Optional[List] = None,
        supported_handler_kwargs: Optional[List] = None,
    ) -> None:

        self.name = name
        self._in_stage = in_stage
        self._out_stage = out_stage
        self._endpoints_config = {}
        self._handler_kwargs = {}
        self.written_files = []
        self._endpoints_proxy = endpoints_proxy

        self._required_endpoints_config = required_endpoints_config
        self._required_handler_kwargs = required_handler_kwargs
        self._supported_handler_kwargs = supported_handler_kwargs

    def set_endpoints_proxy(self, endpoints_proxy: endp.Endpoints_proxy) -> None:
        self._endpoints_proxy = endpoints_proxy

    def set_endpoints_config(self, endpoints_config: Optional[Dict] = None) -> None:
        if endpoints_config is not None:
            self._endpoints_config = endpoints_config

    def check_handler_kwargs(self) -> None:
        if self._required_handler_kwargs is not None:
            missing_args = []
            for arg_name in self._required_handler_kwargs:
                if arg_name not in self._handler_kwargs:
                    missing_args.append(arg_name)
            if missing_args:
                raise app_exceptions.MissingHandlerConfig(
                    (
                        f"The required {missing_args} arguments are missing "
                        f"for the {self.name} handler."
                    )
                )

    def check_required_endpoints_config(self) -> None:
        if self._required_endpoints_config is not None:
            for (
                req_config_group,
                req_config_keys,
            ) in self._required_endpoints_config.items():
                config_group = self._endpoints_config.get(req_config_group)
                if config_group is None:
                    raise app_exceptions.MissingHandlerConfig(
                        (
                            f"The required '{req_config_group}' configuration "
                            f"is missing for the {self.name} handler."
                        )
                    )
                else:
                    for req_key in req_config_keys:
                        if req_key not in config_group:
                            raise app_exceptions.MissingHandlerConfig(
                                (
                                    f"The required '{req_config_group}:{req_key}' "
                                    f"configuration is missing for the {self.name} "
                                    "handler."
                                )
                            )

    @property
    def endpoints_config(self) -> Dict:
        return self._endpoints_config

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

        if self._endpoints_proxy is None:
            logger.warning(
                (
                    f"Endpoint proxy not defined for {self.name} handler. "
                    "Skipping any uploads."
                )
            )
        else:
            self._endpoints_proxy.do_uploads(
                inputs=inputs,
                input_type=input_type,
                endpoints_config=self._endpoints_config,
                dry_run=dry_run,
                triple_store_uploads=triple_store_uploads,
                file_server_uploads=file_server_uploads,
            )

    def set_handle_input_kwargs(self, handler_kwargs: Dict) -> None:
        self._handler_kwargs = handler_kwargs

    def update_handle_input_kwargs(self, handler_kwargs: Dict) -> None:
        for key, value in handler_kwargs.items():
            self._handler_kwargs[key] = value

    def info(self) -> None:
        print("--------------------------------------------")
        print(f"handler: {self.name}")
        print(f"in_stages: {self._in_stage}")
        print(f"out_stage: {self._out_stage}")
        print(f"endpoints_config: {pformat(self._endpoints_config)}")
        if self._required_endpoints_config is not None:
            print(
                f"required_endpoints_config: {pformat(self._required_endpoints_config)}"
            )
        if self._supported_handler_kwargs is not None:
            print(
                f"supported_handler_kwargs: {pformat(self._supported_handler_kwargs)}"
            )
        if self._required_handler_kwargs is not None:
            print(f"required_handler_kwargs: {pformat(self._required_handler_kwargs)}")
        print(f"set handler_kwargs: {pformat(self._handler_kwargs)}")

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
