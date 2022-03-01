from enum import Enum
from typing import List, Tuple, Dict, Optional
import chemaboxwriters.common.endpoints_config as endp_conf
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
        endpoints_proxy: Optional[endp_conf.Endpoints_proxy] = None,
        required_endpoints_config: Optional[Dict] = None,
    ) -> None:

        self.name = name
        self._in_stage = in_stage
        self._out_stage = out_stage
        self._endpoints_config = {}
        self._handler_kwargs = {}
        self.written_files = []
        self._endpoints_proxy: Optional[endp_conf.Endpoints_proxy] = None
        self._required_endpoints_config = required_endpoints_config

    def set_endpoints_proxy(self, endpoints_proxy: endp_conf.Endpoints_proxy) -> None:
        self._endpoints_proxy = endpoints_proxy

    def set_endpoints_config(self, endpoints_config: Optional[Dict] = None) -> None:
        if endpoints_config is not None:
            self._endpoints_config = endpoints_config

    def check_required_endpoints_config(self) -> None:
        if self._required_endpoints_config is not None:
            for (
                req_config_group,
                req_config_keys,
            ) in self._required_endpoints_config.items():
                config_group = self._endpoints_config.get(req_config_group)
                if config_group is None:
                    app_exceptions.MissingHandlerConfig(
                        f"The required '{config_group}' configuration is missing for handler {self.name}."
                    )
                else:
                    for req_key in req_config_keys:
                        if req_key not in config_group:
                            app_exceptions.MissingHandlerConfig(
                                f"The required '{req_key}' configuration is missing for handler {self.name}."
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
                f"Endpoint proxy not defined for {self.name} handler. Skipping any uploads."
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

    def set_handle_input_kwargs(self, handler_kwargs) -> None:
        self._handler_kwargs = handler_kwargs

    def info(self) -> None:
        logger.info("--------------------------------------------")
        logger.info(f"handler: {self.name}")
        logger.info(f"in_stages: {self._in_stage}")
        logger.info(f"out_stage: {self._out_stage}")
        logger.info(f"endpoints_config: {pformat(self._endpoints_config)}")
        if self._required_endpoints_config is not None:
            logger.info(
                f"required_endpoints_config: {pformat(self._required_endpoints_config)}"
            )
        logger.info(f"handler_kwargs: {pformat(self._handler_kwargs)}")

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
