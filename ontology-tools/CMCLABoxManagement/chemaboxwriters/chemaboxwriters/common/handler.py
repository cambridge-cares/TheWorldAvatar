from enum import Enum
from typing import List, Tuple, Dict, Optional
from chemaboxwriters.common.endpoints_config import Endpoints_proxy
from abc import ABC, abstractmethod
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
        endpoints_proxy: Optional[Endpoints_proxy] = None,
    ) -> None:

        self.name = name
        self._in_stage = in_stage
        self._out_stage = out_stage
        self._endpoints_config = {}
        self._handler_kwargs = {}
        self.written_files = []
        self._endpoints_proxy: Optional[Endpoints_proxy] = None

    def set_endpoints_proxy(self, endpoints_proxy: Endpoints_proxy) -> None:
        self._endpoints_proxy = endpoints_proxy

    def set_endpoints_config(self, endpoints_config: Optional[Dict] = None) -> None:
        if endpoints_config is not None:
            self._endpoints_config = endpoints_config

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
        logger.info(f"handler: {self.name}")
        logger.info(f"in_stages: {self._in_stage}")
        logger.info(f"out_stage: {self._out_stage}")
        logger.info(f"handler_kwargs: {self._handler_kwargs}")

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
