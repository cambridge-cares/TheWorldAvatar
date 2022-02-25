import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from collections import OrderedDict
from typing import Dict, List, Optional, Any
from chemaboxwriters.common.handler import Handler
from enum import Enum
from chemaboxwriters.common.endpoints_config import Endpoints_proxy, get_endpoints_proxy
import logging

logger = logging.getLogger(__name__)


class Pipeline:
    """
    The Pipeline interface provides methods to add handlers that can
    process different file inputs.
    """

    def __init__(
        self,
        name: str,
    ):
        self.name = name
        self._handlers: Dict[str, Handler] = OrderedDict()
        self._file_server_uploads = {}
        self._triple_store_uploads = {}

    def get_handler_by_name(self, handler_name) -> Optional[Handler]:
        return self._handlers.get(handler_name)

    def add_handler(
        self,
        handler: Handler,
        silent: bool = False,
    ) -> Any:

        if self.get_handler_by_name(handler.name) is not None:
            logger.warning(f"Handler {handler.name} already exist.")
        else:
            self._handlers[handler.name] = handler
        return self

    @property
    def in_stages(self) -> List[Enum]:
        in_stages = []
        for handler in self._handlers.values():
            if handler._in_stage not in in_stages:
                in_stages.append(handler._in_stage)
        return in_stages

    @property
    def written_files(self) -> List[str]:
        _written_files = []
        for handler in self._handlers.values():
            if handler.written_files:
                _written_files.extend(handler.written_files)
        return _written_files

    def set_handlers_kwargs(self, handlers_kwargs: Dict) -> None:
        for handler_name, handler_kwargs in handlers_kwargs.items():
            handler = self.get_handler_by_name(handler_name)
            if handler is not None:
                handler.set_handle_input_kwargs(handler_kwargs=handler_kwargs)
            else:
                logger.warning(
                    f"Could not set handler_kwargs for the {handler_name} handler. Handler does not exist."
                )

    def clean_written_files(self) -> None:
        for handler in self._handlers.values():
            handler.clean_written_files()

    def run(
        self, inputs: List[str], input_type: Enum, out_dir: str, dry_run: bool = True
    ):

        logger.info(f"Running the {self.name} pipeline.")

        self.clean_written_files()

        if input_type not in self.in_stages:
            raise app_exceptions.UnsupportedStage(
                f"Error: Stage: '{ input_type.name.lower()}' is not supported."
            )

        return self._run_handlers(
            inputs=inputs, input_type=input_type, out_dir=out_dir, dry_run=dry_run
        )

    def _run_handlers(
        self, inputs: List[str], input_type: Enum, out_dir: str, dry_run: bool
    ):

        for handler in self._handlers.values():
            if input_type == handler._in_stage:

                inputs, input_type = handler.handle_input(
                    inputs=inputs,
                    input_type=input_type,
                    out_dir=out_dir,
                    dry_run=dry_run,
                    triple_store_uploads=self._triple_store_uploads,
                    file_server_uploads=self._file_server_uploads,
                )

    def info(self) -> None:
        logger.info(
            f"============== Information on {self.name} pipeline =============="
        )
        logger.info("")
        logger.info("Registered handlers:")
        for handler in self._handlers.values():
            handler.info()
        logger.info("")
        logger.info(
            f"================================================================="
        )


def get_pipeline(
    name: str = "",
    handlers: Optional[List[Handler]] = None,
    endpoints_config: Optional[Dict] = None,
    endpoints_proxy: Optional[Endpoints_proxy] = None,
) -> Pipeline:

    if endpoints_config is None:
        endpoints_config = {}

    if endpoints_proxy is None:
        endpoints_proxy = get_endpoints_proxy()

    pipeline = Pipeline(name=name)
    if handlers is not None:
        for handler in handlers:
            handler.set_endpoints_config(
                endpoints_config=endpoints_config.get(handler.name.lower())
            )
            handler.set_endpoints_proxy(endpoints_proxy=endpoints_proxy)
            pipeline.add_handler(handler=handler)
    return pipeline
