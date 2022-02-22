import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from typing import Dict, List, Optional, Any
from chemaboxwriters.common.handler import IHandler
from enum import Enum
import chemaboxwriters.common.globals as globals
from collections import defaultdict
from chemaboxwriters.common.uploaders import Uploaders
from pyuploader.uploaders.uploader import Uploader
import logging

logger = logging.getLogger(__name__)


class Pipeline:
    """
    The Pipeline interface provides methods to register handlers that can
    process different file inputs. The pipeline acts here as an Observable
    whereas handlers are Observers.
    """

    def __init__(
        self,
        name: str,
        handlers: Optional[List[IHandler]] = None,
        file_server_uploader: Optional[Uploader] = None,
        triple_store_uploader: Optional[Uploader] = None,
    ):
        self.name = name
        self._handlers: List[IHandler] = handlers if handlers is not None else []
        self._uploaders = Uploaders(
            file_server_uploader=file_server_uploader,
            triple_store_uploader=triple_store_uploader,
        )

    def register_handler(
        self,
        handler: IHandler,
        silent: bool = False,
    ) -> Any:

        if not silent:
            logger.info(f"Adding {handler.name} handler.")
        self._handlers.append(handler)
        return self

    @property
    def in_stages(self) -> List[Enum]:
        in_stages = []
        for handler in self._handlers:
            in_stages.extend([x for x in handler.in_stages if x not in in_stages])
        return in_stages

    @property
    def written_files(self) -> List[str]:
        _written_files = []
        for handler in self._handlers:
            if handler.written_files:
                _written_files.extend(handler.written_files)
        return _written_files

    def run(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool = True,
        handler_kwargs: Optional[Dict[str, Any]] = None,
    ):

        logger.info(f"Running the {self.name} pipeline.")

        if input_type not in self.in_stages:
            requestedStage = input_type.name.lower()
            raise app_exceptions.UnsupportedStage(
                f"Error: Stage: '{requestedStage}' is not supported."
            )

        return self._notify_handlers(
            inputs=inputs,
            input_type=input_type,
            out_dir=out_dir,
            dry_run=dry_run,
            handler_kwargs=handler_kwargs,
        )

    def _notify_handlers(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool,
        handler_kwargs: Optional[Dict[str, Any]] = None,
    ):

        if handler_kwargs is None:
            handler_kwargs = {}
        inputs_left_to_process = defaultdict(list)
        inputs_left_to_process[input_type] = inputs

        while inputs_left_to_process:
            if set(inputs_left_to_process.keys()).isdisjoint(set(self.in_stages)):
                break
            input_type = list(inputs_left_to_process.keys())[0]
            inputs = inputs_left_to_process.pop(input_type)

            self.do_uploads(inputs=inputs, input_type=input_type, dry_run=dry_run)

            for handler in self._handlers:
                if input_type in handler.in_stages:
                    this_handler_kwargs = handler_kwargs.get(handler.name, {})
                    this_handler_kwargs[
                        "triple_store_uploads"
                    ] = self._uploaders.triple_store_uploads
                    this_handler_kwargs[
                        "file_server_uploads"
                    ] = self._uploaders.file_server_uploads

                    outputs, output_type = handler.notify(
                        inputs=inputs,
                        out_dir=out_dir,
                        dry_run=dry_run,
                        input_type=input_type,
                        **this_handler_kwargs,
                    )
                    if output_type is not globals.aboxStages.NOT_DEFINED:
                        inputs_left_to_process[output_type].extend(outputs)
                        self._uploaders.do_uploads(
                            inputs=outputs, input_type=output_type, dry_run=dry_run
                        )

    def do_uploads(self, inputs: List[str], input_type: Enum, dry_run: bool) -> None:
        self._uploaders.do_uploads(
            inputs=inputs, input_type=input_type, dry_run=dry_run
        )


def get_pipeline(
    name: str = "",
    handlers: Optional[List[IHandler]] = None,
    file_server_uploader: Optional[Uploader] = None,
    triple_store_uploader: Optional[Uploader] = None,
) -> Pipeline:

    pipeline = Pipeline(
        name=name,
        handlers=handlers,
        file_server_uploader=file_server_uploader,
        triple_store_uploader=triple_store_uploader,
    )
    return pipeline
