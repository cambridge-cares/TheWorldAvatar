from chemaboxwriters.common.utilsfunc import getRefName, stage_name_to_enum
from chemaboxwriters.app_exceptions.app_exceptions import (
    UnsupportedStage,
    IncorrectHandlerParameter,
)
from pyuploader import get_uploader
from enum import Enum
import os
from typing import Callable, Dict, List, Tuple, Optional, Any, Union
import logging
from chemaboxwriters.common.handler import Handler

logger = logging.getLogger(__name__)


class Pipeline:
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """

    def __init__(
        self,
        name: str,
        out_stage: Optional[Enum] = None,
    ):
        self.name = name
        self.handlers: Dict[str, Handler] = {}
        # turn into property
        self.writtenFiles: List[str] = []
        # turn into property
        self.in_stages: List[Enum] = []
        self.out_stage = out_stage
        self.out_stage_auto_set = out_stage is None
        self.out_stage_output: Optional[List[str]] = None

    def add_handler(self, handler: Handler, handlerName: Optional[str] = None) -> Any:

        if handlerName is None:
            handlerName = handler.name

        logger.info(f"Adding {handlerName} handler.")

        self.handlers[handlerName] = handler
        self.in_stages.extend([x for x in handler.in_stages if x not in self.in_stages])

        if self.out_stage_auto_set:
            self.out_stage = handler.out_stage
        return self

    def run(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        handler_kwargs: Optional[Dict[str, Any]] = None,
        dry_run: bool = True,
    ) -> Tuple[List[str], Enum]:

        logger.info(f"Running the {self.name} pipeline.")

        if input_type not in self.in_stages:
            requestedStage = input_type.name.lower()
            raise UnsupportedStage(
                f"Error: Stage: '{requestedStage}' is not supported."
            )

        for handler_name, handler in self.handlers.items():
            if input_type in handler.in_stages:
                logger.info(
                    f"Executing the {handler_name} handler on the following inputs {inputs}."
                )

                inputs, input_type = handler.run(
                    inputs=inputs,
                    input_type=input_type,
                    out_dir=out_dir,
                    dry_run=dry_run,
                    handler_kwargs=handler_kwargs,
                )

        return inputs, input_type
