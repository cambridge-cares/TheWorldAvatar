from chemaboxwriters.app_exceptions.app_exceptions import UnsupportedStage
from typing import Dict, List, Tuple, Optional, Any, Union
import logging
from chemaboxwriters.common.handler import IHandler
from enum import Enum
import chemaboxwriters.common.globals as globals


logger = logging.getLogger(__name__)


class Pipeline(IHandler):
    """
    The Pipeline interface declares a method for building the chain of handlers.
    It also declares a method for executing a request.
    """

    def __init__(self, name: str, out_stage: Optional[Enum] = None):
        self.name = name
        self.handlers: Dict[str, IHandler] = {}
        self._out_stage = out_stage

    def add_handler(self, handler: IHandler, handlerName: Optional[str] = None) -> Any:

        if handlerName is None:
            handlerName = handler.name

        logger.info(f"Adding {handlerName} handler.")
        self.handlers[handlerName] = handler

        return self

    @property
    def in_stages(self) -> List[Enum]:
        in_stages = []
        for handler in self.handlers.values():
            in_stages.extend([x for x in handler.in_stages if x not in in_stages])
        return in_stages

    @property
    def out_stage(self) -> Enum:
        if self._out_stage is not None:
            return self._out_stage
        if self.handlers:
            return list(self.handlers.values())[-1].out_stage
        return globals.aboxStages.NOT_DEFINED

    def run(
        self,
        inputs: List[str],
        input_type: Enum,
        out_dir: str,
        dry_run: bool = True,
        handler_kwargs: Optional[Dict[str, Any]] = None,
    ) -> Tuple[List[str], Enum]:

        logger.info(f"Running the {self.name} pipeline.")

        if input_type not in self.in_stages:
            requestedStage = input_type.name.lower()
            raise UnsupportedStage(
                f"Error: Stage: '{requestedStage}' is not supported."
            )

        outputs = inputs
        output_type = input_type

        if handler_kwargs is None:
            handler_kwargs = {}

        for handler_name, handler in self.handlers.items():

            if input_type in handler.in_stages:
                logger.info(
                    f"Executing the {handler_name} handler on the following inputs {inputs}."
                )

                if self.out_stage == input_type:
                    outputs = inputs
                    output_type = input_type

                _handler_kwargs = handler_kwargs.get(handler_name, {})
                outputs_, output_type_ = handler.execute(
                    inputs=inputs,
                    input_type=input_type,
                    out_dir=out_dir,
                    dry_run=dry_run,
                    **_handler_kwargs,
                )

                if self.out_stage == output_type_:
                    outputs = outputs_
                    output_type = output_type_

                inputs = outputs_
                input_type = output_type_

        return outputs, output_type

    @property
    def written_files(self) -> List[str]:
        _written_files = []
        for handler in self.handlers.values():
            if handler.written_files:
                _written_files.extend(handler.written_files)
        return _written_files

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        dry_run: bool,
        input_type: Enum,
        **handler_kwargs,
    ) -> List[str]:

        outputs, _ = self.run(
            inputs=inputs,
            input_type=input_type,
            out_dir=out_dir,
            dry_run=dry_run,
            handler_kwargs=handler_kwargs,
        )

        return outputs


def get_pipeline(name: str = "", out_stage: Optional[Enum] = None) -> Pipeline:

    pipeline = Pipeline(name=name, out_stage=out_stage)
    return pipeline
