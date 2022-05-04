import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from collections import OrderedDict
from typing import Dict, List, Optional
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.aboxconfig as abconf
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
    ) -> None:
        self.name = name
        self._handlers: Dict[str, Handler] = OrderedDict()
        self._file_server_uploads = {}
        self._triple_store_uploads = {}

    def get_handler_by_name(self, handler_name) -> Optional[Handler]:
        return self._handlers.get(handler_name)

    def get_handler_by_in_stage(self, in_stage: str) -> Optional[Handler]:
        for handler in self._handlers.values():
            if handler._in_stage == in_stage:
                return handler

    def add_handler(
        self,
        handler: Handler,
    ) -> "Pipeline":

        if self.get_handler_by_name(handler.name) is not None:
            logger.warning(f"Handler {handler.name} already exist.")
        else:
            self._handlers[handler.name] = handler
        return self

    @property
    def in_stages(self) -> List[str]:
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

    def clean_written_files(self) -> None:
        for handler in self._handlers.values():
            handler.clean_written_files()

    def run(
        self, inputs: List[str], input_type: str, out_dir: str, dry_run: bool = True
    ) -> None:

        logger.info(f"Running the {self.name} pipeline.")

        self.clean_written_files()
        self.reset_uploads_history()

        if input_type not in self.in_stages:
            raise app_exceptions.UnsupportedStage(
                f"Error: Stage: '{ input_type.lower()}' is not supported."
            )

        return self._run_handlers(
            inputs=inputs, input_type=input_type, out_dir=out_dir, dry_run=dry_run
        )

    def _run_handlers(
        self, inputs: List[str], input_type: str, out_dir: str, dry_run: bool
    ) -> None:

        for handler in self._handlers.values():
            if input_type == handler._in_stage:

                inputs, input_type = handler.run(
                    inputs=inputs,
                    input_type=input_type,
                    out_dir=out_dir,
                    dry_run=dry_run,
                )

    def info(self) -> None:
        print(f"============== Information on {self.name} pipeline ==============")
        print("")
        print("Registered handlers:")
        for handler in self._handlers.values():
            handler.info()
        print("")
        print("=================================================================")

    def set_handlers_parameters(self, handlers_params_config: Dict) -> None:
        for handler_name, params_config in handlers_params_config.items():
            handler = self.get_handler_by_name(handler_name)
            if handler is None:
                continue
            for param_name, param_value in params_config.items():
                handler.set_parameter_value(name=param_name, value=param_value)

    def check_handlers_configs(self, input_type: Optional[str] = None) -> None:
        if input_type is None:
            for handler in self._handlers.values():
                handler.check_configs()
        else:
            for handler in self._handlers.values():
                if handler._in_stage == input_type:
                    handler.check_configs()
                    input_type = handler._out_stage

    def configure_from_file(self, config_file: str) -> None:
        config_dict = abconf.read_config_file(config_file=config_file)
        if config_dict:
            self.configure_from_dict(config_dict=config_dict)

    def configure_from_dict(self, config_dict: Dict) -> None:
        abconf.cascade_configs(configs=config_dict)
        pipeline_configs = config_dict.get(self.name)
        if pipeline_configs is not None:
            for handler in self._handlers.values():
                handler_configs = pipeline_configs.get(handler.name.lower())
                handler_configs = (
                    handler_configs if handler_configs is not None else pipeline_configs
                )
                handler.configure_from_dict(config_dict=handler_configs)

    def reset_uploads_history(self) -> None:
        for handler in self._handlers.values():
            handler.init_fs_uploads_history(
                uploads_history=self._file_server_uploads,
            )
            handler.init_ts_uploads_history(
                uploads_history=self._triple_store_uploads,
            )


def get_pipeline(name: str = "", handlers: Optional[List[Handler]] = None) -> Pipeline:

    pipeline = Pipeline(name=name)
    if handlers is not None:
        for handler in handlers:
            pipeline.add_handler(handler=handler)
    return pipeline
