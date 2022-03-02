import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
from collections import OrderedDict
from typing import Dict, List, Optional, Any, Tuple
from chemaboxwriters.common.handler import Handler
from enum import Enum
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
import yaml
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

    def check_handlers_config(self, input_type: Optional[Enum] = None) -> None:
        if input_type is None:
            for handler in self._handlers.values():
                handler.check_required_endpoints_config()
                handler.check_handler_kwargs()
        else:
            for handler in self._handlers.values():
                if handler._in_stage == input_type:
                    handler.check_required_endpoints_config()
                    handler.check_handler_kwargs()
                    input_type = handler._out_stage

    def configure_from_file(self, config_file: str) -> None:
        pipeline_config = self._read_config_file(config_file=config_file)
        if pipeline_config:
            self.configure_from_dict(config=pipeline_config)

    def configure_from_dict(self, config: Dict) -> None:
        pipeline_config, handlers_config = self._get_pipeline_handler_configs(
            config=config
        )
        for handler_name, handler in self._handlers.items():
            if handler_name.lower() in handlers_config:
                handlers_config_to_use = handlers_config[handler_name.lower()]
            else:
                handlers_config_to_use = pipeline_config

            handlers_kwargs = handlers_config_to_use.pop(abconf.HANDLER_KWARGS, {})
            handler.set_endpoints_config(handlers_config_to_use)
            handler.set_handle_input_kwargs(handler_kwargs=handlers_kwargs)

    def _get_pipeline_handler_configs(self, config: Dict) -> Tuple[Dict, Dict]:
        if abconf.HANDLERS_CONFIG_KEY in config:
            logger.warning(
                f"Found '{abconf.HANDLERS_CONFIG_KEY}' key in the default config section. This will be omitted."
            )
            config.pop(abconf.HANDLERS_CONFIG_KEY)

        pipeline_configs = config.get(self.name, {})

        merge_on_keys = [
            abconf.UPLOAD_SETTINGS_KEY,
            abconf.QUERY_SETTINGS_KEY,
            abconf.WRITERS_PREFIXES_KEY,
            abconf.HANDLER_KWARGS,
        ]

        for key in merge_on_keys:
            self._merge_config_field(
                merge_from=config,
                merge_to=pipeline_configs,
                merge_on=key,
            )

        handlers_config = pipeline_configs.pop(abconf.HANDLERS_CONFIG_KEY, {})
        handlers = handlers_config.keys()
        for handler in handlers:
            for key in merge_on_keys:
                self._merge_config_field(
                    merge_from=pipeline_configs,
                    merge_to=handlers_config[handler],
                    merge_on=key,
                )

        return pipeline_configs, handlers_config

    def _merge_config_field(
        self, merge_from: Dict, merge_to: Dict, merge_on: str
    ) -> None:
        merge_from_configs = merge_from.get(merge_on, {})
        merge_to_configs = merge_to.get(merge_on, {})

        merge_to_configs = self._merge_configs(
            merge_into=merge_to_configs,
            merge_from=merge_from_configs,
        )

        merge_to[merge_on] = merge_to_configs

    @staticmethod
    def _merge_configs(merge_into: Dict, merge_from: Dict) -> Dict:
        return {**merge_from, **merge_into}

    @staticmethod
    def _read_config_file(config_file: str) -> Dict:
        pipeline_config = {}
        if config_file is not None:
            with open(config_file, "r") as stream:
                pipeline_config = yaml.safe_load(stream)

        return pipeline_config


def get_pipeline(
    name: str = "",
    handlers: Optional[List[Handler]] = None,
    endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
) -> Pipeline:

    if endpoints_proxy is None:
        endpoints_proxy = endp.get_endpoints_proxy()

    pipeline = Pipeline(name=name)
    if handlers is not None:
        for handler in handlers:
            handler.set_endpoints_proxy(endpoints_proxy=endpoints_proxy)
            pipeline.add_handler(handler=handler)
    return pipeline
