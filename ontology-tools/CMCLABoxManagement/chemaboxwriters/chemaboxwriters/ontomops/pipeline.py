from chemaboxwriters.common.pipeline import Pipeline
import chemaboxwriters.app_exceptions.app_exceptions as app_exceptions
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontomops.handlers import (
    OMINP_JSON_TO_OM_JSON_Handler,
    OM_JSON_TO_OM_CSV_Handler,
)
from typing import List, Dict, Any, Optional
from enum import Enum
import json
import logging

logger = logging.getLogger(__name__)

OMOPS_PIPELINE = "ontomops"


class OMOPS_Pipeline(Pipeline):
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

        if input_type == globals.aboxStages.OMINP_JSON:
            xyz_inputs = self._extract_XYZ_data(inputs)
            if xyz_inputs:
                self.do_uploads(
                    inputs=xyz_inputs,
                    input_type=globals.aboxStages.OMINP_XYZ,
                    dry_run=dry_run,
                )

        return self._notify_handlers(
            inputs=inputs,
            input_type=input_type,
            out_dir=out_dir,
            dry_run=dry_run,
            handler_kwargs=handler_kwargs,
        )

    def _extract_XYZ_data(self, inputs: List[str]):
        xyz_file_paths = []
        for file_path in inputs:
            with open(file_path, "r") as file_handle:
                data = json.load(file_handle)
                xyz_file = data.get("Mops_XYZ_coordinates_file")
                if xyz_file is not None:
                    if xyz_file not in xyz_file_paths:
                        xyz_file_paths.append(xyz_file)

        return xyz_file_paths


def assemble_omops_pipeline(
    config_file: Optional[str] = None, silent: bool = False
) -> OMOPS_Pipeline:

    if not silent:
        logger.info(f"Assembling {OMOPS_PIPELINE} pipeline.")

    pipeline = OMOPS_Pipeline(name=OMOPS_PIPELINE, config_file=config_file)

    # pipeline.add_handler(handler=OMINP_XYZ_Handler(), silent=silent)
    pipeline.register_handler(handler=OMINP_JSON_TO_OM_JSON_Handler(), silent=silent)
    pipeline.register_handler(handler=OM_JSON_TO_OM_CSV_Handler(), silent=silent)
    pipeline.register_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OM_CSV_TO_OM_OWL",
            in_stages=[globals.aboxStages.OM_CSV],
            out_stage=globals.aboxStages.OM_OWL,
        ),
        silent=silent,
    )
    return pipeline
