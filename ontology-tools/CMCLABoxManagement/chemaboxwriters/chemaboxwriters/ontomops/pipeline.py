from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontomops.handlers import (
    OMINP_JSON_TO_OM_JSON_Handler,
    OM_JSON_TO_OM_CSV_Handler,
)
from typing import Optional
from enum import Enum
import logging

logger = logging.getLogger(__name__)

OMOPS_PIPELINE = "ONTOMOPS"


def assemble_omops_pipeline(out_stage: Optional[Enum] = None) -> Pipeline:

    logger.info(f"Assembling {OMOPS_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OMOPS_PIPELINE, out_stage=out_stage)

    pipeline.add_handler(handler=OMINP_JSON_TO_OM_JSON_Handler())
    pipeline.add_handler(handler=OM_JSON_TO_OM_CSV_Handler())
    pipeline.add_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OM_CSV_TO_OM_OWL",
            in_stages=[globals.aboxStages.OM_CSV],
            out_stage=globals.aboxStages.OM_OWL,
        )
    )
    return pipeline
