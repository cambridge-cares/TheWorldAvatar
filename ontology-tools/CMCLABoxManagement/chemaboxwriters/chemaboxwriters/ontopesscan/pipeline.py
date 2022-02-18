from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
from chemaboxwriters.ontopesscan.handlers import (
    OC_JSON_TO_OPS_JSON_Handler,
    OPS_JSON_TO_OPS_CSV_Handler,
)
from typing import Optional
from enum import Enum
from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers


import logging

logger = logging.getLogger(__name__)

OPS_PIPELINE = "ONTOPESSCAN"


def assemble_ops_pipeline(out_stage: Optional[Enum] = None) -> Pipeline:

    logger.info(f"Assembling {OPS_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OPS_PIPELINE)

    pipeline.add_handler(
        handler=assemble_oc_pipeline(out_stage=globals.aboxStages.OC_JSON)
    )
    pipeline.add_handler(handler=OC_JSON_TO_OPS_JSON_Handler())
    pipeline.add_handler(handler=OPS_JSON_TO_OPS_CSV_Handler())
    pipeline.add_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OPS_CSV_TO_OPS_OWL",
            in_stages=[globals.aboxStages.OPS_CSV],
            out_stage=globals.aboxStages.OPS_OWL,
        )
    )
    return pipeline
