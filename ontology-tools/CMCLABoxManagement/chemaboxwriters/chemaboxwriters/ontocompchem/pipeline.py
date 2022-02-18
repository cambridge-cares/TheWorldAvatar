from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontocompchem.handlers import (
    OC_JSON_TO_OC_CSV_Handler,
    QC_JSON_TO_OC_JSON_Handler,
)
from typing import Optional
import logging
from enum import Enum

logger = logging.getLogger(__name__)

OC_PIPELINE = "ONTOCOMPCHEM"


def assemble_oc_pipeline(out_stage: Optional[Enum] = None) -> Pipeline:

    logger.info(f"Assembling the {OC_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OC_PIPELINE, out_stage=out_stage)

    pipeline.add_handler(handler=handlers.QC_LOG_TO_QC_JSON_Handler())
    pipeline.add_handler(handler=QC_JSON_TO_OC_JSON_Handler())
    pipeline.add_handler(handler=OC_JSON_TO_OC_CSV_Handler()).add_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OC_CSV_TO_OC_OWL",
            in_stages=[globals.aboxStages.OC_CSV],
            out_stage=globals.aboxStages.OC_OWL,
        )
    )
    return pipeline
