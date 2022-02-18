from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontospecies.handlers import (
    OS_JSON_TO_OS_CSV_Handler,
    QC_JSON_TO_OS_JSON_Handler,
)
from typing import Optional
from enum import Enum
import logging


logger = logging.getLogger(__name__)

OS_PIPELINE = "ONTOSPECIES"


def assemble_os_pipeline(out_stage: Optional[Enum] = None) -> Pipeline:

    logger.info(f"Assembling {OS_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OS_PIPELINE, out_stage=out_stage)

    pipeline.add_handler(handler=handlers.QC_LOG_TO_QC_JSON_Handler())
    pipeline.add_handler(QC_JSON_TO_OS_JSON_Handler())
    pipeline.add_handler(OS_JSON_TO_OS_CSV_Handler())
    pipeline.add_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OS_CSV_TO_OS_OWL",
            in_stages=[globals.aboxStages.OS_CSV],
            out_stage=globals.aboxStages.OS_OWL,
        )
    )
    return pipeline
