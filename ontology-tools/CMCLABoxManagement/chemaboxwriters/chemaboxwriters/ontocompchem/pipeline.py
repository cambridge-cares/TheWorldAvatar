from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontocompchem.handlers import (
    OC_JSON_TO_OC_CSV_Handler,
    QC_JSON_TO_OC_JSON_Handler,
)
from typing import Optional
import logging

logger = logging.getLogger(__name__)

OC_PIPELINE = "ontocompchem"


def assemble_oc_pipeline(
    config_file: Optional[str] = None, silent: bool = False
) -> Pipeline:

    if not silent:
        logger.info(f"Assembling the {OC_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OC_PIPELINE, config_file=config_file)

    pipeline.register_handler(
        handler=handlers.QC_LOG_TO_QC_JSON_Handler(), silent=silent
    )
    pipeline.register_handler(handler=QC_JSON_TO_OC_JSON_Handler(), silent=silent)
    pipeline.register_handler(handler=OC_JSON_TO_OC_CSV_Handler())
    pipeline.register_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OC_CSV_TO_OC_OWL",
            in_stages=[globals.aboxStages.OC_CSV],
            out_stage=globals.aboxStages.OC_OWL,
        ),
        silent=silent,
    )
    return pipeline
