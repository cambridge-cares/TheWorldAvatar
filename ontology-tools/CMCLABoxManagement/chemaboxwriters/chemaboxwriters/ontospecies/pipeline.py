from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontospecies.handlers import (
    OS_JSON_TO_OS_CSV_Handler,
    QC_JSON_TO_OS_JSON_Handler,
)
from typing import Optional
import logging


logger = logging.getLogger(__name__)

OS_PIPELINE = "ontospecies"


def assemble_os_pipeline(
    config_file: Optional[str] = None, silent: bool = False
) -> Pipeline:

    if not silent:
        logger.info(f"Assembling {OS_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OS_PIPELINE, config_file=config_file)

    pipeline.register_handler(
        handler=handlers.QC_LOG_TO_QC_JSON_Handler(), silent=silent
    )
    pipeline.register_handler(QC_JSON_TO_OS_JSON_Handler(), silent=silent)
    pipeline.register_handler(OS_JSON_TO_OS_CSV_Handler(), silent=silent)
    pipeline.register_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OS_CSV_TO_OS_OWL",
            in_stages=[globals.aboxStages.OS_CSV],
            out_stage=globals.aboxStages.OS_OWL,
        ),
        silent=silent,
    )
    return pipeline
