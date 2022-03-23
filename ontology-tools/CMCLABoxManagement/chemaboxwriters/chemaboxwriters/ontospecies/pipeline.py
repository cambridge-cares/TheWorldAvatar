from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as hnds
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontospecies.handlers import (
    OS_JSON_TO_OS_CSV_Handler,
    QC_JSON_TO_OS_JSON_Handler,
)
import logging


logger = logging.getLogger(__name__)

OS_PIPELINE = "ospecies"


def assemble_os_pipeline() -> Pipeline:

    handlers = [
        hnds.QC_LOG_TO_QC_JSON_Handler(),
        QC_JSON_TO_OS_JSON_Handler(),
        OS_JSON_TO_OS_CSV_Handler(),
        hnds.CSV_TO_OWL_Handler(
            name="OS_CSV_TO_OS_OWL",
            in_stage=globals.aboxStages.OS_CSV,
            out_stage=globals.aboxStages.OS_OWL,
        ),
    ]

    pipeline = get_pipeline(
        name=OS_PIPELINE,
        handlers=handlers,
    )
    return pipeline
