from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as hnds
from chemaboxwriters.ontospecies.abox_stages import OS_ABOX_STAGES
from chemaboxwriters.ontospecies.handlers import QC_JSON_TO_OS_JSON_Handler
from chemaboxwriters.ontospecies import OS_SCHEMA
import logging

logger = logging.getLogger(__name__)

OS_PIPELINE = "ospecies"


def assemble_os_pipeline() -> Pipeline:

    handlers = [
        hnds.QC_LOG_TO_QC_JSON_Handler(),
        QC_JSON_TO_OS_JSON_Handler(),
        hnds.JSON_TO_CSV_Handler(
            name="OS_JSON_TO_OS_CSV",
            in_stage=OS_ABOX_STAGES.os_json,  # type: ignore
            out_stage=OS_ABOX_STAGES.os_csv,  # type: ignore
            schema_file=OS_SCHEMA,
        ),
        hnds.CSV_TO_OWL_Handler(
            name="OS_CSV_TO_OS_OWL",
            in_stage=OS_ABOX_STAGES.os_csv,  # type: ignore
            out_stage=OS_ABOX_STAGES.os_owl,  # type: ignore
        ),
    ]

    pipeline = get_pipeline(
        name=OS_PIPELINE,
        handlers=handlers,
    )
    return pipeline
