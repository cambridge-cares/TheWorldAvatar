from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as hnds
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES
from chemaboxwriters.ontocompchem.handlers import QC_JSON_TO_OC_JSON_Handler
from chemaboxwriters.ontocompchem import OC_SCHEMA
import logging

logger = logging.getLogger(__name__)

OC_PIPELINE = "ocompchem"


def assemble_oc_pipeline() -> Pipeline:

    handlers = [
        hnds.QC_LOG_TO_QC_JSON_Handler(),
        QC_JSON_TO_OC_JSON_Handler(),
        hnds.JSON_TO_CSV_Handler(
            name="OC_JSON_TO_OC_CSV",
            in_stage=OC_ABOX_STAGES.oc_json,  # type: ignore
            out_stage=OC_ABOX_STAGES.oc_csv,  # type: ignore
            schema_file=OC_SCHEMA,
        ),
        hnds.CSV_TO_OWL_Handler(
            name="OC_CSV_TO_OC_OWL",
            in_stage=OC_ABOX_STAGES.oc_csv,  # type: ignore
            out_stage=OC_ABOX_STAGES.oc_owl,  # type: ignore
        ),
    ]

    pipeline = get_pipeline(
        name=OC_PIPELINE,
        handlers=handlers,
    )
    return pipeline
