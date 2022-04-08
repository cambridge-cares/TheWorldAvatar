from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
from chemaboxwriters.ontopesscan.abox_stages import OPS_ABOX_STAGES
import chemaboxwriters.common.handlers as hnds
from chemaboxwriters.ontopesscan.handlers import OC_JSON_TO_OPS_JSON_Handler
from chemaboxwriters.ontopesscan import OPS_SCHEMA
import logging

logger = logging.getLogger(__name__)

OPS_PIPELINE = "opsscan"


def assemble_ops_pipeline() -> Pipeline:

    handlers = [
        OC_JSON_TO_OPS_JSON_Handler(),
        hnds.JSON_TO_CSV_Handler(
            name="OPS_JSON_TO_OPS_CSV",
            in_stage=OPS_ABOX_STAGES.ops_json,  # type: ignore
            out_stage=OPS_ABOX_STAGES.ops_csv,  # type: ignore
            schema_file=OPS_SCHEMA,
        ),
        hnds.CSV_TO_OWL_Handler(
            name="OPS_CSV_TO_OPS_OWL",
            in_stage=OPS_ABOX_STAGES.ops_csv,  # type: ignore
            out_stage=OPS_ABOX_STAGES.ops_owl,  # type: ignore
        ),
    ]

    pipeline = get_pipeline(name=OPS_PIPELINE, handlers=handlers)
    return pipeline
