from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
from chemaboxwriters.ontopesscan.abox_stages import OPS_ABOX_STAGES
import chemaboxwriters.common.handlers as hnds
from chemaboxwriters.ontopesscan.handlers import (
    OC_JSON_TO_OPS_JSON_Handler,
    OPS_JSON_TO_OPS_CSV_Handler,
)

import logging

logger = logging.getLogger(__name__)

OPS_PIPELINE = "opsscan"


def assemble_ops_pipeline() -> Pipeline:

    handlers = [
        OC_JSON_TO_OPS_JSON_Handler(),
        OPS_JSON_TO_OPS_CSV_Handler(),
        hnds.CSV_TO_OWL_Handler(
            name="OPS_CSV_TO_OPS_OWL",
            in_stage=OPS_ABOX_STAGES.ops_csv,  # type: ignore
            out_stage=OPS_ABOX_STAGES.ops_owl,  # type: ignore
        ),
    ]

    pipeline = get_pipeline(name=OPS_PIPELINE, handlers=handlers)
    return pipeline
