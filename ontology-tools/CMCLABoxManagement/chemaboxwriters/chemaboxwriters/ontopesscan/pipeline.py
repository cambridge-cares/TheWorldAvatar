from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
from chemaboxwriters.ontopesscan.handlers import (
    OC_JSON_TO_OPS_JSON_Handler,
    OPS_JSON_TO_OPS_CSV_Handler,
)
from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers


import logging

logger = logging.getLogger(__name__)

OPS_PIPELINE = "ontopesscan"


def assemble_ops_pipeline(silent: bool = False) -> Pipeline:

    if not silent:
        logger.info(f"Assembling {OPS_PIPELINE} pipeline.")

    pipeline = get_pipeline(name=OPS_PIPELINE)
    oc_pipeline = assemble_oc_pipeline(silent=True)

    # add all oc pipeline handlers
    for handler in oc_pipeline._handlers:
        pipeline.register_handler(handler=handler, silent=silent)
    pipeline.register_handler(handler=OC_JSON_TO_OPS_JSON_Handler(), silent=silent)
    pipeline.register_handler(handler=OPS_JSON_TO_OPS_CSV_Handler(), silent=silent)
    pipeline.register_handler(
        handler=handlers.CSV_TO_OWL_Handler(
            name="OPS_CSV_TO_OPS_OWL",
            in_stages=[globals.aboxStages.OPS_CSV],
            out_stage=globals.aboxStages.OPS_OWL,
        ),
        silent=silent,
    )
    return pipeline
