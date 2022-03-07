from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as hnds
import chemaboxwriters.common.globals as globals
import chemaboxwriters.common.endpoints_proxy as abconf
from chemaboxwriters.ontocompchem.handlers import (
    OC_JSON_TO_OC_CSV_Handler,
    QC_JSON_TO_OC_JSON_Handler,
)
from typing import Optional
import logging

logger = logging.getLogger(__name__)

OC_PIPELINE = "ocompchem"


def assemble_oc_pipeline(
    endpoints_proxy: Optional[abconf.Endpoints_proxy] = None,
) -> Pipeline:

    handlers = [
        hnds.QC_LOG_TO_QC_JSON_Handler(),
        QC_JSON_TO_OC_JSON_Handler(),
        OC_JSON_TO_OC_CSV_Handler(),
        hnds.CSV_TO_OWL_Handler(
            name="OC_CSV_TO_OC_OWL",
            in_stage=globals.aboxStages.OC_CSV,
            out_stage=globals.aboxStages.OC_OWL,
        ),
    ]

    pipeline = get_pipeline(
        name=OC_PIPELINE,
        handlers=handlers,
        endpoints_proxy=endpoints_proxy,
    )
    return pipeline
