from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
from chemaboxwriters.common.endpoints_config import Endpoints_proxy
import chemaboxwriters.common.globals as globals
import chemaboxwriters.common.handlers as hnds
from chemaboxwriters.ontopesscan.handlers import (
    OC_JSON_TO_OPS_JSON_Handler,
    OPS_JSON_TO_OPS_CSV_Handler,
)
from typing import Optional, Dict

import logging

logger = logging.getLogger(__name__)

OPS_PIPELINE = "opsscan"


def assemble_ops_pipeline(
    endpoints_config: Optional[Dict] = None,
    endpoints_proxy: Optional[Endpoints_proxy] = None,
    disable_endpoints_config_check: bool = False,
) -> Pipeline:

    handlers = [
        OC_JSON_TO_OPS_JSON_Handler(),
        OPS_JSON_TO_OPS_CSV_Handler(),
        hnds.CSV_TO_OWL_Handler(
            name="OPS_CSV_TO_OPS_OWL",
            in_stage=globals.aboxStages.OPS_CSV,
            out_stage=globals.aboxStages.OPS_OWL,
        ),
    ]

    pipeline = get_pipeline(
        name=OPS_PIPELINE,
        handlers=handlers,
        endpoints_config=endpoints_config,
        endpoints_proxy=endpoints_proxy,
    )
    return pipeline
