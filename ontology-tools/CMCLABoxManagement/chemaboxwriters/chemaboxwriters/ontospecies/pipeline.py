from chemaboxwriters.common.pipeline import get_pipeline, Pipeline
from chemaboxwriters.common.endpoints_config import Endpoints_proxy
import chemaboxwriters.common.handlers as hnds
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontospecies.handlers import (
    OS_JSON_TO_OS_CSV_Handler,
    QC_JSON_TO_OS_JSON_Handler,
)
from typing import Optional, Dict
import logging


logger = logging.getLogger(__name__)

OS_PIPELINE = "ospecies"


def assemble_os_pipeline(
    endpoints_config: Optional[Dict] = None,
    endpoints_proxy: Optional[Endpoints_proxy] = None,
    disable_endpoints_config_check: bool = False,
) -> Pipeline:

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
        endpoints_config=endpoints_config,
        endpoints_proxy=endpoints_proxy,
    )
    return pipeline
