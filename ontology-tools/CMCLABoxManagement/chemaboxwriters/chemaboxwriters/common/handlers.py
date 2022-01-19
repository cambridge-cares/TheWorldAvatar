from chemaboxwriters.common.base import get_handler, StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.globals import QUANTUM_CALC_TAG
from compchemparser.app import parseLog
from chemaboxwriters.common.utilsfunc import csv2rdf_wrapper
import chemutils.ioutils.ioutils as ioutils
from typing import Callable

def get_qc_log_to_qc_json_handler(
    *handler_args,
    **handler_kwargs
    )->StageHandler:

    inStage = f"{QUANTUM_CALC_TAG}_LOG"
    outStage = f"{QUANTUM_CALC_TAG}_JSON"

    handler = get_handler(
                    inStages=[inStage],
                    outStage=outStage,
                    handlerFunc=parseLog,
                    fileWriter=jsonStringToFile,
                    *handler_args,
                    **handler_kwargs
            )
    return handler

def get_json_to_json_handler(
    inStageTag: str,
    outStageTag: str,
    handlerFunc: Callable,
    *handler_args,
    **handler_kwargs
    )->StageHandler:

    inStage = f"{inStageTag}_JSON"
    outStage = f"{outStageTag}_JSON"

    handler = get_handler(
                    inStages=[inStage],
                    outStage=outStage,
                    handlerFunc=handlerFunc,
                    fileWriter=jsonStringToFile,
                    *handler_args,
                    **handler_kwargs
            )
    return handler

def get_json_to_csv_handler(
    inStageTag: str,
    outStageTag: str,
    handlerFunc: Callable,
    *handler_args,
    **handler_kwargs
    )->StageHandler:

    inStage = f"{inStageTag}_JSON"
    outStage = f"{outStageTag}_CSV"

    handler = get_handler(
                    inStages=[inStage],
                    outStage=outStage,
                    handlerFunc=handlerFunc,
                    fileWriter=ioutils.writeFile,
                    fileWriterKwargs={'newline':''},
                    *handler_args,
                    **handler_kwargs
                )
    return handler

def get_csv_to_owl_handler(
    inStageTag: str,
    outStageTag: str,
    *handler_args,
    **handler_kwargs
    )->StageHandler:

    inStage = f"{inStageTag}_CSV"
    outStage = f"{outStageTag}_OWL"

    handler = get_handler(
                    inStages=[inStage],
                    outStage=outStage,
                    handlerFunc=csv2rdf_wrapper,
                    fileWriter=ioutils.writeFile,
                    fileWriterKwargs={'newline':''},
                    *handler_args,
                    **handler_kwargs
                )
    return handler

