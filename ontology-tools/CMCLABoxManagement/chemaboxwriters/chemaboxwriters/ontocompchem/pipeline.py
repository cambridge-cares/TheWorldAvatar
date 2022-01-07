from chemaboxwriters.common.base import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.stageenums as stge
from chemaboxwriters.ontocompchem.csvwriter import oc_csvwriter
from chemaboxwriters.ontocompchem.jsonwriter import oc_jsonwriter
from typing import Optional
import logging

logger = logging.getLogger(__name__)

def assemble_oc_pipeline(
        name: Optional[str] = None,
        outStage: Optional[str] = None
    )->Pipeline:

    if name is None: name = 'ontocompchem'

    logger.info(f"Assembling the {name} pipeline.")

    pipeline = get_pipeline(
                    name = name,
                    outStage = outStage)

    pipeline.add_handler(handler = handlers.get_qc_log_to_qc_json_handler()) \
            .add_handler(handler = handlers.get_json_to_json_handler(
                                                inStageTag = stge.QUANTUM_CALC_TAG,
                                                outStageTag = stge.ONTO_COMP_CHEM_TAG,
                                                handlerFunc=oc_jsonwriter)) \
            .add_handler(handler = handlers.get_json_to_csv_handler(
                                                inStageTag = stge.ONTO_COMP_CHEM_TAG,
                                                outStageTag = stge.ONTO_COMP_CHEM_TAG,
                                                handlerFunc=oc_csvwriter)) \
            .add_handler(handler = handlers.get_csv_to_owl_handler(
                                                inStageTag = stge.ONTO_COMP_CHEM_TAG,
                                                outStageTag = stge.ONTO_COMP_CHEM_TAG))
    return pipeline