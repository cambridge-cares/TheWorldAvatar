from chemaboxwriters.common.base import get_pipeline, Pipeline
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontospecies.jsonwriter import os_jsonwriter
from chemaboxwriters.ontospecies.csvwriter import os_csvwriter
from typing import Optional
import logging

logger = logging.getLogger(__name__)

def assemble_os_pipeline(
        name: Optional[str] = None,
        outStage: Optional[str] = None
    )->Pipeline:

    if name is None: name = 'ontospecies'
    logger.info(f"Assembling {name} pipeline.")

    pipeline = get_pipeline(
                    name = name,
                    outStage = outStage,
                    fs_upload_subdirs='ontospecies',
                    ts_upload_nmsp='namespace/ontospecies/sparql')

    pipeline.add_handler(handler = handlers.get_qc_log_to_qc_json_handler()) \
            .add_handler(handler = handlers.get_json_to_json_handler(
                                                inStageTag = globals.QUANTUM_CALC_TAG,
                                                outStageTag = globals.ONTO_SPECIES_TAG,
                                                handlerFunc=os_jsonwriter)) \
            .add_handler(handler = handlers.get_json_to_csv_handler(
                                                inStageTag = globals.ONTO_SPECIES_TAG,
                                                outStageTag = globals.ONTO_SPECIES_TAG,
                                                handlerFunc=os_csvwriter)) \
            .add_handler(handler = handlers.get_csv_to_owl_handler(
                                                inStageTag = globals.ONTO_SPECIES_TAG,
                                                outStageTag = globals.ONTO_SPECIES_TAG),
                        upload_outputs_to_ts = True)
    return pipeline