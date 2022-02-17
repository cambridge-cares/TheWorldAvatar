from chemaboxwriters.common.base import get_pipeline, Pipeline, FILE_SERVER, TRIPLE_STORE
import chemaboxwriters.common.handlers as handlers
import chemaboxwriters.common.globals as globals
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
                    outStage = outStage,
                    fs_upload_subdirs='ontocompchem',
                    ts_upload_nmsp='namespace/ontocompchem/sparql')

    pipeline.add_handler(handler = handlers.get_qc_log_to_qc_json_handler(),
                         upload_inputs_to_fs = True) \
            .add_handler(handler = handlers.get_json_to_json_handler(
                                                inStageTag = globals.QUANTUM_CALC_TAG,
                                                outStageTag = globals.ONTO_COMP_CHEM_TAG,
                                                handlerFunc=oc_jsonwriter),
                        pass_uploaders_ref_as_arg = True) \
            .add_handler(handler = handlers.get_json_to_csv_handler(
                                                inStageTag = globals.ONTO_COMP_CHEM_TAG,
                                                outStageTag = globals.ONTO_COMP_CHEM_TAG,
                                                handlerFunc=oc_csvwriter)) \
            .add_handler(handler = handlers.get_csv_to_owl_handler(
                                                inStageTag = globals.ONTO_COMP_CHEM_TAG,
                                                outStageTag = globals.ONTO_COMP_CHEM_TAG),
                        upload_outputs_to_ts = True)
    return pipeline