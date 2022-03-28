from chemaboxwriters.common.base import get_pipeline, Pipeline
import chemaboxwriters.common.globals as globals
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
import chemaboxwriters.common.handlers as handlers
from chemaboxwriters.ontopesscan.jsonwriter import ops_jsonwriter
from chemaboxwriters.ontopesscan.csvwriter import ops_csvwriter
from typing import Optional
import logging

logger = logging.getLogger(__name__)

def assemble_ops_pipeline(
        name: Optional[str] = None,
        outStage: Optional[str] = None
    )->Pipeline:

    if name is None: name = 'ontopesscan'
    logger.info(f"Assembling the {name} pipeline.")

    pipeline = get_pipeline(
                    name = name,
                    outStage = outStage,
                    collate_inputs_at_stages = [f"{globals.ONTO_COMP_CHEM_TAG}_JSON"],
                    fs_upload_subdirs='ontopesscan',
                    ts_upload_nmsp='namespace/ontopesscan/sparql')

    pipeline.add_handler(handler = assemble_oc_pipeline(
                                        name='OC_PIPELINE',
                                        outStage=f"{globals.ONTO_COMP_CHEM_TAG}_JSON"
                                    )) \
            .add_handler(handler = handlers.get_json_to_json_handler(
                                                inStageTag = globals.ONTO_COMP_CHEM_TAG,
                                                outStageTag = globals.ONTO_PESSCAN_TAG,
                                                handlerFunc=ops_jsonwriter,
                                                unroll_input = False)) \
            .add_handler(handler = handlers.get_json_to_csv_handler(
                                                inStageTag = globals.ONTO_PESSCAN_TAG,
                                                outStageTag = globals.ONTO_PESSCAN_TAG,
                                                handlerFunc=ops_csvwriter)) \
            .add_handler(handler = handlers.get_csv_to_owl_handler(
                                                inStageTag = globals.ONTO_PESSCAN_TAG,
                                                outStageTag = globals.ONTO_PESSCAN_TAG),
                        upload_outputs_to_ts = True)
    return pipeline