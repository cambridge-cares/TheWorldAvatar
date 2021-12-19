from chemaboxwriters.common.base import Pipeline
import chemaboxwriters.ontocompchem.handlers as handlers
from chemaboxwriters.common.handlers import get_qc_log_to_qc_json_handler

def assemble_oc_pipeline(
    qc_log_ext:str
    )->Pipeline:

    pipeline = Pipeline() \
                .add_handler(handler = get_qc_log_to_qc_json_handler(
                    stage_inpFileExt= qc_log_ext)) \
                .add_handler(handler = handlers.get_qc_json_to_oc_json_handler()) \
                .add_handler(handler = handlers.get_oc_json_to_oc_csv_handler()) \
                .add_handler(handler = handlers.get_oc_csv_to_oc_owl_handler())
    return pipeline