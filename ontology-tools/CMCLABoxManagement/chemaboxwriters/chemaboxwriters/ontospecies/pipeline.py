from chemaboxwriters.common.base import Pipeline
import chemaboxwriters.ontospecies.handlers as handlers
from chemaboxwriters.common.handlers import get_qc_log_to_qc_json_handler

def assemble_os_pipeline()->Pipeline:
    pipeline = Pipeline() \
                .add_handler(handler = get_qc_log_to_qc_json_handler()) \
                .add_handler(handler = handlers.get_qc_json_to_os_json_handler()) \
                .add_handler(handler = handlers.get_os_json_to_csv_handler()) \
                .add_handler(handler = handlers.get_os_csv_to_os_owl_handler())
    return pipeline