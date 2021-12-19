from chemaboxwriters.common.base import Pipeline
import chemaboxwriters.ontopesscan.handlers as handlers

def assemble_ops_pipeline():
    pipeline = Pipeline() \
                .add_handler(handler = handlers.get_qc_log_to_oc_json_handler()) \
                .add_handler(handler = handlers.get_oc_json_to_ops_json_handler()) \
                .add_handler(handler = handlers.get_ops_json_to_csv_handler()) \
                .add_handler(handler = handlers.get_ops_csv_to_ops_owl_handler())
    return pipeline