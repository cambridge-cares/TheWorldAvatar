from chemaboxwriters.common.base import Pipeline
from chemaboxwriters.common.stageenums import aboxStages
import chemaboxwriters.ontomops.handlers as handlers

def assemble_omops_pipeline():
    pipeline = Pipeline() \
                .add_handler(handler = handlers.get_ominp_json_to_om_json_handler()) \
                .add_handler(handler = handlers.get_om_json_to_csv_handler()) \
                .add_handler(handler = handlers.get_om_csv_to_om_owl_handler())
    return pipeline