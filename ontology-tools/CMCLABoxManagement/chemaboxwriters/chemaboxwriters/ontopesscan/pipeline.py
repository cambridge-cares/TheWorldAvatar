from chemaboxwriters.common.base import Pipeline
import chemaboxwriters.ontopesscan.handlers as handlers
from chemaboxwriters.ontocompchem.pipeline import assemble_oc_pipeline
from chemaboxwriters.common.stageenums import aboxStages

def assemble_ops_pipeline():
    pipeline = Pipeline(collate_inputs_at_stages=[aboxStages.OC_JSON]) \
                .add_handler(handler = assemble_oc_pipeline(
                                            name='OC_PIPELINE',
                                            outStage=aboxStages.OC_JSON)) \
                .add_handler(handler = handlers.get_oc_json_to_ops_json_handler()) \
                .add_handler(handler = handlers.get_ops_json_to_csv_handler()) \
                .add_handler(handler = handlers.get_ops_csv_to_ops_owl_handler())
    return pipeline