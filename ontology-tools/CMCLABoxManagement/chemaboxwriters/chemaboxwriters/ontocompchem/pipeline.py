from chemaboxwriters.common import Pipeline
from chemaboxwriters.ontocompchem.handlers import QC_LOG_TO_QC_JSON, QC_JSON_TO_OC_JSON


def assemblePipeline(writeAllStages=True):
    pipeline = Pipeline().add_handler(handler=QC_LOG_TO_QC_JSON) \
                         .add_handler(handler=QC_JSON_TO_OC_JSON)
    return pipeline