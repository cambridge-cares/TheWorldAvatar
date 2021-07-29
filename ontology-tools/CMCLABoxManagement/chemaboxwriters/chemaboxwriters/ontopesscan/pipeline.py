from chemaboxwriters.common import Pipeline
from chemaboxwriters.ontopesscan.handlers import OC_JSON_TO_OPS_JSON, \
                                                 OPS_CSV_TO_OPS_OWL

OPS_pipeline = Pipeline() \
                        .add_handler(handler=OC_JSON_TO_OPS_JSON, handlerName='OC_JSON_TO_OPS_JSON') \
                        .add_handler(handler=OPS_CSV_TO_OPS_OWL, handlerName='OPS_CSV_TO_OPS_OWL')