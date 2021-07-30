from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontopesscan.handlers import OC_JSON_TO_OPS_JSON, \
                                                 OPS_JSON_TO_CSV, \
                                                 OPS_CSV_TO_OPS_OWL

def assemble_ops_pipeline():
    OPS_pipeline = Pipeline(supportedStages=[
                        aboxStages.OC_JSON,
                        aboxStages.OPS_JSON,
                        aboxStages.CSV ]) \
                .add_handler(handler=OC_JSON_TO_OPS_JSON, handlerName='OC_JSON_TO_OPS_JSON') \
                .add_handler(handler=OPS_JSON_TO_CSV, handlerName = 'OPS_JSON_TO_CSV') \
                .add_handler(handler=OPS_CSV_TO_OPS_OWL, handlerName='OPS_CSV_TO_OPS_OWL')
    return OPS_pipeline