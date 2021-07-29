from chemaboxwriters.common.handlers import QC_LOG_TO_QC_JSON
from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontopesscan.handlers import OC_JSON_TO_OPS_JSON, \
                                                 OPS_CSV_TO_OPS_OWL

OPS_pipeline = Pipeline(supportedStages=[
                            aboxStages.OC_JSON,
                            aboxStages.OPS_JSON,
                            aboxStages.CSV ]) \
                    .add_handler(handler=OC_JSON_TO_OPS_JSON, handlerName='OC_JSON_TO_OPS_JSON') \
                    .add_handler(handler=OPS_CSV_TO_OPS_OWL, handlerName='OPS_CSV_TO_OPS_OWL')