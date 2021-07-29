from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontocompchem.handlers import QC_JSON_TO_OC_JSON, \
                                                  OC_JSON_TO_CSV, \
                                                  OC_CSV_TO_OC_OWL
from chemaboxwriters.common import QC_LOG_TO_QC_JSON

OC_pipeline = Pipeline(supportedStages=[
                            aboxStages.QC_LOG,
                            aboxStages.QC_JSON,
                            aboxStages.OC_JSON,
                            aboxStages.CSV ]) \
                .add_handler(handler=QC_LOG_TO_QC_JSON, handlerName='QC_LOG_TO_QC_JSON') \
                .add_handler(handler=QC_JSON_TO_OC_JSON, handlerName='QC_JSON_TO_OC_JSON') \
                .add_handler(handler=OC_JSON_TO_CSV, handlerName='OC_JSON_TO_CSV') \
                .add_handler(handler=OC_CSV_TO_OC_OWL, handlerName='OC_CSV_TO_OC_OWL')