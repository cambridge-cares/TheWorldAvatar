from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontospecies.handlers import QC_JSON_TO_OS_JSON, \
                                                 OS_JSON_TO_CSV, \
                                                 OS_CSV_TO_OS_OWL
from chemaboxwriters.common import QC_LOG_TO_QC_JSON

OS_pipeline = Pipeline(supportedStages=[
                            aboxStages.QC_LOG,
                            aboxStages.QC_JSON,
                            aboxStages.OS_JSON,
                            aboxStages.CSV ]) \
                .add_handler(handler=QC_LOG_TO_QC_JSON, handlerName='QC_LOG_TO_QC_JSON') \
                .add_handler(handler=QC_JSON_TO_OS_JSON, handlerName='QC_JSON_TO_OS_JSON') \
                .add_handler(handler=OS_JSON_TO_CSV, handlerName='OS_JSON_TO_CSV') \
                .add_handler(handler=OS_CSV_TO_OS_OWL, handlerName='OS_CSV_TO_OS_OWL')