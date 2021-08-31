from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontomops.handlers import OMOPS_INP_JSON_TO_OMOPS_JSON, \
                                              OMOPS_JSON_TO_CSV, \
                                              OMOPS_CSV_TO_OMOPS_OWL

def assemble_omops_pipeline():
    OS_pipeline = Pipeline(supportedStages=[
                            aboxStages.OMOPS_INP_JSON,
                            aboxStages.OMOPS_JSON,
                            aboxStages.CSV ]) \
                .add_handler(handler=OMOPS_INP_JSON_TO_OMOPS_JSON, handlerName='OMOPS_INP_JSON_TO_OMOPS_JSON') \
                .add_handler(handler=OMOPS_JSON_TO_CSV, handlerName='OMOPS_JSON_TO_CSV') \
                .add_handler(handler=OMOPS_CSV_TO_OMOPS_OWL, handlerName='OMOPS_CSV_TO_OMOPS_OWL')
    return OS_pipeline