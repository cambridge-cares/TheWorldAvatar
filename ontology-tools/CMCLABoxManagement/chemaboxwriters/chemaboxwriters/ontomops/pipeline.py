from chemaboxwriters.common import Pipeline
from chemaboxwriters.common import aboxStages
from chemaboxwriters.ontomops.handlers import OMINP_JSON_TO_OM_JSON, \
                                              OM_JSON_TO_CSV, \
                                              OM_CSV_TO_OM_OWL

def assemble_omops_pipeline():
    OS_pipeline = Pipeline(supportedStages=[
                            aboxStages.OMINP_JSON,
                            aboxStages.OM_JSON,
                            aboxStages.CSV ]) \
                .add_handler(handler=OMINP_JSON_TO_OM_JSON, handlerName='OMINP_JSON_TO_OM_JSON') \
                .add_handler(handler=OM_JSON_TO_CSV, handlerName='OM_JSON_TO_CSV') \
                .add_handler(handler=OM_CSV_TO_OM_OWL, handlerName='OM_CSV_TO_OM_OWL')
    return OS_pipeline