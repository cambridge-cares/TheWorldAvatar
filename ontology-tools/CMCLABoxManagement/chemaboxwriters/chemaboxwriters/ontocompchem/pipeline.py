from chemaboxwriters.common import Pipeline
from chemaboxwriters.ontocompchem.handlers import handlerStrip, handlerReplace


def assemblePipeline(writeAllStages=True):
    pipeline = Pipeline(handler=handlerStrip.set_write_all_stages(writeAllStages)) \
           .add_handler(handler=handlerReplace.set_write_all_stages(writeAllStages))
    return pipeline