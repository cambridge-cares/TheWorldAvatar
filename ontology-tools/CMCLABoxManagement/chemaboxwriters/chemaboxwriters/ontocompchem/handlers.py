from chemaboxwriters.common import Handler
from chemaboxwriters.ontocompchem.stageenums import aboxStages

handlerStrip = Handler(handlerFunc=lambda x: x.strip(),
                                inStage=aboxStages.QC_LOG,
                                outStage=aboxStages.QC_JSON)

handlerReplace = Handler(handlerFunc=lambda x: x.replace('a','b'),
                                inStage=aboxStages.QC_JSON,
                                outStage=aboxStages.OC_JSON)