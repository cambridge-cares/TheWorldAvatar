from chemaboxwriters.common import Handler
from chemaboxwriters.ontocompchem.stageenums import aboxStages
from compchemparser.app import parseLog
from compchemparser.helpers.utils import qc_log_to_json as json_to_file

QC_LOG_TO_QC_JSON = Handler(handlerFunc=parseLog,
                       inStage=aboxStages.QC_LOG,
                       outStage=aboxStages.QC_JSON,
                       fileWriter=json_to_file,
                       fileExt='.qc_json')

QC_JSON_TO_OC_JSON = Handler(handlerFunc=lambda x: x,
                             inStage=aboxStages.QC_JSON,
                             outStage=aboxStages.OC_JSON,
                             fileWriter=json_to_file,
                             fileExt='.oc_json')

OC_JSON_TO_OC_CSV = Handler(handlerFunc=lambda x: x,
                             inStage=aboxStages.OC_JSON,
                             outStage=aboxStages.OC_CSV,
                             fileWriter=json_to_file,
                             fileExt='.oc_csv')