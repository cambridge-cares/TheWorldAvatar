from chemaboxwriters.common import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontopesscan import compchem_opsjson_abox_from_string
from chemaboxwriters.common import CSV_TO_OWL
import copy

OC_JSON_TO_OPS_JSON = StageHandler(handlerFunc=compchem_opsjson_abox_from_string,
                             inStage=aboxStages.OC_JSON,
                             outStage=aboxStages.OPS_JSON,
                             unrollListInput=False,
                             fileWriter=jsonStringToFile,
                             fileExt='.ops.json')

OPS_CSV_TO_OPS_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.ops.owl'))