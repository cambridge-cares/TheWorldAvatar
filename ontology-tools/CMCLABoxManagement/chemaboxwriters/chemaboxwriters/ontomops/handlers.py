from chemaboxwriters.common import StageHandler
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontomops.jsonwriter import omops_json_abox_from_string
from chemaboxwriters.ontomops.csvwriter import omops_csv_abox_from_string
from chemutils.ioutils import writeFile
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.handlers import CSV_TO_OWL
import copy

OMOPS_INP_JSON_TO_OMOPS_JSON = StageHandler(handlerFunc=omops_json_abox_from_string,
                             inStage=aboxStages.OMOPS_INP_JSON,
                             outStage=aboxStages.OMOPS_JSON,
                             fileWriter=jsonStringToFile,
                             fileExt='.omops.json')

OMOPS_JSON_TO_CSV = StageHandler(handlerFunc=omops_csv_abox_from_string,
                            inStage=aboxStages.OMOPS_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.omops.csv')

OMOPS_CSV_TO_OMOPS_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.omops.owl'))