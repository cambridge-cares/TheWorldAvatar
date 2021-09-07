from chemaboxwriters.common import StageHandler
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontomops.jsonwriter import om_jsonwriter
from chemaboxwriters.ontomops.csvwriter import om_csvwriter
from chemutils.ioutils import writeFile
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.handlers import CSV_TO_OWL
import copy

OMINP_JSON_TO_OM_JSON = StageHandler(handlerFunc=om_jsonwriter,
                             inStage=aboxStages.OMINP_JSON,
                             outStage=aboxStages.OM_JSON,
                             fileWriter=jsonStringToFile,
                             fileExt='.om.json')

OM_JSON_TO_CSV = StageHandler(handlerFunc=om_csvwriter,
                            inStage=aboxStages.OM_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.om.csv')

OM_CSV_TO_OM_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.om.owl'))