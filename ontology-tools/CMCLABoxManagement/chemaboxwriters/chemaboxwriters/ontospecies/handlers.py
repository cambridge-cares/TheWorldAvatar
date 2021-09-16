from chemaboxwriters.common import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontospecies.csvwriter import os_csvwriter
from chemaboxwriters.ontospecies.jsonwriter import os_jsonwriter
from chemutils.ioutils import writeFile
from chemaboxwriters.common.handlers import CSV_TO_OWL
import copy

QC_JSON_TO_OS_JSON = StageHandler(handlerFunc=os_jsonwriter,
                             inStage=aboxStages.QC_JSON,
                             outStage=aboxStages.OS_JSON,
                             fileWriter=jsonStringToFile,
                             fileExt='.os.json')

OS_JSON_TO_CSV = StageHandler(handlerFunc=os_csvwriter,
                            inStage=aboxStages.OS_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.os.csv')

OS_CSV_TO_OS_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.os.owl'))