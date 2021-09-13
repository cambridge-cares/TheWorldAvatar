from chemaboxwriters.common import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontocompchem.csvwriter import oc_csvwriter
from chemaboxwriters.ontocompchem.jsonwriter import oc_jsonwriter
from chemutils.ioutils import writeFile
from chemaboxwriters.common.handlers import CSV_TO_OWL
import copy

QC_JSON_TO_OC_JSON = StageHandler(handlerFunc=oc_jsonwriter,
                             inStage=aboxStages.QC_JSON,
                             outStage=aboxStages.OC_JSON,
                             fileWriter=jsonStringToFile,
                             fileExt='.oc.json')

OC_JSON_TO_CSV = StageHandler(handlerFunc=oc_csvwriter,
                            inStage=aboxStages.OC_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.oc.csv')

OC_CSV_TO_OC_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.oc.owl'))