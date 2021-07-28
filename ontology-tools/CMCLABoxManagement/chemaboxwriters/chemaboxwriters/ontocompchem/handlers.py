from chemaboxwriters.common import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontocompchem.csvstagewriter import compchem_csv_abox_from_string
from chemaboxwriters.ontocompchem.ocjsonstagewriter import compchem_ocjson_abox_from_string
from chemutils.ioutils import writeFile
from chemaboxwriters.common.handlers import CSV_TO_OWL
import copy

QC_JSON_TO_OC_JSON = StageHandler(handlerFunc=compchem_ocjson_abox_from_string,
                             inStage=aboxStages.QC_JSON,
                             outStage=aboxStages.OC_JSON,
                             fileWriter=jsonStringToFile,
                             fileExt='.oc.json')

OC_JSON_TO_CSV = StageHandler(handlerFunc=compchem_csv_abox_from_string,
                            inStage=aboxStages.OC_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.oc.csv')

OC_CSV_TO_OC_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.oc.owl'))