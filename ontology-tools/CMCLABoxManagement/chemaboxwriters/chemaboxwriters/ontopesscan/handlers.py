from chemaboxwriters.common import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontopesscan import ops_jsonwriter, \
                                        ops_csvwriter
import chemutils.ioutils.ioutils as ioutils
from chemaboxwriters.common import CSV_TO_OWL
import copy

OC_JSON_TO_OPS_JSON = StageHandler(handlerFunc=ops_jsonwriter,
                             inStage=aboxStages.OC_JSON,
                             outStage=aboxStages.OPS_JSON,
                             unrollListInput=False,
                             fileWriter=jsonStringToFile,
                             fileExt='.ops.json')

OPS_JSON_TO_CSV = StageHandler(handlerFunc=ops_csvwriter,
                            inStage=aboxStages.OPS_JSON,
                            outStage=aboxStages.CSV,
                            fileWriter= ioutils.writeFile,
                            fileWriterKwargs={'newline':''},
                            fileExt='.ops.csv')

OPS_CSV_TO_OPS_OWL = copy.deepcopy(CSV_TO_OWL.set_file_ext('.ops.owl'))