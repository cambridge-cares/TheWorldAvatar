from chemaboxwriters.common.base import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from compchemparser.app import parseLog
from chemaboxwriters.common.commonfunc import csv2rdf_wrapper
from chemutils.ioutils import writeFile


QC_LOG_TO_QC_JSON = StageHandler(handlerFunc=parseLog,
                       inStage=aboxStages.QC_LOG,
                       outStage=aboxStages.QC_JSON,
                       disableFileToStrConv=True,
                       fileWriter=jsonStringToFile,
                       fileExt='.qc.json')

CSV_TO_OWL = StageHandler(handlerFunc=csv2rdf_wrapper,
                    inStage=aboxStages.CSV,
                    outStage=aboxStages.OWL,
                    fileWriter= writeFile,
                    fileWriterKwargs={'newline':''})