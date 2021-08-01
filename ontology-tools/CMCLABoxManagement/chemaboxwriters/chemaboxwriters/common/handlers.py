from chemaboxwriters.common.base import StageHandler
from compchemparser.app import parseLog
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from entityrdfizer.ABoxTemplateCSVFileToRDF import convert_csv_string_into_rdf
from chemutils.ioutils import writeFile


QC_LOG_TO_QC_JSON = StageHandler(handlerFunc=parseLog,
                       inStage=aboxStages.QC_LOG,
                       outStage=aboxStages.QC_JSON,
                       disableFileToStrConv=True,
                       fileWriter=jsonStringToFile,
                       fileExt='.qc.json')

CSV_TO_OWL = StageHandler(handlerFunc=convert_csv_string_into_rdf,
                    inStage=aboxStages.CSV,
                    outStage=aboxStages.OWL,
                    fileWriter= writeFile,
                    fileWriterKwargs={'newline':''})