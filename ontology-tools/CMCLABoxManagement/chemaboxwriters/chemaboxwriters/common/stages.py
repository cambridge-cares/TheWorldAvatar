from chemaboxwriters.common.base import Stage
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from compchemparser.app import parseLog
from chemaboxwriters.common.commonfunc import csv2rdf_wrapper
import chemutils.ioutils.ioutils as ioutils
from enum import Enum
from typing import Optional

def get_qc_log_to_qc_json_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None,
    inpFileExt: Optional[str] = None,
    )->Stage:

    if name is None: name = 'QC_LOG_TO_QC_JSON_STAGE'
    if fileExt is None: fileExt = '.qc.json'

    QC_LOG_TO_QC_JSON_STAGE = Stage(
                        name=name,
                        inStage=aboxStages.QC_LOG,
                        outStage=aboxStages.QC_JSON,
                        stageFunc=parseLog,
                        fileWriter=jsonStringToFile,
                        fileExt=fileExt,
                        inpFileExt=inpFileExt.split(',') \
                                   if inpFileExt is not None \
                                   else None
                    )
    return QC_LOG_TO_QC_JSON_STAGE


def get_csv_to_owl_stage(
    inStage: Enum,
    outStage: Enum,
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'CSV_TO_OWL_STAGE'
    if fileExt is None: fileExt = '.owl'

    CSV_TO_OWL_STAGE = Stage(
                            name=name,
                            inStage=inStage,
                            outStage=outStage,
                            stageFunc=csv2rdf_wrapper,
                            fileWriter=ioutils.writeFile,
                            fileExt=fileExt,
                            fileWriterKwargs={'newline':''}
                        )
    return CSV_TO_OWL_STAGE