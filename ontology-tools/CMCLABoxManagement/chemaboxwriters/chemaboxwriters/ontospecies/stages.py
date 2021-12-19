from chemaboxwriters.common.base import Stage
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontospecies.csvwriter import os_csvwriter
from chemaboxwriters.ontospecies.jsonwriter import os_jsonwriter
import chemutils.ioutils.ioutils as ioutils
import chemaboxwriters.common.stages as stages
from typing import Optional


def get_qc_json_to_os_json_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'QC_JSON_TO_OS_JSON_STAGE'
    if fileExt is None: fileExt = '.os.json'

    QC_JSON_TO_OS_JSON_STAGE = Stage(
                                    name = name,
                                    inStage = aboxStages.QC_JSON,
                                    outStage = aboxStages.OS_JSON,
                                    stageFunc = os_jsonwriter,
                                    fileWriter = jsonStringToFile,
                                    fileExt = fileExt
                                )
    return QC_JSON_TO_OS_JSON_STAGE

def get_os_json_to_csv_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OS_JSON_TO_CSV_STAGE'
    if fileExt is None: fileExt = '.os.csv'

    OS_JSON_TO_CSV_STAGE = Stage(
                                name = name,
                                inStage = aboxStages.OS_JSON,
                                outStage = aboxStages.CSV,
                                stageFunc = os_csvwriter,
                                fileWriter = ioutils.writeFile,
                                fileWriterKwargs={'newline':''},
                                fileExt = fileExt
                            )
    return OS_JSON_TO_CSV_STAGE

def get_os_csv_to_os_owl_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OS_CSV_TO_OS_OWL_STAGE'
    if fileExt is None: fileExt = '.os.owl'

    OS_CSV_TO_OS_OWL_STAGE = stages.get_csv_to_owl_stage(
                                name = name,
                                fileExt = fileExt
                             )
    return OS_CSV_TO_OS_OWL_STAGE