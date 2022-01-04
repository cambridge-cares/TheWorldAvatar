from chemaboxwriters.common.base import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontospecies.csvwriter import os_csvwriter
from chemaboxwriters.ontospecies.jsonwriter import os_jsonwriter
from chemaboxwriters.common.handlers import get_json_to_csv_handler, get_csv_to_owl_handler
from typing import Optional

def get_qc_json_to_os_json_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'QC_JSON_TO_OS_JSON'
    if fileExt is None: fileExt = '.os.json'

    QC_JSON_TO_OS_JSON = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.QC_JSON],
                            outStage= aboxStages.OS_JSON,
                            handlerFunc= os_jsonwriter,
                            fileWriter= jsonStringToFile,
                            fileExt= fileExt
                        )
    return QC_JSON_TO_OS_JSON


def get_os_json_to_csv_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OS_JSON_TO_OS_CSV'
    if fileExt is None: fileExt = '.os.csv'

    OS_JSON_TO_OS_CSV = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.OS_JSON],
                            outStage= aboxStages.OS_CSV,
                            handlerFunc= os_csvwriter,
                            fileExt= fileExt
                        )
    return OS_JSON_TO_OS_CSV


def get_os_csv_to_os_owl_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OS_CSV_TO_OS_OWL'
    if fileExt is None: fileExt = '.os.owl'

    OS_CSV_TO_OS_OWL = get_csv_to_owl_handler(
                            name=name,
                            inStages=[aboxStages.OS_CSV],
                            outStage=aboxStages.OS_OWL,
                            fileExt=fileExt
                        )
    return OS_CSV_TO_OS_OWL