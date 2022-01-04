from chemaboxwriters.common.base import StageHandler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontocompchem.csvwriter import oc_csvwriter
from chemaboxwriters.ontocompchem.jsonwriter import oc_jsonwriter
from chemaboxwriters.common.handlers import get_json_to_csv_handler, get_csv_to_owl_handler
from typing import Optional

def get_qc_json_to_oc_json_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'QC_JSON_TO_OC_JSON'
    if fileExt is None: fileExt = '.oc.json'

    QC_JSON_TO_OC_JSON = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.QC_JSON],
                            outStage= aboxStages.OC_JSON,
                            handlerFunc= oc_jsonwriter,
                            fileWriter= jsonStringToFile,
                            fileExt= fileExt
                        )
    return QC_JSON_TO_OC_JSON

def get_oc_json_to_oc_csv_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OC_JSON_TO_OC_CSV'
    if fileExt is None: fileExt = '.oc.csv'

    OC_JSON_TO_OC_CSV = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.OC_JSON],
                            outStage= aboxStages.OC_CSV,
                            handlerFunc= oc_csvwriter,
                            fileExt= fileExt
                        )
    return OC_JSON_TO_OC_CSV


def get_oc_csv_to_oc_owl_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OC_CSV_TO_OC_OWL'
    if fileExt is None: fileExt = '.oc.owl'

    OC_CSV_TO_OC_OWL = get_csv_to_owl_handler(
        name=name,
        inStages=[aboxStages.OC_CSV],
        outStage=aboxStages.OC_OWL,
        fileExt=fileExt
    )
    return OC_CSV_TO_OC_OWL