from chemaboxwriters.common.base import get_handler, StageHandler
from chemaboxwriters.common.handlers import get_json_to_csv_handler, get_csv_to_owl_handler
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontomops.jsonwriter import om_jsonwriter
from chemaboxwriters.ontomops.csvwriter import om_csvwriter
from compchemparser.helpers.utils import jsonStringToFile
from typing import Optional


def get_ominp_json_to_om_json_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OMINP_JSON_TO_OM_JSON'
    if fileExt is None: fileExt = '.om.json'

    OMINP_JSON_TO_OM_JSON = get_handler(
                            name=name,
                            inStages=[aboxStages.OMINP_JSON],
                            outStage = aboxStages.OM_JSON,
                            handlerFunc = om_jsonwriter,
                            fileWriter = jsonStringToFile,
                            fileExt = fileExt
    )
    return OMINP_JSON_TO_OM_JSON


def get_om_json_to_csv_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OM_JSON_TO_OM_CSV'
    if fileExt is None: fileExt = '.om.csv'

    OM_JSON_TO_OM_CSV = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.OM_JSON],
                            outStage= aboxStages.OM_CSV,
                            handlerFunc= om_csvwriter,
                            fileExt= fileExt,
                        )
    return OM_JSON_TO_OM_CSV

def get_om_csv_to_om_owl_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OM_CSV_TO_OM_OWL'
    if fileExt is None: fileExt = '.om.owl'

    OM_CSV_TO_OM_OWL = get_csv_to_owl_handler(
                            name=name,
                            inStages=[aboxStages.OM_CSV],
                            outStage=aboxStages.OM_OWL,
                            fileExt=fileExt
                        )
    return OM_CSV_TO_OM_OWL