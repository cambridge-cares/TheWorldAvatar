from chemaboxwriters.common.base import get_handler, StageHandler
from chemaboxwriters.common.handlers import get_json_to_csv_handler, get_csv_to_owl_handler
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontopesscan import ops_jsonwriter, \
                                        ops_csvwriter
from typing import Optional

def get_oc_json_to_ops_json_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OC_JSON_TO_OPS_JSON'
    if fileExt is None: fileExt = '.ops.json'

    OC_JSON_TO_OPS_JSON = get_handler(
                            name=name,
                            inStages=[aboxStages.OC_JSON],
                            outStage = aboxStages.OPS_JSON,
                            handlerFunc = ops_jsonwriter,
                            fileWriter = jsonStringToFile,
                            fileExt = fileExt,
                            unroll_input=False,
                        )
    return OC_JSON_TO_OPS_JSON


def get_ops_json_to_csv_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OPS_JSON_TO_OPS_CSV'
    if fileExt is None: fileExt = '.ops.csv'

    OPS_JSON_TO_OPS_CSV = get_json_to_csv_handler(
                            name=name,
                            inStages= [aboxStages.OPS_JSON],
                            outStage= aboxStages.OPS_CSV,
                            handlerFunc= ops_csvwriter,
                            fileExt= fileExt,
                        )
    return OPS_JSON_TO_OPS_CSV


def get_ops_csv_to_ops_owl_handler(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->StageHandler:

    if name is None: name = 'OPS_CSV_TO_OPS_OWL'
    if fileExt is None: fileExt = '.ops.owl'

    OPS_CSV_TO_OPS_OWL = get_csv_to_owl_handler(
                            name=name,
                            inStages=[aboxStages.OPS_CSV],
                            outStage=aboxStages.OPS_OWL,
                            fileExt=fileExt
                        )
    return OPS_CSV_TO_OPS_OWL