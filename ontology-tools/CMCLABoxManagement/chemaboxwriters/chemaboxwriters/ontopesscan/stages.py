from chemaboxwriters.common.base import Stage
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontopesscan import ops_jsonwriter, \
                                        ops_csvwriter
import chemutils.ioutils.ioutils as ioutils
import chemaboxwriters.common.stages as stages
from typing import Optional

def get_oc_json_to_ops_json_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OC_JSON_TO_OPS_JSON_STAGE'
    if fileExt is None: fileExt = '.ops.json'

    OC_JSON_TO_OPS_JSON_STAGE = Stage(
                                    name= name,
                                    inStage=aboxStages.QC_JSON,
                                    outStage=aboxStages.OPS_JSON,
                                    stageFunc=ops_jsonwriter,
                                    fileWriter=jsonStringToFile,
                                    fileExt=fileExt,
                                    unrollListInput = False
                                )
    return OC_JSON_TO_OPS_JSON_STAGE

def get_ops_json_to_csv_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OPS_JSON_TO_CSV_STAGE'
    if fileExt is None: fileExt = '.ops.csv'

    OPS_JSON_TO_CSV_STAGE = Stage(
                                    name= name,
                                    inStage=aboxStages.OPS_JSON,
                                    outStage=aboxStages.CSV,
                                    stageFunc=ops_csvwriter,
                                    fileWriter=ioutils.writeFile,
                                    fileWriterKwargs={'newline':''},
                                    fileExt=fileExt
                                )
    return OPS_JSON_TO_CSV_STAGE

def get_ops_csv_to_ops_owl_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OPS_CSV_TO_OPS_OWL_STAGE'
    if fileExt is None: fileExt = '.ops.owl'

    OPS_CSV_TO_OPS_OWL_STAGE = stages.get_csv_to_owl_stage(
                                name = name,
                                fileExt= fileExt)
    return OPS_CSV_TO_OPS_OWL_STAGE