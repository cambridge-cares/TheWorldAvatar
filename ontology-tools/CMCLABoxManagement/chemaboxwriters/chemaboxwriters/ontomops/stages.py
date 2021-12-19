from chemaboxwriters.common.base import Stage
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontomops.jsonwriter import om_jsonwriter
from chemaboxwriters.ontomops.csvwriter import om_csvwriter
import chemutils.ioutils.ioutils as ioutils
from compchemparser.helpers.utils import jsonStringToFile
import chemaboxwriters.common.stages as stages
from typing import Optional

def get_ominp_json_to_om_json_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OMINP_JSON_TO_OM_JSON_STAGE'
    if fileExt is None: fileExt = '.om.json'

    OMINP_JSON_TO_OM_JSON_STAGE = Stage(
                                name = name,
                                inStage = aboxStages.OMINP_JSON,
                                outStage = aboxStages.OM_JSON,
                                stageFunc = om_jsonwriter,
                                fileWriter = jsonStringToFile,
                                fileExt = fileExt
                            )
    return OMINP_JSON_TO_OM_JSON_STAGE


def get_om_json_to_csv_stage_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OM_JSON_TO_CSV_STAGE'
    if fileExt is None: fileExt = '.om.csv'

    OM_JSON_TO_CSV_STAGE = Stage(
                        name = name,
                        inStage = aboxStages.OM_JSON,
                        outStage = aboxStages.CSV,
                        stageFunc = om_csvwriter,
                        fileWriter = ioutils.writeFile,
                        fileWriterKwargs = {'newline':''},
                        fileExt = fileExt
                    )
    return OM_JSON_TO_CSV_STAGE


def get_om_csv_to_om_owl_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OM_CSV_TO_OM_OWL_STAGE'
    if fileExt is None: fileExt = '.om.owl'

    OM_CSV_TO_OM_OWL_STAGE = stages.get_csv_to_owl_stage(
                                name = name,
                                fileExt = fileExt
                             )
    return OM_CSV_TO_OM_OWL_STAGE