from chemaboxwriters.common.base import Stage
from compchemparser.helpers.utils import jsonStringToFile
from chemaboxwriters.common.stageenums import aboxStages
from chemaboxwriters.ontocompchem.csvwriter import oc_csvwriter
from chemaboxwriters.ontocompchem.jsonwriter import oc_jsonwriter
import chemutils.ioutils.ioutils as ioutils
import chemaboxwriters.common.stages as stages
from typing import Optional

def get_qc_json_to_oc_json_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'QC_JSON_TO_OC_JSON_STAGE'
    if fileExt is None: fileExt = '.oc.json'

    QC_JSON_TO_OC_JSON_STAGE = Stage(
                                    name= name,
                                    inStage=aboxStages.QC_JSON,
                                    outStage=aboxStages.OC_JSON,
                                    stageFunc=oc_jsonwriter,
                                    fileWriter=jsonStringToFile,
                                    fileExt=fileExt
                                )
    return QC_JSON_TO_OC_JSON_STAGE

def get_oc_json_to_oc_csv_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'QC_JSON_TO_OC_JSON_STAGE'
    if fileExt is None: fileExt = '.oc.json'

    OC_JSON_TO_CSV_STAGE = Stage(
                                name= name,
                                inStage=aboxStages.OC_JSON,
                                outStage=aboxStages.OC_CSV,
                                stageFunc=oc_csvwriter,
                                fileWriter=ioutils.writeFile,
                                fileWriterKwargs={'newline':''},
                                fileExt=fileExt
                            )
    return OC_JSON_TO_CSV_STAGE

def get_oc_csv_to_owl_stage(
    name: Optional[str] = None,
    fileExt: Optional[str] = None
    )->Stage:

    if name is None: name = 'OC_CSV_TO_OC_OWL_STAGE'
    if fileExt is None: fileExt = '.oc.owl'

    OC_CSV_TO_OC_OWL_STAGE = stages.get_csv_to_owl_stage(
                                inStage=aboxStages.OC_CSV,
                                outStage=aboxStages.OC_OWL,
                                name = name,
                                fileExt= fileExt)
    return OC_CSV_TO_OC_OWL_STAGE