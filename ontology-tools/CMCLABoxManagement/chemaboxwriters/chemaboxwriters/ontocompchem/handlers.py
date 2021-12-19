from chemaboxwriters.common.base import StageHandler
import chemaboxwriters.ontocompchem.stages as stages
from typing import Optional

def get_qc_json_to_oc_json_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'QC_JSON_TO_OC_JSON'

    QC_JSON_TO_OC_JSON = StageHandler(name=name) \
                        .add_stage(
                            stage = stages.get_qc_json_to_oc_json_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return QC_JSON_TO_OC_JSON

def get_oc_json_to_oc_csv_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OC_JSON_TO_CSV'

    OC_JSON_TO_CSV = StageHandler(name=name) \
                        .add_stage(
                            stage = stages.get_oc_json_to_oc_csv_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return OC_JSON_TO_CSV

def get_oc_csv_to_oc_owl_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OC_CSV_TO_OC_OWL'

    OC_CSV_TO_OC_OWL = StageHandler(name=name) \
                        .add_stage(
                            stage = stages.get_oc_csv_to_owl_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return OC_CSV_TO_OC_OWL