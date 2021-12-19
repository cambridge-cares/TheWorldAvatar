from chemaboxwriters.common.base import StageHandler
import chemaboxwriters.ontomops.stages as stages
from typing import Optional

def get_ominp_json_to_om_json_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OMINP_JSON_TO_OM_JSON'

    OMINP_JSON_TO_OM_JSON = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_ominp_json_to_om_json_stage(
                                name = stage_name,
                                fileExt = stage_fileExt
                            )
                        )
    return OMINP_JSON_TO_OM_JSON

def get_om_json_to_csv_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OM_JSON_TO_CSV'

    OM_JSON_TO_CSV = StageHandler(name = name) \
                        .add_stage(stage = stages.get_om_json_to_csv_stage_stage(
                            name = stage_name,
                            fileExt = stage_fileExt
                        )
                    )
    return OM_JSON_TO_CSV

def get_om_csv_to_om_owl_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OM_CSV_TO_OM_OWL'
    OM_CSV_TO_OM_OWL = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_om_csv_to_om_owl_stage(
                                name = stage_name,
                                fileExt = stage_fileExt
                        )
                    )
    return OM_CSV_TO_OM_OWL