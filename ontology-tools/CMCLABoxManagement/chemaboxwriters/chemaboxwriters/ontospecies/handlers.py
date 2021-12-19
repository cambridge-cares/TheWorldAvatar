from chemaboxwriters.common.base import StageHandler
import chemaboxwriters.ontospecies.stages as stages
from typing import Optional

def get_qc_json_to_os_json_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'QC_JSON_TO_OS_JSON'

    QC_JSON_TO_OS_JSON = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_qc_json_to_os_json_stage(
                                name = stage_name,
                                fileExt = stage_fileExt
                            )
                        )
    return QC_JSON_TO_OS_JSON

def get_os_json_to_csv_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OS_JSON_TO_CSV'

    OS_JSON_TO_CSV = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_os_json_to_csv_stage(
                                name = stage_name,
                                fileExt = stage_fileExt
                            )
                        )
    return OS_JSON_TO_CSV


def get_os_csv_to_os_owl_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OS_CSV_TO_OS_OWL'

    OS_CSV_TO_OS_OWL = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_os_csv_to_os_owl_stage(
                                name = stage_name,
                                fileExt = stage_fileExt
                            )
                        )
    return OS_CSV_TO_OS_OWL