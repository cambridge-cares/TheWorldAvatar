from chemaboxwriters.common.base import StageHandler
from chemaboxwriters.common.stageenums import aboxStages
import chemaboxwriters.ontopesscan.stages as stages
import chemaboxwriters.ontocompchem.stages as oc_stages
import chemaboxwriters.common.stages as common_stages
from typing import Optional


def get_qc_log_to_oc_json_handler(
    name: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'QC_LOG_TO_OC_JSON'

    QC_LOG_TO_OC_JSON = StageHandler(
                            name='QC_LOG_TO_OC_JSON',
                            outStage = aboxStages.OC_JSON) \
                        .add_stage(stage = common_stages.get_qc_log_to_qc_json_stage()) \
                        .add_stage(stage = oc_stages.get_qc_json_to_oc_json_stage()) \
                        .add_stage(stage = oc_stages.get_oc_json_to_oc_csv_stage()) \
                        .add_stage(stage = oc_stages.get_oc_csv_to_owl_stage())
    return QC_LOG_TO_OC_JSON

def get_oc_json_to_ops_json_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OC_JSON_TO_OPS_JSON'

    OC_JSON_TO_OPS_JSON = StageHandler(name=name) \
                        .add_stage(
                            stage = stages.get_oc_json_to_ops_json_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return OC_JSON_TO_OPS_JSON


def get_ops_json_to_csv_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OPS_JSON_TO_CSV'

    OPS_JSON_TO_CSV = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_ops_json_to_csv_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return OPS_JSON_TO_CSV

def get_ops_csv_to_ops_owl_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'OPS_CSV_TO_OPS_OWL'

    OPS_CSV_TO_OPS_OWL = StageHandler(name = name) \
                        .add_stage(
                            stage = stages.get_ops_csv_to_ops_owl_stage(
                                name = stage_name,
                                fileExt= stage_fileExt
                            )
                        )
    return OPS_CSV_TO_OPS_OWL