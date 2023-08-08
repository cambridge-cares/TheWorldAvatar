from chemaboxwriters.common.abox_stages import Abox_Writer_Stage
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES

OPS_ABOX_STAGES = Abox_Writer_Stage()

OPS_ABOX_STAGES.register_stage(stage=OC_ABOX_STAGES.oc_json)  # type: ignore
OPS_ABOX_STAGES.register_stage(stage="ops_json")
OPS_ABOX_STAGES.register_stage(stage="ops_csv")
OPS_ABOX_STAGES.register_stage(stage="ops_owl")
