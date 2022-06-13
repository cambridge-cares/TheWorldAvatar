from chemaboxwriters.common.abox_stages import Abox_Writer_Stage, ABOX_STAGES_COMMON

OC_ABOX_STAGES = Abox_Writer_Stage()

OC_ABOX_STAGES.register_stage(stage="oc_json")
OC_ABOX_STAGES.register_stage(stage="oc_csv")
OC_ABOX_STAGES.register_stage(stage="oc_owl")
OC_ABOX_STAGES.merge_stages(other=ABOX_STAGES_COMMON)
