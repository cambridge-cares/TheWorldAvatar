from chemaboxwriters.common.abox_stages import Abox_Writer_Stage, ABOX_STAGES_COMMON

OS_ABOX_STAGES = Abox_Writer_Stage()
OS_ABOX_STAGES.register_stage(stage="os_json")
OS_ABOX_STAGES.register_stage(stage="os_csv")
OS_ABOX_STAGES.register_stage(stage="os_owl")
OS_ABOX_STAGES.merge_stages(other=ABOX_STAGES_COMMON)
