from chemaboxwriters.common.abox_stages import Abox_Writer_Stage

OM_ABOX_STAGES = Abox_Writer_Stage()
OM_ABOX_STAGES.register_stage(stage="ominp_json")
OM_ABOX_STAGES.register_stage(stage="om_json")
OM_ABOX_STAGES.register_stage(stage="om_csv")
OM_ABOX_STAGES.register_stage(stage="om_owl")
