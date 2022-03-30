class Abox_Writer_Stage:
    def __init__(self) -> None:
        self._stages = {}

    def register_stage(self, stage: str) -> None:
        self._stages[stage] = stage

    def __getattr__(self, stage: str) -> None:
        return self._stages[stage]

    def merge_stages(self, other: "Abox_Writer_Stage") -> None:
        self._stages = {**other._stages, **self._stages}


ABOX_STAGES_COMMON = Abox_Writer_Stage()
ABOX_STAGES_COMMON.register_stage("qc_log")
ABOX_STAGES_COMMON.register_stage("qc_json")
