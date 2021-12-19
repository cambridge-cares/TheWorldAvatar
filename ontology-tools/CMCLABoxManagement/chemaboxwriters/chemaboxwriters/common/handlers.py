from chemaboxwriters.common.base import StageHandler
import chemaboxwriters.common.stages as stages
from enum import Enum
from typing import Optional

def get_qc_log_to_qc_json_handler(
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    stage_inpFileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'QC_LOG_TO_QC_JSON'

    QC_LOG_TO_QC_JSON = StageHandler(name=name) \
                        .add_stage(
                            stage=stages.get_qc_log_to_qc_json_stage(
                                name = stage_name,
                                fileExt= stage_fileExt,
                                inpFileExt=stage_inpFileExt
                            )
                        )

    return QC_LOG_TO_QC_JSON

def get_csv_to_owl_handler(
    inStage: Enum,
    outStage: Enum,
    name: Optional[str] = None,
    stage_name: Optional[str] = None,
    stage_fileExt: Optional[str] = None,
    )->StageHandler:

    if name is None: name = 'CSV_TO_OWL'

    CSV_TO_OWL = StageHandler(name=name) \
                .add_stage(
                    stage=stages.get_csv_to_owl_stage(
                        inStage=inStage,
                        outStage=outStage,
                        name= stage_name,
                        fileExt= stage_fileExt
                    )
                )
    return CSV_TO_OWL

