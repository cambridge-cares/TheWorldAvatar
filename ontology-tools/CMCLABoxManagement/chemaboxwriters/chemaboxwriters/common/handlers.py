from chemaboxwriters.common.handler import Handler
import compchemparser.app as qcparser
import chemaboxwriters.common.utilsfunc as utilsfunc
import entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF as entityrdfizer
from typing import List
from chemaboxwriters.common.abox_stages import ABOX_STAGES_COMMON
import json

CCLOG_SOURCE_LOCATION = "cclog_source_location"


class QC_LOG_TO_QC_JSON_Handler(Handler):
    """Gaussian quantum claculations log files handler.
    Inputs: List of gaussian log file paths
    Outputs: List of parsed json file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="QC_LOG_TO_QC_JSON",
            in_stage=ABOX_STAGES_COMMON.qc_log,  # type: ignore
            out_stage=ABOX_STAGES_COMMON.qc_json,  # type: ignore
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
    ) -> List[str]:

        outputs: List[str] = []
        # cclog_upload_loc = None
        for cclog_file_path in inputs:
            cclog_upload_loc = self.get_fs_upload_location(upload_file=cclog_file_path)

            cclog_parsed_jobs = qcparser.parseLog(cclog_file_path)

            for i, cclog_parsed_job in enumerate(cclog_parsed_jobs):
                inp_file_suffix = f"_{i+1}" if len(cclog_parsed_jobs) > 1 else ""

                out_file_path = utilsfunc.get_out_file_path(
                    input_file_path=f"{cclog_file_path}{inp_file_suffix}",
                    file_extension=self._out_stage.lower(),
                    out_dir=out_dir,
                    replace_last_ext=False,
                )
                cclog_dict_data = json.loads(cclog_parsed_job)
                if cclog_upload_loc is not None:
                    cclog_dict_data[CCLOG_SOURCE_LOCATION] = cclog_upload_loc

                utilsfunc.write_dict_to_file(
                    dict_data=cclog_dict_data,
                    dest_path=out_file_path,
                )
                outputs.append(out_file_path)
        return outputs


class CSV_TO_OWL_Handler(Handler):
    """Handler converting csv files to owl.
    Inputs: List of csv file paths
    Outputs: List of owl file paths
    """

    def __init__(
        self,
        name: str,
        in_stage: str,
        out_stage: str,
    ) -> None:

        super().__init__(
            name=name,
            in_stage=in_stage,
            out_stage=out_stage,
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
    ) -> List[str]:

        outputs: List[str] = []
        for csv_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=csv_file_path,
                file_extension=self._out_stage,
                out_dir=out_dir,
            )
            with open(csv_file_path, "r") as csvfile:
                owl_string = entityrdfizer.convert_csv_string_into_rdf(
                    csv_string=csvfile.read()
                )
            with open(out_file_path, "w") as owlfile:
                owlfile.write(owl_string)

            outputs.append(out_file_path)
        return outputs
