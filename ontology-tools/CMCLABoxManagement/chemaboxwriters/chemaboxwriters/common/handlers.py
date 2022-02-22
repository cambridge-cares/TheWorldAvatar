from chemaboxwriters.common.handler import IHandler
from compchemparser.app import parseLog
import chemaboxwriters.common.utilsfunc as utilsfunc
import entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF as entityrdfizer
from typing import List
from enum import Enum
from chemaboxwriters.common.globals import aboxStages
from dataclasses import dataclass, field
import json


@dataclass
class QC_LOG_TO_QC_JSON_Handler(IHandler):
    """Gaussian quantum claculations log files handler.
    Inputs: List of gaussian log file paths
    Outputs: List of parsed json file paths
    """

    name: str = field(default="QC_LOG_TO_QC_JSON")
    in_stages: List[Enum] = field(default_factory=lambda: [aboxStages.QC_LOG])
    out_stage: Enum = field(default=aboxStages.QC_JSON)

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for cclog_file_path in inputs:
            cclog_parsed_jobs = parseLog(cclog_file_path)

            if len(cclog_parsed_jobs) == 1:
                out_file_path = utilsfunc.get_out_file_path(
                    input_file_path=cclog_file_path,
                    file_extension=self.out_stage.name.lower(),
                    out_dir=out_dir,
                    replace_last_ext=False,
                )
                utilsfunc.write_dict_to_file(
                    dict_data=json.loads(cclog_parsed_jobs[0]), dest_path=out_file_path
                )
                outputs.append(out_file_path)
            else:
                for i, cclog_parsed_job in enumerate(cclog_parsed_jobs):
                    out_file_path = utilsfunc.get_out_file_path(
                        input_file_path=f"{cclog_file_path}_{i+1}",
                        file_extension=self.out_stage.name.lower(),
                        out_dir=out_dir,
                        replace_last_ext=False,
                    )
                    utilsfunc.write_dict_to_file(
                        dict_data=json.loads(cclog_parsed_job),
                        dest_path=out_file_path,
                    )
                    outputs.append(out_file_path)
        return outputs


class CSV_TO_OWL_Handler(IHandler):
    """Handler converting csv files to owl.
    Inputs: List of csv file paths
    Outputs: List of owl file paths
    """

    name: str = field(default="CSV_TO_OWL")

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for csv_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=csv_file_path,
                file_extension=self.out_stage.name.lower(),
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
