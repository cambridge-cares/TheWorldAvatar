from chemaboxwriters.common.handler import Handler
import compchemparser.app as qcparser
import chemaboxwriters.common.utilsfunc as utilsfunc
import entityrdfizer.aboxgenerator.ABoxTemplateCSVFileToRDF as entityrdfizer
from typing import List, Optional, Dict
from enum import Enum
from chemaboxwriters.common.endpoints_config import Endpoints_proxy
from chemaboxwriters.common.globals import aboxStages
import json


class QC_LOG_TO_QC_JSON_Handler(Handler):
    """Gaussian quantum claculations log files handler.
    Inputs: List of gaussian log file paths
    Outputs: List of parsed json file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="QC_LOG_TO_QC_JSON",
            in_stage=aboxStages.QC_LOG,
            out_stage=aboxStages.QC_JSON,
            endpoints_proxy=endpoints_proxy,
        )

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for cclog_file_path in inputs:
            cclog_parsed_jobs = qcparser.parseLog(cclog_file_path)

            if len(cclog_parsed_jobs) == 1:
                out_file_path = utilsfunc.get_out_file_path(
                    input_file_path=cclog_file_path,
                    file_extension=self._out_stage.name.lower(),
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
                        file_extension=self._out_stage.name.lower(),
                        out_dir=out_dir,
                        replace_last_ext=False,
                    )
                    utilsfunc.write_dict_to_file(
                        dict_data=json.loads(cclog_parsed_job),
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
        in_stage: Enum,
        out_stage: Enum,
        endpoints_proxy: Optional[Endpoints_proxy] = None,
    ) -> None:

        super().__init__(
            name=name,
            in_stage=in_stage,
            out_stage=out_stage,
            endpoints_proxy=endpoints_proxy,
        )

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for csv_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=csv_file_path,
                file_extension=self._out_stage.name.lower(),
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
