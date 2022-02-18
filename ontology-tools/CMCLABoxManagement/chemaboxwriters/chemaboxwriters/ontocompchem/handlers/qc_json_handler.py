import chemaboxwriters.kgoperations.querytemplates as querytemplates
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
from chemaboxwriters.common.utilsfunc import get_random_id
import json
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common import PREFIXES
from compchemparser.parsers.ccgaussian_parser import PROGRAM_NAME, PROGRAM_VERSION
from chemaboxwriters.common.globals import aboxStages
from chemaboxwriters.common.handler import IHandler
import chemaboxwriters.common.utilsfunc as utilsfunc
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional

comp_pref = PREFIXES["comp_pref"]


@dataclass
class QC_JSON_TO_OC_JSON_Handler(IHandler):
    """Handler converting qc json files to oc json.
    Inputs: List of qc json file paths
    Outputs: List of oc json file paths
    """

    name: str = field(default="QC_JSON_TO_OC_JSON")
    in_stages: List[Enum] = field(default_factory=lambda: [aboxStages.QC_JSON])
    out_stage: Enum = field(default=aboxStages.OC_JSON)

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        **handler_kwargs
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self.out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._oc_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    @staticmethod
    def _oc_jsonwriter(
        file_path: str,
        output_file_path: str,
        random_id: str = "",
        spec_IRI: Optional[str] = None,
    ) -> None:

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        xyz = get_xyz_from_parsed_json(data)
        inchi = obconverter.obConvert(xyz, "xyz", "inchi")
        if spec_IRI is None:
            spec_IRI = querytemplates.get_species_iri(inchi)
        if not random_id:
            random_id = get_random_id()

        # at the moment we only support gaussian
        jobType = ""
        if "Gaussian" in data[PROGRAM_NAME]:
            if PROGRAM_VERSION in data:
                jobType = "G" + data[PROGRAM_VERSION][2:4]
            else:
                jobType = "Gxx"
        data[globals.SPECIES_IRI] = spec_IRI
        data[globals.ENTRY_IRI] = comp_pref + jobType + "_" + random_id
        data[globals.ENTRY_UUID] = random_id

        utilsfunc.write_dict_to_file(dict_data=data, dest_path=output_file_path)
