import json
from chemaboxwriters.common.handler import IHandler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common import PREFIXES
from dataclasses import dataclass, field
from typing import List
from enum import Enum

omops_entry_prefix = PREFIXES["omops_entry_prefix"]


@dataclass
class OMINP_JSON_TO_OM_JSON_Handler(IHandler):
    """Handler converting ontomops ominp json files to om json.
    Inputs: List of ominp json file paths
    Outputs: List of om json file paths
    """

    name: str = field(default="OMINP_JSON_TO_OM_JSON")
    in_stages: List[Enum] = field(
        default_factory=lambda: [globals.aboxStages.OMINP_JSON]
    )
    out_stage: Enum = field(default=globals.aboxStages.OM_JSON)

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
            self.om_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    @staticmethod
    def om_jsonwriter(
        file_path: str, output_file_path: str, random_id: str = "", *args, **kwargs
    ) -> None:

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        if not random_id:
            random_id = utilsfunc.get_random_id()

        data[globals.ENTRY_UUID] = random_id
        data[globals.ENTRY_IRI] = omops_entry_prefix + random_id

        utilsfunc.write_dict_to_file(dict_data=data, dest_path=output_file_path)
