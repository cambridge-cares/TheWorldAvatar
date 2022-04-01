import json
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.common.params as params
from chemaboxwriters.ontomops.abox_stages import OM_ABOX_STAGES
import os
from typing import List


HANDLER_PREFIXES = {
    "omops_entry_prefix": {"required": True},
}

HANDLER_PARAMETERS = {
    "random_id": {"required": False},
}

MOPS_XYZ_GEOMETRY_FILE_URL = "Mops_xyz_geometry_file_url"


class OMINP_JSON_TO_OM_JSON_Handler(Handler):
    """Handler converting ontomops ominp_json files to om_json.
    Inputs: List of ominp_json file paths
    Outputs: List of om_json file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="OMINP_JSON_TO_OM_JSON",
            in_stage=OM_ABOX_STAGES.ominp_json,  # type: ignore
            out_stage=OM_ABOX_STAGES.om_json,  # type: ignore
            prefixes=HANDLER_PREFIXES,
            handler_params=HANDLER_PARAMETERS,
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage,
                out_dir=out_dir,
            )

            self.om_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                dry_run=dry_run,
            )
            outputs.append(out_file_path)
        return outputs

    def om_jsonwriter(
        self, file_path: str, output_file_path: str, dry_run: bool
    ) -> None:

        omops_entry_prefix = self.get_prefix_value(name="omops_entry_prefix")
        random_id = self.get_parameter_value(name="random_id")

        if omops_entry_prefix is None:
            omops_entry_prefix = ""

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        xyz_file = data.get("Mops_XYZ_coordinates_file")
        if xyz_file is not None:
            xyz_file = os.path.abspath(xyz_file)
            self.do_fs_uploads(
                inputs=[xyz_file],
                input_type="ominp_xyz",
                dry_run=dry_run,
            )
            xyz_file_loc = self.get_fs_upload_location(upload_file=xyz_file)
            if xyz_file_loc is not None:
                data[MOPS_XYZ_GEOMETRY_FILE_URL] = xyz_file_loc

        if random_id is None:
            random_id = utilsfunc.get_random_id()

        data[params.ENTRY_UUID] = random_id
        data[params.ENTRY_IRI] = f"{omops_entry_prefix}{random_id}"

        utilsfunc.write_dict_to_file(dict_data=data, dest_path=output_file_path)
