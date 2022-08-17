import json
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.ontomops.handlers.om_json_keys as om_keys
import chemaboxwriters.ontomops.handlers.ominp_json_keys as ominp_keys
from chemaboxwriters.ontomops.abox_stages import OM_ABOX_STAGES
import chemaboxwriters.kgoperations.querytemplates as qtmpl
import os
from chemaboxwriters.ontomops import OM_SCHEMA
from typing import List, Optional, Dict


HANDLER_PARAMETERS = {
    "random_id": {"required": False},
}


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
            handler_params=HANDLER_PARAMETERS,
        )

    def handle_input(
        self, inputs: List[str], out_dir: str, dry_run: bool, *args, **kwargs
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

        random_id = self.get_parameter_value(
            name="random_id", default=utilsfunc.get_random_id()
        )

        # read in the incoming ominp json data file
        with open(file_path, "r") as file_handle:
            ominp_data_in = json.load(file_handle)

        # create an empty om json out dict
        data_out = dict.fromkeys(om_keys.OM_JSON_KEYS, None)

        # populate the selected om json entries with the ominp json data
        # apply any defined post processing steps
        # --------------------------------------------------
        for om_key, ominp_om_map in om_keys.OM_JSON_TO_OMINP_JSON_KEYS_MAP.items():
            ominp_values = [ominp_data_in.get(key) for key in ominp_om_map["cckeys"]]
            data_out[om_key] = ominp_om_map["postproc_func"](ominp_values)

        data_out[om_keys.MOPS_XYZ_GEOMETRY_FILE_URL] = self._handle_xyz_file_data(
            xyz_file=ominp_data_in.get(ominp_keys.MOPS_XYZ_COORDINATES_FILE),
            dry_run=dry_run,
        )

        main_inst_pref = utilsfunc.read_main_pref_from_schema(
            schema_file=OM_SCHEMA, main_pref_name="main_inst_pref"
        )

        data_out[om_keys.ENTRY_ID] = random_id
        data_out[om_keys.ENTRY_IRI] = f"{main_inst_pref}{random_id}"

        assemblymodel = self.get_assembly_model_iri(om_data=data_out)

        if assemblymodel is not None:
            assemblymodel = assemblymodel.split("_")[-1]
        else:
            assemblymodel = random_id
        data_out[om_keys.ASSEMBLY_MODEL_ID] = assemblymodel

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)

    def _handle_xyz_file_data(
        self, xyz_file: Optional[str], dry_run: bool
    ) -> Optional[str]:

        if xyz_file is not None:
            xyz_file = os.path.abspath(xyz_file)
            self.do_fs_uploads(
                inputs=[xyz_file],
                input_type="ominp_xyz",
                dry_run=dry_run,
            )
            return self.get_fs_upload_location(upload_file=xyz_file)

    def get_assembly_model_iri(self, om_data: Dict) -> Optional[str]:

        mops_sym_point_group = om_data.get(ominp_keys.MOPS_SYMMETRY_POINT_GROUP)
        mops_gbu_mods = om_data.get(om_keys.GBU_MODULARITIES)
        mops_gbu_plan = om_data.get(om_keys.GBU_PLANARITIES)
        mops_gbu_nums = om_data.get(om_keys.GBU_NUMBERS)

        if (
            mops_sym_point_group is None
            or mops_gbu_mods is None
            or mops_gbu_plan is None
            or mops_gbu_nums is None
        ):
            return

        assemblymodel = None
        gbu_properties = []
        for i in range(len(mops_gbu_mods)):
            gbu_properties.append(
                {
                    "modularity": mops_gbu_mods[i],
                    "planarity": mops_gbu_plan[i],
                    "gbu_number": mops_gbu_nums[i],
                },
            )

        response = self.do_remote_store_query(
            endpoint_prefix="omops",
            query_str=qtmpl.get_assemblyModel(
                gbu_properties=gbu_properties, mops_symmetry=mops_sym_point_group
            ),
        )
        if response:
            assemblymodel = response[0]["AssemblyModel"]

        return assemblymodel
