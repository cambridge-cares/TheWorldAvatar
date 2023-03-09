import chemaboxwriters.ontopesscan.handlers.ops_json_keys as ops_keys
import chemaboxwriters.ontocompchem.handlers.oc_json_keys as oc_keys
import chemutils.mathutils.linalg as linalg
import chemaboxwriters.common.utilsfunc as utilsfunc
import json
import numpy as np
from chemaboxwriters.common.handler import Handler
from chemaboxwriters.ontopesscan.abox_stages import OPS_ABOX_STAGES
from chemaboxwriters.ontopesscan import OPS_SCHEMA
from typing import List, Tuple


HANDLER_PARAMETERS = {
    "os_iris": {"required": True},
    "os_atoms_iris": {"required": True},
    "oc_atoms_pos": {"required": True},
    "random_id": {"required": False},
}


class OC_JSON_TO_OPS_JSON_Handler(Handler):
    """Handler converting oc_json files to ops_json.
    Inputs: List of oc_json file paths
    Outputs: List of ops_json file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="OC_JSON_TO_OPS_JSON",
            in_stage=OPS_ABOX_STAGES.oc_json,  # type: ignore
            out_stage=OPS_ABOX_STAGES.ops_json,  # type: ignore
            handler_params=HANDLER_PARAMETERS,
        )

    def handle_input(
        self, inputs: List[str], out_dir: str, *args, **kwargs
    ) -> List[str]:

        outputs: List[str] = []
        out_file_path = utilsfunc.get_out_file_path(
            input_file_path=sorted(inputs)[0],
            file_extension=self._out_stage,
            out_dir=out_dir,
        )
        self._ops_jsonwriter(oc_json_files=inputs, output_file_path=out_file_path)
        outputs.append(out_file_path)
        return outputs

    def _ops_jsonwriter(self, oc_json_files: List[str], output_file_path: str):

        # read handler params
        random_id = self.get_parameter_value(
            name="random_id", default=utilsfunc.get_random_id()
        )
        os_iris: str = self.get_parameter_value(
            name="os_iris", default=""
        )  # type: ignore
        os_atoms_iris: str = self.get_parameter_value(
            name="os_atoms_iris", default=""
        )  # type: ignore
        oc_atoms_pos: str = self.get_parameter_value(
            name="oc_atoms_pos", default=""
        )  # type: ignore

        # create an empty oc json out dict
        data_out = dict.fromkeys(ops_keys.OPS_JSON_KEYS, None)

        # assign values from the passed params
        data_out[ops_keys.SPECIES_IRI] = os_iris.split(",")
        data_out[ops_keys.SCAN_ATOMS_IRIS] = [
            iri.strip() for iri in os_atoms_iris.split(",")
        ]
        data_out[ops_keys.SCAN_ATOMS_CCJOB_POSITIONS] = " ".join(
            oc_atoms_pos.split(",")
        )
        data_out[ops_keys.ENTRY_ID] = random_id
        # assign the main ops entry IRI
        main_inst_pref = utilsfunc.read_main_pref_from_schema(
            schema_file=OPS_SCHEMA, main_pref_name="main_inst_pref"
        )
        data_out[ops_keys.ENTRY_IRI] = f"{main_inst_pref}{random_id}"

        # now read oc json files and assign values
        scan_coord_values = []
        oc_jobs = []
        atom_pos_int = list(map(lambda x: int(x) - 1, oc_atoms_pos.split(",")))

        for oc_file in oc_json_files:
            with open(oc_file, "r") as file_handle:
                oc_data_in = json.load(file_handle)

            oc_jobs.append(oc_data_in[oc_keys.ENTRY_IRI])

            coord_value, scan_type, coord_unit = self._get_scan_coord_data(
                atom_pos=atom_pos_int,
                x_coord=oc_data_in[oc_keys.X_ATOMS_COORDINATES],
                y_coord=oc_data_in[oc_keys.Y_ATOMS_COORDINATES],
                z_coord=oc_data_in[oc_keys.Z_ATOMS_COORDINATES],
            )
            scan_coord_values.append(coord_value)

            data_out[ops_keys.SCAN_COORDINATE_UNIT] = coord_unit
            data_out[ops_keys.SCAN_COORDINATE_TYPE] = scan_type

        scan_coord_values, oc_jobs = zip(*sorted(zip(scan_coord_values, oc_jobs)))
        scan_coord_values = list(scan_coord_values)
        oc_jobs = list(oc_jobs)

        data_out[ops_keys.SCAN_COORDINATE_VALUES] = scan_coord_values
        data_out[ops_keys.SCAN_POINT_JOBS_IRIS] = oc_jobs

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)

    def _get_scan_coord_data(
        self,
        atom_pos: List[int],
        x_coord: List[float],
        y_coord: List[float],
        z_coord: List[float],
    ) -> Tuple[float, str, str]:
        xyz = np.array(
            list(
                zip(
                    x_coord,
                    y_coord,
                    z_coord,
                )
            )
        )
        scanAtomsPos = xyz[atom_pos]

        if len(atom_pos) == 2:
            value = round(linalg.getXYZPointsDistance(*scanAtomsPos), 3)
            scan_type = "DistanceCoordinate"
            coord_unit = "Angstrom"
        elif len(atom_pos) == 3:
            value = round(linalg.getPlaneAngle(*scanAtomsPos), 3)
            scan_type = "AngleCoordinate"
            coord_unit = "DegreeAngle"
        else:
            value = round(linalg.getDihedralAngle(*scanAtomsPos), 3)
            scan_type = "DihedralAngleCoordinate"
            coord_unit = "DegreeAngle"
        return round(value, 3), scan_type, coord_unit
