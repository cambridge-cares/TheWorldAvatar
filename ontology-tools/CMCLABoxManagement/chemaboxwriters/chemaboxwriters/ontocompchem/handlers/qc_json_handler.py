import chemaboxwriters.kgoperations.querytemplates as querytemplates
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
import chemaboxwriters.common.params as params
from compchemparser.parsers.ccgaussian_parser import (
    PROGRAM_NAME,
    PROGRAM_VERSION,
    ATOM_COUNTS,
    FREQ,
    ROT_CONST,
    ELECTRONIC_ENERGY,
    ELECTRONIC_ZPE_ENERGY,
    EMP_FORMULA,
    ATOM_TYPES,
    GEOM,
    ROT_SYM_NR,
)
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.kgoperations.remotestore_client as rsc
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES
import json
from typing import List
from chemaboxwriters.ontocompchem import OC_SCHEMA
import logging


logger = logging.getLogger(__name__)

HANDLER_PARAMETERS = {
    "random_id": {"required": False},
    "ontospecies_IRI": {"required": False},
    "generate_png": {"required": False},
}

PNG_SOURCE_LOCATION = "png_source_location"
XML_SOURCE_LOCATION = "xml_source_location"
UNIQUE_ATOMS = "Unique_Atoms"
TOTAL_ATOMS_COUNTS = "Total_Atom_Counts"
FREQ_STRING = "Frequencies_String"
ROT_CONST_STRING = "Rotational_Constants_String"
ZPE_ENERGY = "Zero_Point_Energy"
ATOM_INDICES = "AtomsIndices"
COORD_X = "CoordinateX"
COORD_Y = "CoordinateY"
COORD_Z = "CoordinateZ"


class QC_JSON_TO_OC_JSON_Handler(Handler):
    """Handler converting qc_json files to oc_json.
    Inputs: List of qc_json file paths
    Outputs: List of oc_json file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="QC_JSON_TO_OC_JSON",
            in_stage=OC_ABOX_STAGES.qc_json,  # type: ignore
            out_stage=OC_ABOX_STAGES.oc_json,  # type: ignore
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
            self._oc_jsonwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                dry_run=dry_run,
            )
            outputs.append(out_file_path)
        return outputs

    def _oc_jsonwriter(
        self,
        file_path: str,
        output_file_path: str,
        dry_run: bool,
    ) -> None:

        random_id = self.get_parameter_value(name="random_id")
        ontospecies_IRI = self.get_parameter_value(name="ontospecies_IRI")
        generate_png = self.get_parameter_value(name="generate_png")

        if random_id is None:
            random_id = utilsfunc.get_random_id()

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        xyz = get_xyz_from_parsed_json(data)
        inchi = obconverter.obConvert(xyz, "xyz", "inchi")

        if ROT_SYM_NR in data:
            data[ROT_SYM_NR] = int(data[ROT_SYM_NR])

        if generate_png is True:
            png_out_path = f"{output_file_path}.png"
            utilsfunc.generate_molecule_png(inchi=inchi, out_path=png_out_path)
            self.do_fs_uploads(
                inputs=[png_out_path],
                input_type="oc_png",
                dry_run=dry_run,
            )
            png_file_loc = self.get_fs_upload_location(upload_file=png_out_path)
            if png_file_loc is not None:
                data[PNG_SOURCE_LOCATION] = png_file_loc
            self.written_files.append(png_out_path)

        if ontospecies_IRI is None:
            response = self.do_remote_store_query(
                endpoint_prefix="ospecies",
                store_client_class=rsc.SPARQLWrapperRemoteStoreClient,
                query_str=querytemplates.spec_inchi_query(inchi),
            )
            if response:
                ontospecies_IRI = response[0]["speciesIRI"]

        # at the moment we only support gaussian
        jobType = ""
        if "Gaussian" in data[PROGRAM_NAME]:
            if PROGRAM_VERSION in data:
                jobType = "G" + data[PROGRAM_VERSION][2:4]
            else:
                jobType = "Gxx"

        data[EMP_FORMULA] = utilsfunc.clean_qc_json_emp_formula(
            emp_formula=data[EMP_FORMULA]
        )

        coord_x, coord_y, coord_z = utilsfunc.split_qc_json_geom_to_xyz_coords(
            data[GEOM]
        )
        data[COORD_X] = coord_x
        data[COORD_Y] = coord_y
        data[COORD_Z] = coord_z

        data[ATOM_INDICES] = utilsfunc.get_atom_indices_from_qc_json(data[ATOM_TYPES])

        data[params.SPECIES_IRI] = ontospecies_IRI

        main_inst_pref = utilsfunc.read_main_pref_from_schema(
            schema_file=OC_SCHEMA, main_pref_name="main_inst_pref"
        )
        data[params.ENTRY_IRI] = f"{main_inst_pref}{jobType}_{random_id}"
        data[params.ENTRY_UUID] = random_id

        if ATOM_COUNTS in data:
            data[UNIQUE_ATOMS] = list(data[ATOM_COUNTS].keys())
            data[TOTAL_ATOMS_COUNTS] = list(data[ATOM_COUNTS].values())

        if FREQ in data:
            data[FREQ_STRING] = " ".join(str(i) for i in data[FREQ])

        if ROT_CONST in data:
            data[ROT_CONST_STRING] = " ".join(str(i) for i in data[ROT_CONST])

        if ELECTRONIC_ZPE_ENERGY in data and ELECTRONIC_ENERGY in data:
            data[ZPE_ENERGY] = data.pop(ELECTRONIC_ZPE_ENERGY) - data[ELECTRONIC_ENERGY]

        utilsfunc.write_dict_to_file(dict_data=data, dest_path=output_file_path)
