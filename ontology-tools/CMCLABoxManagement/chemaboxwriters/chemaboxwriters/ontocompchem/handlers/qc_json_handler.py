import chemaboxwriters.kgoperations.querytemplates as querytemplates
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
import chemaboxwriters.common.params as params
from compchemparser.parsers.ccgaussian_parser import PROGRAM_NAME, PROGRAM_VERSION
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.kgoperations.remotestore_client as rsc
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES
import json
from typing import List
import logging

logger = logging.getLogger(__name__)


HANDLER_PREFIXES = {
    "comp_pref": {"required": True},
}

HANDLER_PARAMETERS = {
    "random_id": {"required": False},
    "ontospecies_IRI": {"required": False},
    "generate_png": {"required": False},
}

PNG_SOURCE_LOCATION = "png_source_location"
XML_SOURCE_LOCATION = "xml_source_location"


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
        comp_pref = self.get_prefix_value(name="comp_pref")

        if random_id is None:
            random_id = utilsfunc.get_random_id()

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        xyz = get_xyz_from_parsed_json(data)
        inchi = obconverter.obConvert(xyz, "xyz", "inchi")

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
        data[params.SPECIES_IRI] = ontospecies_IRI
        data[params.ENTRY_IRI] = f"{comp_pref}{jobType}_{random_id}"
        data[params.ENTRY_UUID] = random_id

        utilsfunc.write_dict_to_file(dict_data=data, dest_path=output_file_path)
