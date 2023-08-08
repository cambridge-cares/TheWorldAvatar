import chemaboxwriters.kgoperations.querytemplates as querytemplates
import chemutils.obabelutils.obconverter as obconverter
from compchemparser.helpers.utils import get_xyz_from_parsed_json
import chemaboxwriters.ontocompchem.handlers.oc_json_keys as oc_keys
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
import chemaboxwriters.kgoperations.remotestore_client as rsc
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES
import json
from typing import List, Optional
from chemaboxwriters.ontocompchem import OC_SCHEMA
import logging


logger = logging.getLogger(__name__)


HANDLER_PARAMETERS = {
    "random_id": {"required": False},
    "ontospecies_IRI": {"required": False},
    "generate_png": {"required": False},
}


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
        self, inputs: List[str], out_dir: str, dry_run: bool, *args, **kwargs
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

        # get handler params
        random_id = self.get_parameter_value(
            name="random_id", default=utilsfunc.get_random_id()
        )
        generate_png = self.get_parameter_value(name="generate_png", default=False)

        # read in the incoming qc json data file
        with open(file_path, "r") as file_handle:
            data_in = json.load(file_handle)

        # create an empty oc json out dict
        data_out = dict.fromkeys(oc_keys.OC_JSON_KEYS, None)

        # populate the selected oc json entries with the qc json data
        # apply any defined post processing steps
        # --------------------------------------------------
        for oc_key, qc_oc_map in oc_keys.OC_JSON_TO_QC_JSON_KEYS_MAP.items():
            qc_values = [data_in.get(key) for key in qc_oc_map["cckeys"]]
            data_out[oc_key] = qc_oc_map["postproc_func"](qc_values)

        # now add any remaining oc json data entries
        # --------------------------------------------------
        inchi = obconverter.obConvert(get_xyz_from_parsed_json(data_in), "xyz", "inchi")

        data_out[oc_keys.SPECIES_IRI] = self.get_parameter_value(
            name="ontospecies_IRI", default=self._search_ontospecies_iri(inchi=inchi)
        )

        if generate_png is True:
            data_out[oc_keys.PNG_SOURCE_LOCATION] = self._process_png_input(
                inchi=inchi, output_file_path=output_file_path, dry_run=dry_run
            )

        # at the moment we only support gaussian
        jobType = self._get_job_type(
            program_name=data_out[oc_keys.PROGRAM_NAME],
            program_version=data_out[oc_keys.PROGRAM_VERSION],
        )

        main_inst_pref = utilsfunc.read_main_pref_from_schema(
            schema_file=OC_SCHEMA, main_pref_name="main_inst_pref"
        )
        data_out[oc_keys.ENTRY_IRI] = f"{main_inst_pref}{jobType}_{random_id}"
        data_out[oc_keys.ENTRY_ID] = random_id

        utilsfunc.write_dict_to_file(dict_data=data_out, dest_path=output_file_path)

    def _process_png_input(
        self, inchi: str, output_file_path: str, dry_run: bool
    ) -> Optional[str]:
        png_out_path = f"{output_file_path}.png"
        self._generate_molecule_png(output_file_path=png_out_path, inchi=inchi)
        png_file_loc = self._upload_mol_png(png_out_path=png_out_path, dry_run=dry_run)

        return png_file_loc

    def _generate_molecule_png(self, output_file_path: str, inchi: str) -> None:
        utilsfunc.generate_molecule_png(inchi=inchi, out_path=output_file_path)
        self.written_files.append(output_file_path)

    def _upload_mol_png(self, png_out_path: str, dry_run: bool) -> Optional[str]:
        self.do_fs_uploads(
            inputs=[png_out_path],
            input_type="oc_png",
            dry_run=dry_run,
        )
        return self.get_fs_upload_location(upload_file=png_out_path)

    def _search_ontospecies_iri(self, inchi: str) -> Optional[str]:
        ontospecies_IRI = None
        # note that in this query we use the SPARQLWrapper remote
        # store client rather than the JPS one. This is because
        # it was impossible for me to get the JPS client to work
        # with the inchi query...
        response = self.do_remote_store_query(
            endpoint_prefix="ospecies",
            store_client_class=rsc.SPARQLWrapperRemoteStoreClient,
            query_str=querytemplates.spec_inchi_query(inchi),
        )
        if response:
            ontospecies_IRI = response[0]["speciesIRI"]
        return ontospecies_IRI

    def _get_job_type(
        self, program_name: Optional[str], program_version: Optional[str]
    ) -> str:
        job_type = ""
        if program_name is None:
            return job_type
        if "Gaussian" in program_name:
            if program_version is not None:
                # the way we extract job type here is ugly and needs to be fixed at
                #  some point..
                job_type = "G" + program_version[2:4]
            else:
                job_type = "Gxx"
        return job_type
