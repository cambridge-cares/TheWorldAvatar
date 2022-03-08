import json
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.ontopesscan.handlers.oc_json_handler import (
    SCAN_COORDINATE_ATOMS_IRIS,
    SCAN_COORDINATE_TYPE,
    SCAN_COORDINATE_UNIT,
    SCAN_COORDINATE_VALUE,
    SCAN_POINTS_JOBS,
    SCAN_ATOM_IDS,
)
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from typing import List, Optional, Dict
from enum import Enum


Abox_Writer = utilsfunc.Abox_csv_writer


class OPS_JSON_TO_OPS_CSV_Handler(Handler):
    """Handler converting ops_json files to ops_csv.
    Inputs: List of ops_json file paths
    Outputs: List of ops_csv file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="OPS_JSON_TO_OPS_CSV",
            in_stage=globals.aboxStages.OPS_JSON,
            out_stage=globals.aboxStages.OPS_CSV,
            endpoints_proxy=endpoints_proxy,
            required_configs={
                abconf.WRITERS_PREFIXES_KEY: [
                    "spec_pref",
                    "pes_pref",
                    "gain_pref",
                    "unit_pref",
                    "onto_spec",
                    "onto_comp",
                    "onto_pes",
                ]
            },
        )

    def _handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._ops_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **self._handler_kwargs,
            )
            outputs.append(out_file_path)
        return outputs

    def _ops_csvwriter(self, file_path: str, output_file_path: str, **handler_kwargs):

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        spec_IRI = data[globals.SPECIES_IRI]
        calc_id = data[globals.ENTRY_UUID]
        entryIRI = data[globals.ENTRY_IRI]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as aboxwriter:
            aboxwriter.write_header()

            self._write_initial(aboxwriter, entryIRI, spec_IRI)
            self._write_scancoordinate(aboxwriter, calc_id, data)
            self._write_scanpoints(aboxwriter, entryIRI, calc_id, data)

    def _write_initial(self, aboxwriter: Abox_Writer, entryIRI, spec_IRI):

        onto_pes = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_pes"]
        pes_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["pes_pref"]
        spec_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["spec_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        abox_name = "ABoxOntoPESSscan"
        aboxwriter.write_imports(abox_name=abox_name, importing=onto_pes)
        aboxwriter.write_imports(
            abox_name=abox_name, importing=pes_pref[:-1], relation="base"
        )
        aboxwriter.write_instance(
            inst_iri=f"{pes_pref}{entryIRI}",
            inst_class=f"{onto_pes}#PotentialEnergySurfaceScan",
        )
        aboxwriter.write_instance(
            inst_iri=f"{spec_pref}{spec_IRI[0]}",
            inst_class=f"{onto_spec}#Species",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{pes_pref}{entryIRI}",
            relation=f"{onto_pes}#onSpecies",
            trg_inst_iri=f"{spec_pref}{spec_IRI[0]}",
        )

    def _write_scancoordinate(self, aboxwriter: Abox_Writer, calc_id, data):

        onto_pes = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_pes"]
        pes_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["pes_pref"]
        spec_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["spec_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        scan_type = data[SCAN_COORDINATE_TYPE]

        aboxwriter.write_instance(
            inst_iri=f"{pes_pref}{scan_type}_{calc_id}",
            inst_class=f"{onto_pes}#{scan_type}",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{pes_pref}{data['EntryIRI']}",
            trg_inst_iri=f"{pes_pref}{scan_type}_{calc_id}",
            relation=f"{onto_pes}#hasScanCoordinate",
        )
        for atomiri in data[SCAN_COORDINATE_ATOMS_IRIS]:

            aboxwriter.write_instance(
                inst_iri=f"{spec_pref}{atomiri}",
                inst_class=f"{gain_pref}Atom",
            )

            aboxwriter.write_object_property(
                src_inst_iri=f"{pes_pref}{scan_type}_{calc_id}",
                trg_inst_iri=f"{spec_pref}{atomiri}",
                relation=f"{onto_pes}#hasScanAtom",
            )

    def _write_scanpoints(self, aboxwriter: Abox_Writer, entryIRI, calc_id, data):

        onto_pes = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_pes"]
        pes_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["pes_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        for k in range(len(data[SCAN_COORDINATE_VALUE])):
            gauss_type = data[SCAN_POINTS_JOBS][k].split("_")[0][-3:]

            aboxwriter.write_instance(
                inst_iri=f"{pes_pref}ScanPoint_{calc_id}_{k + 1}",
                inst_class=f"{onto_pes}#ScanPoint",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{pes_pref}{entryIRI}",
                trg_inst_iri=f"{pes_pref}ScanPoint_{calc_id}_{k + 1}",
                relation=f"{onto_pes}#hasScanPoint",
            )
            aboxwriter.write_instance(
                inst_iri=data[SCAN_POINTS_JOBS][k],
                inst_class=f"{onto_comp}#{gauss_type}",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{pes_pref}ScanPoint_{calc_id}_{k + 1}",
                trg_inst_iri=data[SCAN_POINTS_JOBS][k],
                relation=f"{onto_pes}#hasCalculation",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{pes_pref}ScanPoint_{calc_id}_{k + 1}",
                relation=f"{onto_pes}#hasInputAtomIds",
                value=data[SCAN_ATOM_IDS],
                data_type="String",
            )
            aboxwriter.write_instance(
                inst_iri=f"{pes_pref}ScanCoordinateValue_{calc_id}_{k + 1}",
                inst_class=f"{onto_pes}#ScanCoordinateValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{pes_pref}ScanPoint_{calc_id}_{k + 1}",
                trg_inst_iri=f"{pes_pref}ScanCoordinateValue_{calc_id}_{k + 1}",
                relation=f"{onto_pes}#hasScanCoordinateValue",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{pes_pref}ScanCoordinateValue_{calc_id}_{k + 1}",
                relation=f"{gain_pref}hasValue",
                value=data[SCAN_COORDINATE_VALUE][k],
                data_type="String",
            )

            scan_unit = ""

            if data[SCAN_COORDINATE_UNIT] == "Angstrom":
                scan_unit = f"{unit_pref}unit#Angstrom"

            elif data[SCAN_COORDINATE_UNIT] == "Degrees":
                scan_unit = f"{unit_pref}unit#DegreeAngle"

            if scan_unit:
                aboxwriter.write_object_property(
                    src_inst_iri=f"{pes_pref}ScanCoordinateValue_{calc_id}_{k + 1}",
                    trg_inst_iri=scan_unit,
                    relation=f"{gain_pref}hasUnit",
                )
