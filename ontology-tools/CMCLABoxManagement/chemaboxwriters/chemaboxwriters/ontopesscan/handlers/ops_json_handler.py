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
                "prefixes": [
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

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            if self._required_configs is not None:
                for prefix_name in self._required_configs["prefixes"]:
                    prefix_value = self._endpoints_config["prefixes"][prefix_name]

                    writer.register_prefix(name=prefix_name, value=prefix_value)

            writer.write_header()
            self._write_initial(writer, entryIRI, spec_IRI)
            self._write_scancoordinate(writer, calc_id, data)
            self._write_scanpoints(writer, entryIRI, calc_id, data)

    def _write_initial(self, writer: Abox_Writer, entryIRI, spec_IRI):

        onto_pes = self._endpoints_config["prefixes"]["onto_pes"]
        pes_pref = self._endpoints_config["prefixes"]["pes_pref"]

        abox_name = "ABoxOntoPESSscan"
        writer.write_imports(name=abox_name, importing=onto_pes)
        writer.write_imports(name=abox_name, importing=pes_pref[:-1], rel="base")
        writer.write_inst(
            iri=f"pes_pref:{entryIRI}",
            type="onto_pes:#PotentialEnergySurfaceScan",
        )
        writer.write_inst(
            iri=f"spec_pref:{spec_IRI[0]}",
            type="onto_spec:#Species",
        )
        writer.write_obj_prop(
            src_iri=f"pes_pref:{entryIRI}",
            rel="onto_pes:#onSpecies",
            trg_iri=f"spec_pref:{spec_IRI[0]}",
        )

    def _write_scancoordinate(self, writer: Abox_Writer, calc_id, data):

        scan_type = data[SCAN_COORDINATE_TYPE]

        writer.write_inst(
            iri=f"pes_pref:{scan_type}_{calc_id}",
            type=f"onto_pes:#{scan_type}",
        ).add_obj_prop(
            iri=f"pes_pref:{data['EntryIRI']}",
            rel="onto_pes:#hasScanCoordinate",
        )
        for atomiri in data[SCAN_COORDINATE_ATOMS_IRIS]:

            writer.write_inst(
                iri=f"spec_pref:{atomiri}",
                type="gain_pref:Atom",
            ).add_obj_prop(
                iri=f"pes_pref:{scan_type}_{calc_id}",
                rel="onto_pes:#hasScanAtom",
            )

    def _write_scanpoints(self, writer: Abox_Writer, entryIRI, calc_id, data):

        for k in range(len(data[SCAN_COORDINATE_VALUE])):
            gauss_type = data[SCAN_POINTS_JOBS][k].split("_")[0][-3:]

            writer.write_inst(
                iri=f"pes_pref:ScanPoint_{calc_id}_{k + 1}",
                type="onto_pes:#ScanPoint",
            ).add_obj_prop(
                iri=f"pes_pref:{entryIRI}",
                rel="onto_pes:#hasScanPoint",
            )
            writer.write_inst(
                iri=data[SCAN_POINTS_JOBS][k],
                type=f"onto_comp:#{gauss_type}",
            ).add_obj_prop(
                iri=f"pes_pref:ScanPoint_{calc_id}_{k + 1}",
                rel="onto_pes:#hasCalculation",
                store_inst=True,
            ).add_data_prop(
                rel="onto_pes:#hasInputAtomIds",
                value=data[SCAN_ATOM_IDS],
            )

            writer.write_inst(
                iri=f"pes_pref:ScanCoordinateValue_{calc_id}_{k + 1}",
                type="onto_pes:#ScanCoordinateValue",
            ).add_obj_prop(
                iri=f"pes_pref:ScanPoint_{calc_id}_{k + 1}",
                rel="onto_pes:#hasScanCoordinateValue",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data[SCAN_COORDINATE_VALUE][k],
            )

            scan_unit = ""
            if data[SCAN_COORDINATE_UNIT] == "Angstrom":
                scan_unit = "unit#Angstrom"
            elif data[SCAN_COORDINATE_UNIT] == "Degree":
                scan_unit = "unit#DegreeAngle"
            if scan_unit:
                writer.write_obj_prop(
                    src_iri=f"pes_pref:ScanCoordinateValue_{calc_id}_{k + 1}",
                    trg_iri=f"unit_pref:{scan_unit}",
                    rel="gain_pref:hasUnit",
                )
