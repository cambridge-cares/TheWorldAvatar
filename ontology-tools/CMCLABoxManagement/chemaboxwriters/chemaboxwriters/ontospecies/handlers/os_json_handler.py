# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import chemaboxwriters.common.utilsfunc as utilsfunc
from compchemparser.parsers.ccgaussian_parser import (
    ATOM_TYPES,
    FORMAL_CHARGE,
    EMP_FORMULA,
)
from chemaboxwriters.ontospecies.handlers.qc_json_handler import (
    MOLWT,
    INCHI,
    SMILES,
    GEOM_STRING,
    BOND_STRING,
    PUBCHEM_ALT_LABEL,
    PUBCHEM_CID,
    CAS_NUMBER,
    ATOM_LIST,
    ATOM_COUNTS,
    SPIN_MULT,
    ENTH_FORM,
    ENTH_UNIT,
    ENTH_PHASE,
    ENTH_REFTEMP,
    ENTH_REFTEMP_UNIT,
    ENTH_PROV,
)

import chemaboxwriters.common.params as params
from typing import List, Optional, Dict
from chemaboxwriters.common.handler import Handler
from chemaboxwriters.ontospecies.abox_stages import OS_ABOX_STAGES


Abox_Writer = utilsfunc.Abox_csv_writer

HANDLER_PREFIXES = {
    "onto_spec": {"required": True},
    "gain_pref": {"required": True},
    "onto_kin": {"required": True},
    "table_pref": {"required": True},
    "unit_pref": {"required": True},
    "spec_pref": {"required": True},
}


class OS_JSON_TO_OS_CSV_Handler(Handler):
    """Handler converting os_json files to os_csv.
    Inputs: List of os_json file paths
    Outputs: List of os_csv file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="OS_JSON_TO_OS_CSV",
            in_stage=OS_ABOX_STAGES.os_json,  # type: ignore
            out_stage=OS_ABOX_STAGES.os_csv,  # type: ignore
            prefixes=HANDLER_PREFIXES,
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
        triple_store_uploads: Optional[Dict] = None,
        file_server_uploads: Optional[Dict] = None,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage,
                out_dir=out_dir,
            )
            self._os_csvwriter(file_path=json_file_path, output_file_path=out_file_path)
            outputs.append(out_file_path)
        return outputs

    def _os_csvwriter(self, file_path: str, output_file_path: str):

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[params.ENTRY_UUID]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            for prefix_name in self._handler_prefixes._parameters:
                prefix_value = self.get_prefix_value(name=prefix_name)
                if prefix_value is not None:
                    writer.register_prefix(name=prefix_name, value=prefix_value)

            out_id = data[params.ENTRY_IRI]
            label = utilsfunc.formula_clean_re.sub("", data[EMP_FORMULA])

            self._write_prelim(writer, out_id, label)
            self._write_identifier_geom(writer, out_id, data)
            self._write_atom_info(writer, gen_id, out_id, data)
            self._write_charge_info(writer, gen_id, out_id, data)
            self._write_atoms(writer, gen_id, out_id, data)
            self._write_molwts(writer, gen_id, out_id, data)
            self._write_enth(writer, gen_id, out_id, data)

    def _write_prelim(self, writer: Abox_Writer, out_id, label):
        writer.write_header()
        writer.write_imports(
            name="ABoxOntoSpecies", importing="onto_spec:"
        ).add_imports(importing="spec_pref_no_slash:", rel="base")
        writer.write_inst(iri=out_id, type="Species").add_data_prop(
            rel="http://purl.org/dc/elements/1.1/identifier", value=out_id
        ).add_data_prop(rel="http://www.w3.org/2000/01/rdf-schema#label", value=label)

    def _write_identifier_geom(self, writer: Abox_Writer, out_id, data):

        pubchem_alt_label_value = data.get(PUBCHEM_ALT_LABEL)
        if pubchem_alt_label_value is not None:
            writer.write_data_prop(
                iri=out_id,
                rel="http://www.w3.org/2004/02/skos/core#altLabel",
                value=pubchem_alt_label_value,
            )
        cas_number_value = data.get(CAS_NUMBER)
        if cas_number_value is not None:
            writer.write_data_prop(
                iri=out_id,
                rel="onto_spec:#casRegistryID",
                value=cas_number_value,
            )
        writer.write_data_prop(
            iri=out_id,
            rel="onto_spec:#SMILES",
            value=data[SMILES],
        )
        writer.write_data_prop(
            iri=out_id,
            rel="onto_spec:#inChI",
            value=data[INCHI],
        )
        pubchem_cid_value = data.get(PUBCHEM_CID)
        if pubchem_cid_value is not None:
            writer.write_data_prop(
                iri=out_id,
                rel="onto_spec:#pubChemCID",
                value=pubchem_cid_value,
            )
        writer.write_data_prop(
            iri=out_id,
            rel="onto_spec:#hasAtomicBond",
            value=data[BOND_STRING],
        )
        writer.write_data_prop(
            iri=out_id,
            rel="onto_spec:#hasGeometry",
            value=data[GEOM_STRING],
        )
        writer.write_data_prop(
            iri=out_id,
            rel="onto_spec:#spinMultiplicity",
            value=data[SPIN_MULT],
        )

    def _write_atom_info(self, writer: Abox_Writer, gen_id, out_id, data):

        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        atom_counters = {atom_type: 1 for atom_type in set(data[ATOM_TYPES])}
        for k, atom_type in enumerate(data[ATOM_TYPES]):

            atom_nr = atom_counters[atom_type]
            atom_id = f"{gen_id}_{atom_type}_{atom_nr}"
            # Now the atoms are written here
            writer.write_inst(
                iri=f"Atom_{atom_id}",
                type="gain_pref:Atom",
            ).add_obj_prop(iri=out_id, rel="gain_pref:hasAtom",).add_obj_prop(
                iri=f"table_pref:#{atom_type}",
                rel="gain_pref:isElement",
                reverse=True,
            )
            for i in range(3):  # Write the atom coordinates.
                writer.write_inst(
                    iri=f"AtomCoordinate{coords[i]}_{atom_id}",
                    type="gain_pref:FloatValue",
                ).add_obj_prop(
                    iri=f"Atom_{atom_id}",
                    rel=f"gain_pref:hasAtomCoordinate{coords[i]}",
                ).add_data_prop(
                    rel="gain_pref:hasValue",
                    value=data["Geometry"][k][i],
                ).add_obj_prop(
                    iri="unit_pref:unit#Angstrom",
                    rel="gain_pref:hasUnit",
                    reverse=True,
                )
            atom_counters[atom_type] += 1

    def _write_charge_info(self, writer: Abox_Writer, gen_id, out_id, data):

        if FORMAL_CHARGE in data:
            charge = data[FORMAL_CHARGE]

            writer.write_inst(
                iri=f"Charge_{gen_id}", type="onto_spec:#Charge"
            ).add_obj_prop(iri=out_id, rel="onto_spec:#hasCharge",).add_data_prop(
                rel="onto_spec:#value",
                value=charge,
            ).add_data_prop(
                rel="onto_spec:#units",
                value="e",
            )
            writer.write_inst(
                iri=f"MolecularFormula_{gen_id}",
                type="onto_spec:#MolecularFormula",
            ).add_obj_prop(
                iri=out_id,
                rel="onto_spec:#hasMolecularFormula",
            )

    def _write_atoms(self, writer: Abox_Writer, gen_id, out_id, data):

        atom_list = data[ATOM_LIST]
        atom_counts = data[ATOM_COUNTS]
        for i in range(len(atom_list)):
            writer.write_inst(
                iri=f"Element_{atom_list[i]}", type="onto_kin:#Element"
            ).add_obj_prop(
                iri=f"MolecularFormula_{gen_id}",
                rel="onto_kin:#hasElement",
            )
            writer.write_inst(
                iri=f"ElementNumber_{gen_id}_{i + 1}",
                type="onto_kin:#ElementNumber",
            ).add_obj_prop(
                iri=f"MolecularFormula_{gen_id}",
                rel="onto_kin:#hasElementNumber",
            ).add_data_prop(
                rel="onto_kin:#hasNumberOfElement",
                value=atom_counts[i],
                data_type="Integer",
            ).add_obj_prop(
                iri=f"Element_{atom_list[i]}",
                rel="onto_kin:#indicatesNumberOf",
                reverse=True,
            )
        writer.write_inst(iri=out_id, type="onto_spec:#Species")

    def _write_molwts(self, writer: Abox_Writer, gen_id, out_id, data):

        if MOLWT in data:
            molwt = data[MOLWT]
            writer.write_inst(
                iri=f"MolecularWeight_{gen_id}",
                type="onto_spec:#MolecularWeight",
            ).add_obj_prop(
                iri=out_id,
                rel="onto_spec:#hasMolecularWeight",
            ).add_data_prop(
                rel="onto_spec:#value",
                value=molwt,
            ).add_data_prop(
                rel="onto_spec:#units",
                value="g/mol",
            )

    def _write_enth(self, writer: Abox_Writer, gen_id, out_id, data):

        # Write enthalpy of formation data.
        if ENTH_FORM in data:
            writer.write_inst(
                iri=f"StandardEnthalpyOfFormation_{gen_id}",
                type="onto_spec:#StandardEnthalpyOfFormation",
            ).add_obj_prop(
                iri=out_id,
                rel="onto_spec:#hasStandardEnthalpyOfFormation",
            ).add_data_prop(
                rel="onto_spec:#value",
                value=data[ENTH_FORM],
            )
        if ENTH_UNIT in data:
            writer.write_data_prop(
                iri=f"StandardEnthalpyOfFormation_{gen_id}",
                rel="onto_spec:#units",
                value=data[ENTH_UNIT],
            )
        if ENTH_REFTEMP in data:
            writer.write_inst(
                iri=f"Temperature_{gen_id}", type="onto_spec:#Temperature"
            ).add_obj_prop(
                iri=f"StandardEnthalpyOfFormation_{gen_id}",
                rel="onto_spec:#hasReferenceTemperature",
            ).add_data_prop(
                rel="onto_spec:#value",
                value=data[ENTH_REFTEMP],
            )
        if ENTH_REFTEMP_UNIT in data:
            writer.write_data_prop(
                iri=f"Temperature_{gen_id}",
                rel="onto_spec:#units",
                value=data[ENTH_REFTEMP_UNIT],
            )
        if ENTH_PHASE in data:
            writer.write_inst(
                iri=f"{data[ENTH_PHASE]}Phase_{gen_id}",
                type=f"onto_kin:#{data[ENTH_PHASE]}Phase",
            ).add_obj_prop(
                iri=f"StandardEnthalpyOfFormation_{gen_id}",
                rel="onto_spec:#hasPhase",
            )
        if ENTH_PROV in data:
            writer.write_inst(
                iri=f"Reference_{gen_id}", type="onto_kin:#Reference"
            ).add_obj_prop(
                iri=f"StandardEnthalpyOfFormation_{gen_id}",
                rel="onto_spec:#hasProvenance",
            ).add_data_prop(
                rel="http://www.w3.org/2000/01/rdf-schema#label",
                value=data[ENTH_PROV],
            )
