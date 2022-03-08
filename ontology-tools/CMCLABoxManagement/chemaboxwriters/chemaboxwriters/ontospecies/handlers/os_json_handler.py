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

import chemaboxwriters.common.globals as globals
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from typing import List, Optional, Dict
from enum import Enum
from chemaboxwriters.common.handler import Handler

Abox_Writer = utilsfunc.Abox_csv_writer


class OS_JSON_TO_OS_CSV_Handler(Handler):
    """Handler converting os_json files to os_csv.
    Inputs: List of os_json file paths
    Outputs: List of os_csv file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="OS_JSON_TO_OS_CSV",
            in_stage=globals.aboxStages.OS_JSON,
            out_stage=globals.aboxStages.OS_CSV,
            endpoints_proxy=endpoints_proxy,
            required_configs={
                abconf.WRITERS_PREFIXES_KEY: [
                    "onto_spec",
                    "gain_pref",
                    "onto_kin",
                    "table_pref",
                    "unit_pref",
                    "spec_pref",
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
            self._os_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **self._handler_kwargs,
            )
            outputs.append(out_file_path)
        return outputs

    def _os_csvwriter(self, file_path: str, output_file_path: str, *args, **kwargs):

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[globals.ENTRY_UUID]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as aboxwriter:
            aboxwriter.write_header()

            out_id = data[globals.ENTRY_IRI]

            label = utilsfunc.formula_clean_re.sub("", data[EMP_FORMULA])

            self._write_prelim(aboxwriter, out_id, label)
            self._write_identifier_geom(aboxwriter, out_id, data)
            self._write_atom_info(aboxwriter, gen_id, out_id, data)
            self._write_charge_info(aboxwriter, gen_id, out_id, data)
            self._write_atoms(aboxwriter, gen_id, out_id, data)
            self._write_molwts(aboxwriter, gen_id, out_id, data)
            self._write_enth(aboxwriter, gen_id, out_id, data)

    def _write_prelim(self, aboxwriter: Abox_Writer, out_id, label):

        spec_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["spec_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        aboxwriter.write_imports(
            abox_name="ABoxOntoSpecies",
            importing=onto_spec,
            relation="http://www.w3.org/2002/07/owl#imports",
        )
        aboxwriter.write_imports(
            abox_name="ABoxOntoSpecies", importing=spec_pref[:-1], relation="base"
        )
        aboxwriter.write_instance(inst_iri=out_id, inst_class="Species")
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation="http://purl.org/dc/elements/1.1/identifier",
            value=out_id,
            data_type="String",
        )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation="http://www.w3.org/2000/01/rdf-schema#label",
            value=label,
            data_type="String",
        )

    def _write_identifier_geom(self, aboxwriter: Abox_Writer, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        pubchem_alt_label_value = data.get(PUBCHEM_ALT_LABEL)
        if pubchem_alt_label_value is not None:
            aboxwriter.write_data_property(
                inst_iri=out_id,
                relation="http://www.w3.org/2004/02/skos/core#altLabel",
                value=pubchem_alt_label_value,
                data_type="String",
            )
        cas_number_value = data.get(CAS_NUMBER)
        if cas_number_value is not None:
            aboxwriter.write_data_property(
                inst_iri=out_id,
                relation=f"{onto_spec}#casRegistryID",
                value=cas_number_value,
                data_type="String",
            )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation=f"{onto_spec}#SMILES",
            value=data[SMILES],
            data_type="String",
        )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation=f"{onto_spec}#inChI",
            value=data[INCHI],
            data_type="String",
        )
        pubchem_cid_value = data.get(PUBCHEM_CID)
        if pubchem_cid_value is not None:
            aboxwriter.write_data_property(
                inst_iri=out_id,
                relation=f"{onto_spec}#pubChemCID",
                value=pubchem_cid_value,
                data_type="String",
            )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation=f"{onto_spec}#hasAtomicBond",
            value=data[BOND_STRING],
            data_type="String",
        )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation=f"{onto_spec}#hasGeometry",
            value=data[GEOM_STRING],
            data_type="String",
        )
        aboxwriter.write_data_property(
            inst_iri=out_id,
            relation=f"{onto_spec}#spinMultiplicity",
            value=data[SPIN_MULT],
            data_type="String",
        )

    def _write_atom_info(self, aboxwriter: Abox_Writer, gen_id, out_id, data):

        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        table_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["table_pref"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        atom_counters = {atom_type: 1 for atom_type in set(data[ATOM_TYPES])}
        for k, atom_type in enumerate(data[ATOM_TYPES]):

            atom_nr = atom_counters[atom_type]
            atom_id = f"{gen_id}_{atom_type}_{atom_nr}"
            # Now the atoms are written here
            aboxwriter.write_instance(
                inst_iri=f"Atom_{atom_id}",
                inst_class=f"{gain_pref}Atom",
            )
            aboxwriter.write_object_property(
                src_inst_iri=out_id,
                trg_inst_iri=f"Atom_{atom_id}",
                relation=f"{gain_pref}hasAtom",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"Atom_{atom_id}",
                trg_inst_iri=f"{table_pref}#{atom_type}",
                relation=f"{gain_pref}isElement",
            )
            for i in range(3):  # Write the atom coordinates.
                aboxwriter.write_instance(
                    inst_iri=f"AtomCoordinate{coords[i]}_{atom_id}",
                    inst_class=f"{gain_pref}FloatValue",
                )
                aboxwriter.write_object_property(
                    src_inst_iri=f"Atom_{atom_id}",
                    trg_inst_iri=f"AtomCoordinate{coords[i]}_{atom_id}",
                    relation=f"{gain_pref}hasAtomCoordinate{coords[i]}",
                )
                aboxwriter.write_data_property(
                    inst_iri=f"AtomCoordinate{coords[i]}_{atom_id}",
                    relation=f"{gain_pref}hasValue",
                    value=data["Geometry"][k][i],
                    data_type="String",
                )
                aboxwriter.write_object_property(
                    src_inst_iri=f"AtomCoordinate{coords[i]}_{atom_id}",
                    trg_inst_iri=f"{unit_pref}unit#Angstrom",
                    relation=f"{gain_pref}hasUnit",
                )
            atom_counters[atom_type] += 1

    def _write_charge_info(self, aboxwriter: Abox_Writer, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        if FORMAL_CHARGE in data:
            charge = data[FORMAL_CHARGE]

            aboxwriter.write_instance(
                inst_iri=f"Charge_{gen_id}", inst_class=f"{onto_spec}#Charge"
            )
            aboxwriter.write_object_property(
                src_inst_iri=out_id,
                trg_inst_iri=f"Charge_{gen_id}",
                relation=f"{onto_spec}#hasCharge",
            )
            aboxwriter.write_data_property(
                inst_iri=f"Charge_{gen_id}",
                relation=f"{onto_spec}#value",
                value=charge,
                data_type="String",
            )
            aboxwriter.write_data_property(
                inst_iri=f"Charge_{gen_id}",
                relation=f"{onto_spec}#units",
                value="e",
                data_type="String",
            )
            aboxwriter.write_instance(
                inst_iri=f"MolecularFormula_{gen_id}",
                inst_class=f"{onto_spec}#MolecularFormula",
            )
            aboxwriter.write_object_property(
                src_inst_iri=out_id,
                trg_inst_iri=f"MolecularFormula_{gen_id}",
                relation=f"{onto_spec}#hasMolecularFormula",
            )

    def _write_atoms(self, aboxwriter: Abox_Writer, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        onto_kin = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_kin"]

        atom_list = data[ATOM_LIST]
        atom_counts = data[ATOM_COUNTS]
        for i in range(len(atom_list)):
            aboxwriter.write_instance(
                inst_iri=f"Element_{atom_list[i]}", inst_class=f"{onto_kin}#Element"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"MolecularFormula_{gen_id}",
                trg_inst_iri=f"Element_{atom_list[i]}",
                relation=f"{onto_kin}#hasElement",
            )
            aboxwriter.write_instance(
                inst_iri=f"ElementNumber_{gen_id}_{i + 1}",
                inst_class=f"{onto_kin}#ElementNumber",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"MolecularFormula_{gen_id}",
                trg_inst_iri=f"ElementNumber_{gen_id}_{i + 1}",
                relation=f"{onto_kin}#hasElementNumber",
            )
            aboxwriter.write_data_property(
                inst_iri=f"ElementNumber_{gen_id}_{i + 1}",
                relation=f"{onto_kin}#hasNumberOfElement",
                value=atom_counts[i],
                data_type="Integer",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"ElementNumber_{gen_id}_{i + 1}",
                trg_inst_iri=f"Element_{atom_list[i]}",
                relation=f"{onto_kin}#indicatesNumberOf",
            )
        aboxwriter.write_instance(inst_iri=out_id, inst_class=f"{onto_spec}#Species")

    def _write_molwts(self, aboxwriter: Abox_Writer, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        if MOLWT in data:
            molwt = data[MOLWT]
            aboxwriter.write_instance(
                inst_iri=f"MolecularWeight_{gen_id}",
                inst_class=f"{onto_spec}#MolecularWeight",
            )
            aboxwriter.write_object_property(
                src_inst_iri=out_id,
                trg_inst_iri=f"MolecularWeight_{gen_id}",
                relation=f"{onto_spec}#hasMolecularWeight",
            )
            aboxwriter.write_data_property(
                inst_iri=f"MolecularWeight_{gen_id}",
                relation=f"{onto_spec}#value",
                value=molwt,
                data_type="String",
            )
            aboxwriter.write_data_property(
                inst_iri=f"MolecularWeight_{gen_id}",
                relation=f"{onto_spec}#units",
                value="g/mol",
                data_type="String",
            )

    def _write_enth(self, aboxwriter: Abox_Writer, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        onto_kin = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_kin"]

        # Write enthalpy of formation data.
        if ENTH_FORM in data:
            aboxwriter.write_instance(
                inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                inst_class=f"{onto_spec}#StandardEnthalpyOfFormation",
            )
            aboxwriter.write_object_property(
                src_inst_iri=out_id,
                trg_inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                relation=f"{onto_spec}#hasStandardEnthalpyOfFormation",
            )
            aboxwriter.write_data_property(
                inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                relation=f"{onto_spec}#value",
                value=data[ENTH_FORM],
                data_type="String",
            )
        if ENTH_UNIT in data:
            aboxwriter.write_data_property(
                inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                relation=f"{onto_spec}#units",
                value=data[ENTH_UNIT],
                data_type="String",
            )
        if ENTH_REFTEMP in data:
            aboxwriter.write_instance(
                inst_iri=f"Temperature_{gen_id}", inst_class=f"{onto_spec}#Temperature"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                trg_inst_iri=f"Temperature_{gen_id}",
                relation=f"{onto_spec}#hasReferenceTemperature",
            )
            aboxwriter.write_data_property(
                inst_iri=f"Temperature_{gen_id}",
                relation=f"{onto_spec}#value",
                value=data[ENTH_REFTEMP],
                data_type="String",
            )
        if ENTH_REFTEMP_UNIT in data:
            aboxwriter.write_data_property(
                inst_iri=f"Temperature_{gen_id}",
                relation=f"{onto_spec}#units",
                value=data[ENTH_REFTEMP_UNIT],
                data_type="String",
            )
        if ENTH_PHASE in data:
            aboxwriter.write_instance(
                inst_iri=f"{data[ENTH_PHASE]}Phase_{gen_id}",
                inst_class=f"{onto_kin}#{data[ENTH_PHASE]}Phase",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                trg_inst_iri=f"{data[ENTH_PHASE]}Phase_{gen_id}",
                relation=f"{onto_spec}#hasPhase",
            )
        if ENTH_PROV in data:
            aboxwriter.write_instance(
                inst_iri=f"Reference_{gen_id}", inst_class=f"{onto_kin}#Reference"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"StandardEnthalpyOfFormation_{gen_id}",
                trg_inst_iri=f"Reference_{gen_id}",
                relation=f"{onto_spec}#hasProvenance",
            )
            aboxwriter.write_data_property(
                inst_iri=f"Reference_{gen_id}",
                relation="http://www.w3.org/2000/01/rdf-schema#label",
                value=data[ENTH_PROV],
                data_type="String",
            )
