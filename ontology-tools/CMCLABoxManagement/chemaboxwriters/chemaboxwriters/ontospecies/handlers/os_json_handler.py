# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import csv
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
from chemaboxwriters.common import PREFIXES
from dataclasses import dataclass, field
from typing import List
from enum import Enum
from chemaboxwriters.common.handler import IHandler

onto_spec = PREFIXES["onto_spec"]
gain_pref = PREFIXES["gain_pref"]
kin_pref = PREFIXES["kin_pref"]
table_pref = PREFIXES["table_pref"]
unit_pref = PREFIXES["unit_pref"]
spec_pref = PREFIXES["spec_pref"]


@dataclass
class OS_JSON_TO_OS_CSV_Handler(IHandler):
    """Handler converting os json files to os csv.
    Inputs: List of os json file paths
    Outputs: List of os csv file paths
    """

    name: str = field(default="OS_JSON_TO_OS_CSV")
    in_stages: List[Enum] = field(default_factory=lambda: [globals.aboxStages.OS_JSON])
    out_stage: Enum = field(default=globals.aboxStages.OS_CSV)

    def _handle_input(
        self, inputs: List[str], out_dir: str, **handler_kwargs
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self.out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._os_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    def _os_csvwriter(
        self,
        file_path: str,
        output_file_path: str,
        spec_pref: str = PREFIXES["spec_pref"],
        *args,
        **kwargs
    ):

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[globals.ENTRY_UUID]

        with open(output_file_path, "w", newline="") as csvfile:

            spamwriter = csv.writer(
                csvfile, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
            )

            out_id = data[globals.ENTRY_IRI]

            label = utilsfunc.formula_clean_re.sub("", data[EMP_FORMULA])

            spamwriter = csv.writer(
                csvfile, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
            )
            spamwriter.writerow(
                ["Source", "Type", "Target", "Relation", "Value", "Data Type"]
            )

            self._write_prelim(spamwriter, out_id, spec_pref, label)
            self._write_identifier_geom(spamwriter, out_id, data)
            self._write_atom_info(spamwriter, gen_id, out_id, data)
            self._write_charge_info(spamwriter, gen_id, out_id, data)
            self._write_atoms(spamwriter, gen_id, out_id, data)
            self._write_molwts(spamwriter, gen_id, out_id, data)
            self._write_enth(spamwriter, gen_id, out_id, data)

    @staticmethod
    def _write_prelim(spamwriter, out_id, spec_pref, label):
        spamwriter.writerow(
            [
                "ABoxOntoSpecies",
                "Ontology",
                onto_spec,
                "http://www.w3.org/2002/07/owl#imports",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            ["ABoxOntoSpecies", "Ontology", spec_pref[:-1], "base", "", ""]
        )
        spamwriter.writerow([out_id, "Instance", "Species", "", "", ""])
        spamwriter.writerow(
            [
                "http://purl.org/dc/elements/1.1/identifier",
                "Data Property",
                out_id,
                "",
                out_id,
                "String",
            ]
        )
        spamwriter.writerow(
            [
                "http://www.w3.org/2000/01/rdf-schema#label",
                "Data Property",
                out_id,
                "",
                label,
                "String",
            ]
        )

    @staticmethod
    def _write_identifier_geom(spamwriter, out_id, data):
        pubchem_alt_label_value = data.get(PUBCHEM_ALT_LABEL)
        if pubchem_alt_label_value is not None:
            spamwriter.writerow(
                [
                    "http://www.w3.org/2004/02/skos/core#altLabel",
                    "Data Property",
                    out_id,
                    "",
                    pubchem_alt_label_value,
                    "String",
                ]
            )
        cas_number_value = data.get(CAS_NUMBER)
        if cas_number_value is not None:
            spamwriter.writerow(
                [
                    onto_spec + "#casRegistryID",
                    "Data Property",
                    out_id,
                    "",
                    cas_number_value,
                    "String",
                ]
            )
        spamwriter.writerow(
            [onto_spec + "#SMILES", "Data Property", out_id, "", data[SMILES], "String"]
        )
        spamwriter.writerow(
            [onto_spec + "#inChI", "Data Property", out_id, "", data[INCHI], "String"]
        )
        pubchem_cid_value = data.get(PUBCHEM_CID)
        if pubchem_cid_value is not None:
            spamwriter.writerow(
                [
                    onto_spec + "#pubChemCID",
                    "Data Property",
                    out_id,
                    "",
                    pubchem_cid_value,
                    "String",
                ]
            )
        spamwriter.writerow(
            [
                onto_spec + "#hasAtomicBond",
                "Data Property",
                out_id,
                "",
                data[BOND_STRING],
                "String",
            ]
        )
        spamwriter.writerow(
            [
                onto_spec + "#hasGeometry",
                "Data Property",
                out_id,
                "",
                data[GEOM_STRING],
                "String",
            ]
        )
        spamwriter.writerow(
            [
                onto_spec + "#spinMultiplicity",
                "Data Property",
                out_id,
                "",
                data[SPIN_MULT],
                "String",
            ]
        )

    @staticmethod
    def _write_atom_info(spamwriter, gen_id, out_id, data):
        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        atom_counters = {atom_type: 1 for atom_type in set(data[ATOM_TYPES])}
        for k, atom_type in enumerate(data[ATOM_TYPES]):

            atom_nr = atom_counters[atom_type]
            # Now the atoms are written here
            spamwriter.writerow(
                [
                    "Atom_" + gen_id + "_" + atom_type + "_" + str(atom_nr),
                    "Instance",
                    gain_pref + "Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    "Atom_" + gen_id + "_" + atom_type + "_" + str(atom_nr),
                    gain_pref + "hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "Atom_" + gen_id + "_" + atom_type + "_" + str(atom_nr),
                    "Instance",
                    table_pref + "#" + atom_type,
                    gain_pref + "isElement",
                    "",
                    "",
                ]
            )
            for i in range(3):  # Write the atom coordinates.
                spamwriter.writerow(
                    [
                        "AtomCoordinate"
                        + coords[i]
                        + "_"
                        + gen_id
                        + "_"
                        + atom_type
                        + "_"
                        + str(atom_nr),
                        "Instance",
                        gain_pref + "FloatValue",
                        "",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        "Atom_" + gen_id + "_" + atom_type + "_" + str(atom_nr),
                        "Instance",
                        "AtomCoordinate"
                        + coords[i]
                        + "_"
                        + gen_id
                        + "_"
                        + atom_type
                        + "_"
                        + str(atom_nr),
                        gain_pref + "hasAtomCoordinate" + coords[i],
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        gain_pref + "hasValue",
                        "Data Property",
                        "AtomCoordinate"
                        + coords[i]
                        + "_"
                        + gen_id
                        + "_"
                        + atom_type
                        + "_"
                        + str(atom_nr),
                        "",
                        data["Geometry"][k][i],
                        "String",
                    ]
                )
                spamwriter.writerow(
                    [
                        "AtomCoordinate"
                        + coords[i]
                        + "_"
                        + gen_id
                        + "_"
                        + atom_type
                        + "_"
                        + str(atom_nr),
                        "Instance",
                        unit_pref + "unit#Angstrom",
                        gain_pref + "hasUnit",
                        "",
                        "",
                    ]
                )
            atom_counters[atom_type] += 1

    @staticmethod
    def _write_charge_info(spamwriter, gen_id, out_id, data):
        if FORMAL_CHARGE in data:
            charge = data[FORMAL_CHARGE]

            spamwriter.writerow(
                ["Charge_" + gen_id, "Instance", onto_spec + "#Charge", "", "", ""]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    "Charge_" + gen_id,
                    onto_spec + "#hasCharge",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#value",
                    "Data Property",
                    "Charge_" + gen_id,
                    "",
                    charge,
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#units",
                    "Data Property",
                    "Charge_" + gen_id,
                    "",
                    "e",
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    "MolecularFormula_" + gen_id,
                    "Instance",
                    onto_spec + "#MolecularFormula",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    "MolecularFormula_" + gen_id,
                    onto_spec + "#hasMolecularFormula",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_atoms(spamwriter, gen_id, out_id, data):
        atom_list = data[ATOM_LIST]
        atom_counts = data[ATOM_COUNTS]
        for i in range(len(atom_list)):
            spamwriter.writerow(
                [
                    "Element_" + atom_list[i],
                    "Instance",
                    kin_pref + "#Element",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "MolecularFormula_" + gen_id,
                    "Instance",
                    "Element_" + atom_list[i],
                    kin_pref + "#hasElement",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "ElementNumber_" + gen_id + "_" + str(i + 1),
                    "Instance",
                    kin_pref + "#ElementNumber",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "MolecularFormula_" + gen_id,
                    "Instance",
                    "ElementNumber_" + gen_id + "_" + str(i + 1),
                    kin_pref + "#hasElementNumber",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    kin_pref + "#hasNumberOfElement",
                    "Data Property",
                    "ElementNumber_" + gen_id + "_" + str(i + 1),
                    "",
                    atom_counts[i],
                    "Integer",
                ]
            )
            spamwriter.writerow(
                [
                    "ElementNumber_" + gen_id + "_" + str(i + 1),
                    "Instance",
                    "Element_" + atom_list[i],
                    kin_pref + "#indicatesNumberOf",
                    "",
                    "",
                ]
            )
        spamwriter.writerow([out_id, "Instance", onto_spec + "#Species", "", "", ""])

    @staticmethod
    def _write_molwts(spamwriter, gen_id, out_id, data):
        if MOLWT in data:
            molwt = data[MOLWT]
            spamwriter.writerow(
                [
                    "MolecularWeight_" + gen_id,
                    "Instance",
                    onto_spec + "#MolecularWeight",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    "MolecularWeight_" + gen_id,
                    onto_spec + "#hasMolecularWeight",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#value",
                    "Data Property",
                    "MolecularWeight_" + gen_id,
                    "",
                    molwt,
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#units",
                    "Data Property",
                    "MolecularWeight_" + gen_id,
                    "",
                    "g/mol",
                    "String",
                ]
            )

    @staticmethod
    def _write_enth(spamwriter, gen_id, out_id, data):
        # Write enthalpy of formation data.
        if ENTH_FORM in data:
            spamwriter.writerow(
                [
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "Instance",
                    onto_spec + "#StandardEnthalpyOfFormation",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    "StandardEnthalpyOfFormation_" + gen_id,
                    onto_spec + "#hasStandardEnthalpyOfFormation",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#value",
                    "Data Property",
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "",
                    data[ENTH_FORM],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#units",
                    "Data Property",
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "",
                    data[ENTH_UNIT],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    "Temperature_" + gen_id,
                    "Instance",
                    onto_spec + "#Temperature",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "Instance",
                    "Temperature_" + gen_id,
                    onto_spec + "#hasReferenceTemperature",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#value",
                    "Data Property",
                    "Temperature_" + gen_id,
                    "",
                    data[ENTH_REFTEMP],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    onto_spec + "#units",
                    "Data Property",
                    "Temperature_" + gen_id,
                    "",
                    data[ENTH_REFTEMP_UNIT],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    data[ENTH_PHASE] + "Phase_" + gen_id,
                    "Instance",
                    kin_pref + "#" + data[ENTH_PHASE] + "Phase",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "Instance",
                    data[ENTH_PHASE] + "Phase_" + gen_id,
                    onto_spec + "#hasPhase",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                ["Reference_" + gen_id, "Instance", kin_pref + "#Reference", "", "", ""]
            )
            spamwriter.writerow(
                [
                    "StandardEnthalpyOfFormation_" + gen_id,
                    "Instance",
                    "Reference_" + gen_id,
                    onto_spec + "#hasProvenance",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "http://www.w3.org/2000/01/rdf-schema#label",
                    "Data Property",
                    "Reference_" + gen_id,
                    "",
                    data[ENTH_PROV],
                    "String",
                ]
            )
