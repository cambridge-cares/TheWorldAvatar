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
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from typing import List, Optional, Dict
from enum import Enum
from chemaboxwriters.common.handler import Handler


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
            required_endpoints_config={
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

            self._write_prelim(spamwriter, out_id, label)
            self._write_identifier_geom(spamwriter, out_id, data)
            self._write_atom_info(spamwriter, gen_id, out_id, data)
            self._write_charge_info(spamwriter, gen_id, out_id, data)
            self._write_atoms(spamwriter, gen_id, out_id, data)
            self._write_molwts(spamwriter, gen_id, out_id, data)
            self._write_enth(spamwriter, gen_id, out_id, data)

    def _write_prelim(self, spamwriter, out_id, label):

        spec_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["spec_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

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

    def _write_identifier_geom(self, spamwriter, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

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
                    f"{onto_spec}#casRegistryID",
                    "Data Property",
                    out_id,
                    "",
                    cas_number_value,
                    "String",
                ]
            )
        spamwriter.writerow(
            [f"{onto_spec}#SMILES", "Data Property", out_id, "", data[SMILES], "String"]
        )
        spamwriter.writerow(
            [f"{onto_spec}#inChI", "Data Property", out_id, "", data[INCHI], "String"]
        )
        pubchem_cid_value = data.get(PUBCHEM_CID)
        if pubchem_cid_value is not None:
            spamwriter.writerow(
                [
                    f"{onto_spec}#pubChemCID",
                    "Data Property",
                    out_id,
                    "",
                    pubchem_cid_value,
                    "String",
                ]
            )
        spamwriter.writerow(
            [
                f"{onto_spec}#hasAtomicBond",
                "Data Property",
                out_id,
                "",
                data[BOND_STRING],
                "String",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_spec}#hasGeometry",
                "Data Property",
                out_id,
                "",
                data[GEOM_STRING],
                "String",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_spec}#spinMultiplicity",
                "Data Property",
                out_id,
                "",
                data[SPIN_MULT],
                "String",
            ]
        )

    def _write_atom_info(self, spamwriter, gen_id, out_id, data):

        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        table_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["table_pref"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        atom_counters = {atom_type: 1 for atom_type in set(data[ATOM_TYPES])}
        for k, atom_type in enumerate(data[ATOM_TYPES]):

            atom_nr = atom_counters[atom_type]
            # Now the atoms are written here
            spamwriter.writerow(
                [
                    f"Atom_{gen_id}_{atom_type}_{atom_nr}",
                    "Instance",
                    f"{gain_pref}Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    f"Atom_{gen_id}_{atom_type}_{atom_nr}",
                    f"{gain_pref}hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"Atom_{gen_id}_{atom_type}_{atom_nr}",
                    "Instance",
                    f"{table_pref}#{atom_type}",
                    f"{gain_pref}isElement",
                    "",
                    "",
                ]
            )
            for i in range(3):  # Write the atom coordinates.
                spamwriter.writerow(
                    [
                        f"AtomCoordinate{coords[i]}_{gen_id}_{atom_type}_{atom_nr}",
                        "Instance",
                        f"{gain_pref}FloatValue",
                        "",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        f"Atom_{gen_id}_{atom_type}_{atom_nr}",
                        "Instance",
                        f"AtomCoordinate{coords[i]}_{gen_id}_{atom_type}_{atom_nr}",
                        f"{gain_pref}hasAtomCoordinate{coords[i]}",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        f"{gain_pref}hasValue",
                        "Data Property",
                        f"AtomCoordinate{coords[i]}_{gen_id}_{atom_type}_{atom_nr}",
                        "",
                        data["Geometry"][k][i],
                        "String",
                    ]
                )
                spamwriter.writerow(
                    [
                        f"AtomCoordinate{coords[i]}_{gen_id}_{atom_type}_{atom_nr}",
                        "Instance",
                        f"{unit_pref}unit#Angstrom",
                        f"{gain_pref}hasUnit",
                        "",
                        "",
                    ]
                )
            atom_counters[atom_type] += 1

    def _write_charge_info(self, spamwriter, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        if FORMAL_CHARGE in data:
            charge = data[FORMAL_CHARGE]

            spamwriter.writerow(
                [f"Charge_{gen_id}", "Instance", f"{onto_spec}#Charge", "", "", ""]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    f"Charge_{gen_id}",
                    f"{onto_spec}#hasCharge",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#value",
                    "Data Property",
                    f"Charge_{gen_id}",
                    "",
                    charge,
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#units",
                    "Data Property",
                    f"Charge_{gen_id}",
                    "",
                    "e",
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"MolecularFormula_{gen_id}",
                    "Instance",
                    f"{onto_spec}#MolecularFormula",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    f"MolecularFormula_{gen_id}",
                    f"{onto_spec}#hasMolecularFormula",
                    "",
                    "",
                ]
            )

    def _write_atoms(self, spamwriter, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        onto_kin = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_kin"]

        atom_list = data[ATOM_LIST]
        atom_counts = data[ATOM_COUNTS]
        for i in range(len(atom_list)):
            spamwriter.writerow(
                [
                    f"Element_{atom_list[i]}",
                    "Instance",
                    f"{onto_kin}#Element",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"MolecularFormula_{gen_id}",
                    "Instance",
                    f"Element_{atom_list[i]}",
                    f"{onto_kin}#hasElement",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"ElementNumber_{gen_id}_{i + 1}",
                    "Instance",
                    f"{onto_kin}#ElementNumber",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"MolecularFormula_{gen_id}",
                    "Instance",
                    f"ElementNumber_{gen_id}_{i + 1}",
                    f"{onto_kin}#hasElementNumber",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_kin}#hasNumberOfElement",
                    "Data Property",
                    f"ElementNumber_{gen_id}_{i + 1}",
                    "",
                    atom_counts[i],
                    "Integer",
                ]
            )
            spamwriter.writerow(
                [
                    f"ElementNumber_{gen_id}_{i + 1}",
                    "Instance",
                    f"Element_{atom_list[i]}",
                    f"{onto_kin}#indicatesNumberOf",
                    "",
                    "",
                ]
            )
        spamwriter.writerow([out_id, "Instance", f"{onto_spec}#Species", "", "", ""])

    def _write_molwts(self, spamwriter, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        if MOLWT in data:
            molwt = data[MOLWT]
            spamwriter.writerow(
                [
                    f"MolecularWeight_{gen_id}",
                    "Instance",
                    f"{onto_spec}#MolecularWeight",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    f"MolecularWeight_{gen_id}",
                    f"{onto_spec}#hasMolecularWeight",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#value",
                    "Data Property",
                    f"MolecularWeight_{gen_id}",
                    "",
                    molwt,
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#units",
                    "Data Property",
                    f"MolecularWeight_{gen_id}",
                    "",
                    "g/mol",
                    "String",
                ]
            )

    def _write_enth(self, spamwriter, gen_id, out_id, data):

        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        onto_kin = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_kin"]

        # Write enthalpy of formation data.
        if ENTH_FORM in data:
            spamwriter.writerow(
                [
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "Instance",
                    f"{onto_spec}#StandardEnthalpyOfFormation",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    out_id,
                    "Instance",
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    f"{onto_spec}#hasStandardEnthalpyOfFormation",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#value",
                    "Data Property",
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "",
                    data[ENTH_FORM],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#units",
                    "Data Property",
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "",
                    data[ENTH_UNIT],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"Temperature_{gen_id}",
                    "Instance",
                    f"{onto_spec}#Temperature",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "Instance",
                    f"Temperature_{gen_id}",
                    f"{onto_spec}#hasReferenceTemperature",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#value",
                    "Data Property",
                    f"Temperature_{gen_id}",
                    "",
                    data[ENTH_REFTEMP],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_spec}#units",
                    "Data Property",
                    f"Temperature_{gen_id}",
                    "",
                    data[ENTH_REFTEMP_UNIT],
                    "String",
                ]
            )
            spamwriter.writerow(
                [
                    f"{data[ENTH_PHASE]}Phase_{gen_id}",
                    "Instance",
                    f"{onto_kin}#{data[ENTH_PHASE]}Phase",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "Instance",
                    f"{data[ENTH_PHASE]}Phase_{gen_id}",
                    f"{onto_spec}#hasPhase",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [f"Reference_{gen_id}", "Instance", f"{onto_kin}#Reference", "", "", ""]
            )
            spamwriter.writerow(
                [
                    f"StandardEnthalpyOfFormation_{gen_id}",
                    "Instance",
                    f"Reference_{gen_id}",
                    f"{onto_spec}#hasProvenance",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    "http://www.w3.org/2000/01/rdf-schema#label",
                    "Data Property",
                    f"Reference_{gen_id}",
                    "",
                    data[ENTH_PROV],
                    "String",
                ]
            )
