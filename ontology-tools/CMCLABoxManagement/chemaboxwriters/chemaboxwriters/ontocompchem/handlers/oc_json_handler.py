import json
import csv
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common import PREFIXES
from chemaboxwriters.common.handler import IHandler
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.globals import aboxStages
import re
from typing import List
from enum import Enum
from dataclasses import dataclass, field


comp_pref = PREFIXES["comp_pref"]
data_pref = PREFIXES["data_pref"]
onto_comp = PREFIXES["onto_comp"]
inst_spec = PREFIXES["inst_spec"]
has_spec = PREFIXES["has_spec"]
gain_pref = PREFIXES["gain_pref"]
table_pref = PREFIXES["table_pref"]
unit_pref = PREFIXES["unit_pref"]


@dataclass
class OC_JSON_TO_OC_CSV_Handler(IHandler):
    """Handler converting csv files to owl.
    Inputs: List of csv file paths
    Outputs: List of owl file paths
    """

    name: str = field(default="OC_LOG_OC_CSV")
    in_stages: List[Enum] = field(default_factory=lambda: [aboxStages.OC_JSON])
    out_stage: Enum = field(default=aboxStages.OC_CSV)

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: Enum,
        dry_run: bool,
        **handler_kwargs
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self.out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._oc_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs
            )
            outputs.append(out_file_path)
        return outputs

    def _oc_csvwriter(
        self, file_path: str, output_file_path: str, *args, **kwargs
    ) -> None:

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        spec_IRI = data[globals.SPECIES_IRI]
        calc_id = data[globals.ENTRY_UUID]
        entryIRI = data[globals.ENTRY_IRI]

        with open(output_file_path, "w", newline="") as csvfile:

            spamwriter = csv.writer(
                csvfile, delimiter=",", quotechar='"', quoting=csv.QUOTE_MINIMAL
            )

            spamwriter.writerow(
                ["Source", "Type", "Target", "Relation", "Value", "Data Type"]
            )

            self._write_initial(spamwriter, entryIRI, calc_id, spec_IRI)
            self._write_mols(spamwriter, calc_id, data)
            self._write_level_of_theory(spamwriter, calc_id, data)
            self._write_name(spamwriter, calc_id, data)
            self._write_frequencies(spamwriter, entryIRI, calc_id, data)
            self._write_rotations(spamwriter, entryIRI, calc_id, data)
            self._write_geom_type(spamwriter, entryIRI, calc_id, data)
            self._write_zpe(spamwriter, entryIRI, calc_id, data)
            self._write_scf(spamwriter, entryIRI, calc_id, data)
            self._write_occ(spamwriter, entryIRI, calc_id, data)
            self._write_virt(spamwriter, entryIRI, calc_id, data)
            self._write_geom_opt(spamwriter, entryIRI, calc_id, data)
            self._write_atom_info(spamwriter, calc_id, data)
            self._write_metadata(spamwriter, calc_id, data)

    @staticmethod
    def _write_initial(spamwriter, jobIRI, calc_id, spec_IRI):
        # This is all the initialization part of the ABox
        spamwriter.writerow(
            [
                "ABoxOntoCompChem",
                "Ontology",
                onto_comp,
                "http://www.w3.org/2002/07/owl#imports",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            ["ABoxOntoCompChem", "Ontology", comp_pref[:-1], "base", "", ""]
        )
        spamwriter.writerow([jobIRI, "Instance", onto_comp + "#G09", "", "", ""])
        if (
            spec_IRI
        ):  # If you have the ontospecies IRI, it puts it here. Otherwise, it leaves it out.
            spamwriter.writerow([spec_IRI, "Instance", inst_spec, "", "", ""])
            spamwriter.writerow([jobIRI, "Instance", spec_IRI, has_spec, "", ""])
        spamwriter.writerow(
            [
                comp_pref + "InitializationModule_" + calc_id,
                "Instance",
                onto_comp + "#InitializationModule",
                "",
                "",
                "",
            ]
        )  # Sets up initialization.
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                comp_pref + "InitializationModule_" + calc_id,
                onto_comp + "#hasInitialization",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "Instance",
                gain_pref + "SourcePackage",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                onto_comp + "#hasEnvironment",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "MoleculeProperty_" + calc_id,
                "Instance",
                gain_pref + "MoleculeProperty",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "InitializationModule_" + calc_id,
                "Instance",
                comp_pref + "MoleculeProperty_" + calc_id,
                gain_pref + "hasMoleculeProperty",
                "",
                "",
            ]
        )

    @staticmethod
    def _write_mols(spamwriter, calc_id, data):
        # This section starts the representation of the molecule, namely dividing the species into sub-molecules that contain the different atom types.
        # This will hopefully be changed by an update in OntoCompChem later.
        at_count = []
        for key, value in data["Atom counts"].items():
            temp = [key, value]
            at_count.append(temp)
        for k in range(
            len(at_count)
        ):  # For each atom in the molecule, make a molecule object (This is the way it's done atm.)
            spamwriter.writerow(
                [
                    comp_pref
                    + "Molecule_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "Instance",
                    gain_pref + "Molecule",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "MoleculeProperty_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "Instance",
                    comp_pref
                    + "Molecule_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    gain_pref + "hasMolecule",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "Instance",
                    gain_pref + "Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "Molecule_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "Instance",
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    gain_pref + "hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    table_pref + "#" + at_count[k][0],
                    "Instance",
                    table_pref + "#Element",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "Instance",
                    table_pref + "#" + at_count[k][0],
                    gain_pref + "isElement",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasNumberOfAtoms",
                    "Data Property",
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + at_count[k][0]
                    + str(float(at_count[k][1])),
                    "",
                    at_count[k][1],
                    "",
                ]
            )

    @staticmethod
    def _write_level_of_theory(spamwriter, calc_id, data):
        # This section writes the information related to the level of theory for the ABox (method and basis set).
        spamwriter.writerow(
            [
                comp_pref + "LevelOfTheory_" + calc_id,
                "Instance",
                onto_comp + "#LevelOfTheory",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "MethodologyFeature_" + calc_id + "_LevelofTheoryParameter",
                "Instance",
                gain_pref + "MethodologyFeature",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "InitializationModule_" + calc_id,
                "Instance",
                comp_pref + "MethodologyFeature_" + calc_id + "_LevelofTheoryParameter",
                gain_pref + "hasParameter",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                onto_comp + "#hasLevelOfTheory",
                "Data Property",
                comp_pref + "MethodologyFeature_" + calc_id + "_LevelofTheoryParameter",
                "",
                data["Method"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "BasisSet_" + calc_id,
                "Instance",
                gain_pref + "BasisSet",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "InitializationModule_" + calc_id,
                "Instance",
                comp_pref + "BasisSet_" + calc_id,
                gain_pref + "hasParameter",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                gain_pref + "hasBasisSet",
                "Data Property",
                comp_pref + "BasisSet_" + calc_id,
                "",
                '"{}"'.format(data["Basis set"]),
                "",
            ]
        )  # Note that the string formatting is used to escape the ',' in basis sets.

    @staticmethod
    def _write_name(spamwriter, calc_id, data):
        # This writes the name of the species, taken as the formula, but with extraneous 1s removed.
        spamwriter.writerow(
            [
                gain_pref + "hasName",
                "Data Property",
                comp_pref + "MoleculeProperty_" + calc_id,
                "",
                utilsfunc.formula_clean_re.sub("", data["Empirical formula"]),
                "",
            ]
        )

    @staticmethod
    def _write_frequencies(spamwriter, jobIRI, calc_id, data):
        # This section writes the vibrations to the ABox (if they exist).
        if "Frequencies" in data:
            spamwriter.writerow(
                [
                    comp_pref + "VibrationalAnalysis_" + calc_id,
                    "Instance",
                    gain_pref + "VibrationalAnalysis",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "VibrationalAnalysis_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "Frequency_" + calc_id,
                    "Instance",
                    gain_pref + "Frequency",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "VibrationalAnalysis_" + calc_id,
                    "Instance",
                    comp_pref + "Frequency_" + calc_id,
                    gain_pref + "hasResult",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_comp + "#hasFrequencies",
                    "Data Property",
                    comp_pref + "Frequency_" + calc_id,
                    "",
                    " ".join(str(i) for i in data["Frequencies"]),
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasVibrationCount",
                    "Data Property",
                    comp_pref + "Frequency_" + calc_id,
                    "",
                    data["Frequencies number"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "cm-1",
                    "Instance",
                    unit_pref + "qudt#FrequencyUnit",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "Frequency_" + calc_id,
                    "Instance",
                    gain_pref + "cm-1",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_rotations(spamwriter, jobIRI, calc_id, data):
        if "Rotational constants" in data:
            # This section writes the rotational constants information - rotational symmetry, rotational constants, and their values/units.
            spamwriter.writerow(
                [
                    comp_pref + "RotationalConstants_" + calc_id,
                    "Instance",
                    onto_comp + "#RotationalConstants",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "RotationalConstants_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_comp + "#hasRotationalConstants",
                    "Data Property",
                    comp_pref + "RotationalConstants_" + calc_id,
                    "",
                    " ".join(str(i) for i in data["Rotational constants"]),
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_comp + "#hasRotationalConstantsCount",
                    "Data Property",
                    comp_pref + "RotationalConstants_" + calc_id,
                    "",
                    data["Rotational constants number"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "RotationalConstants_" + calc_id,
                    "Instance",
                    unit_pref + "unit#GigaHertz",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

        if "Rotational symmetry number" in data:
            spamwriter.writerow(
                [
                    comp_pref + "RotationalSymmetry_" + calc_id,
                    "Instance",
                    onto_comp + "#RotationalSymmetry",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "RotationalSymmetry_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    onto_comp + "#hasRotationalSymmetryNumber",
                    "Data Property",
                    comp_pref + "RotationalSymmetry_" + calc_id,
                    "",
                    int(data["Rotational symmetry number"]),
                    "",
                ]
            )

    @staticmethod
    def _write_geom_type(spamwriter, jobIRI, calc_id, data):
        # This section writes the geometry type information.
        spamwriter.writerow(
            [
                comp_pref + "GeometryType_" + calc_id,
                "Instance",
                onto_comp + "#GeometryType",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                comp_pref + "GeometryType_" + calc_id,
                gain_pref + "isCalculationOn",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                onto_comp + "#hasGeometryType",
                "Data Property",
                comp_pref + "GeometryType_" + calc_id,
                "",
                data["Geometry type"],
                "",
            ]
        )

    @staticmethod
    def _write_zpe(spamwriter, jobIRI, calc_id, data):
        # This section writes the zero-point energy information (if it exists). Note that this requires a frequency calculation to be computed.
        if "Electronic and ZPE energy" in data and "Electronic energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "ZeroPointEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#ZeroPointEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "ZeroPointEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ZeroPointEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "ZeroPointEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_ZeroPointEnergy",
                    gain_pref + "hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_ZeroPointEnergy",
                    "",
                    data["Electronic and ZPE energy"] - data["Electronic energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ZeroPointEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
        elif "Electronic and ZPE energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "ElectronicAndZPEEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#ElectronicAndZPEEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "ElectronicAndZPEEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ElectronicAndZPEEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "ElectronicAndZPEEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_ElectronicAndZPEEnergy",
                    gain_pref + "hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_ElectronicAndZPEEnergy",
                    "",
                    data["Electronic and ZPE energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ElectronicAndZPEEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_scf(spamwriter, jobIRI, calc_id, data):
        if "Electronic energy" in data:
            # This section writes the electronic (SCF) energy information.
            spamwriter.writerow(
                [
                    comp_pref + "ScfEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#ScfEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "ScfEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ScfEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "ScfEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_ScfEnergy",
                    gain_pref + "hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_ScfEnergy",
                    "",
                    data["Electronic energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_ScfEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_occ(spamwriter, jobIRI, calc_id, data):
        # This section writes the information on the occupied orbitals: HOMO, HOMO-1, HOMO-2 energies.
        # HOMO
        if "HOMO energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "HomoEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#HomoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "HomoEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "HomoEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_HomoEnergy",
                    onto_comp + "#hasHomoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_HomoEnergy",
                    "",
                    data["HOMO energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
        # HOMO-1
        if "HOMO-1 energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "HomoMinusOneEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#HomoMinusOneEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "HomoMinusOneEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusOneEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "HomoMinusOneEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusOneEnergy",
                    onto_comp + "#hasHomoMinusOneEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusOneEnergy",
                    "",
                    data["HOMO-1 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusOneEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
        # HOMO-2
        if "HOMO-2 energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "HomoMinusTwoEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#HomoMinusTwoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "HomoMinusTwoEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusTwoEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "HomoMinusTwoEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusTwoEnergy",
                    onto_comp + "#hasHomoMinusTwoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusTwoEnergy",
                    "",
                    data["HOMO-2 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_HomoMinusTwoEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_virt(spamwriter, jobIRI, calc_id, data):
        # This section writes the information on the unoccupied (virtual) orbitals: LUMO, LUMO+1, LUMO+2 energies.
        # LUMO
        if "LUMO energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "LumoEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#LumoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "LumoEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "LumoEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_LumoEnergy",
                    onto_comp + "#hasLumoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_LumoEnergy",
                    "",
                    data["LUMO energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
        # LUMO+1
        if "LUMO+1 energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "LumoPlusOneEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#LumoPlusOneEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "LumoPlusOneEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusOneEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "LumoPlusOneEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusOneEnergy",
                    onto_comp + "#hasLumoPlusOneEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusOneEnergy",
                    "",
                    data["LUMO+1 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusOneEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
        # LUMO+2
        if "LUMO+2 energy" in data:
            spamwriter.writerow(
                [
                    comp_pref + "LumoPlusTwoEnergy_" + calc_id,
                    "Instance",
                    onto_comp + "#LumoPlusTwoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    comp_pref + "LumoPlusTwoEnergy_" + calc_id,
                    gain_pref + "isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusTwoEnergy",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "LumoPlusTwoEnergy_" + calc_id,
                    "Instance",
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusTwoEnergy",
                    onto_comp + "#hasLumoPlusTwoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusTwoEnergy",
                    "",
                    data["LUMO+2 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "FloatValue_" + calc_id + "_LumoPlusTwoEnergy",
                    "Instance",
                    unit_pref + "unit#Hartree",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )

    @staticmethod
    def _write_geom_opt(spamwriter, jobIRI, calc_id, data):
        # This section writes the geometry optimization, spin multiplicity and formal charge information.
        spamwriter.writerow(
            [
                comp_pref + "GeometryOptimization_" + calc_id,
                "Instance",
                gain_pref + "GeometryOptimization",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                comp_pref + "GeometryOptimization_" + calc_id,
                gain_pref + "isCalculationOn",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "Molecule_" + calc_id,
                "Instance",
                gain_pref + "Molecule",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "GeometryOptimization_" + calc_id,
                "Instance",
                comp_pref + "Molecule_" + calc_id,
                gain_pref + "hasMolecule",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                onto_comp + "#hasSpinMultiplicity",
                "Data Property",
                comp_pref + "Molecule_" + calc_id,
                "",
                data["Spin multiplicity"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "IntegerValue_" + calc_id + "_FormalCharge",
                "Instance",
                gain_pref + "IntegerValue",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "Molecule_" + calc_id,
                "Instance",
                comp_pref + "IntegerValue_" + calc_id + "_FormalCharge",
                gain_pref + "hasFormalCharge",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                gain_pref + "hasValue",
                "Data Property",
                comp_pref + "IntegerValue_" + calc_id + "_FormalCharge",
                "",
                data["Formal charge"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "IntegerValue_" + calc_id + "_FormalCharge",
                "Instance",
                gain_pref + "atomicUnit",
                gain_pref + "hasUnit",
                "",
                "",
            ]
        )

    @staticmethod
    def _write_atom_info(spamwriter, calc_id, data):
        # This section writes the atom coordinates and masses information.
        count = 1  # This count essentially counts the indices of the atoms starting with 1. Basically, this additional number helps uniquely assign an IRI to each atom.
        coord_string = ["x3", "y3", "z3"]  # How the coordinates are labeled.
        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        # Coordinates.
        for k in range(len(data["Atom types"])):
            spamwriter.writerow(
                [
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count),
                    "Instance",
                    gain_pref + "Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref + "Molecule_" + calc_id,
                    "Instance",
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count),
                    gain_pref + "hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count),
                    "Instance",
                    table_pref + "#" + data["Atom types"][k],
                    gain_pref + "isElement",
                    "",
                    "",
                ]
            )
            for i in range(3):
                spamwriter.writerow(
                    [
                        comp_pref
                        + "FloatValue_"
                        + calc_id
                        + "_"
                        + data["Atom types"][k]
                        + str(count)
                        + "_"
                        + coord_string[i]
                        + "Coordinate",
                        "Instance",
                        gain_pref + "FloatValue",
                        "",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        comp_pref
                        + "Atom_"
                        + calc_id
                        + "_"
                        + data["Atom types"][k]
                        + str(count),
                        "Instance",
                        comp_pref
                        + "FloatValue_"
                        + calc_id
                        + "_"
                        + data["Atom types"][k]
                        + str(count)
                        + "_"
                        + coord_string[i]
                        + "Coordinate",
                        gain_pref + "hasAtomCoordinate" + coords[i],
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        gain_pref + "hasValue",
                        "Data Property",
                        comp_pref
                        + "FloatValue_"
                        + calc_id
                        + "_"
                        + data["Atom types"][k]
                        + str(count)
                        + "_"
                        + coord_string[i]
                        + "Coordinate",
                        "",
                        data["Geometry"][k][i],
                        "",
                    ]
                )
            # Write atom masses.
            spamwriter.writerow(
                [
                    comp_pref
                    + "FloatValue_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count)
                    + "_Mass",
                    "Instance",
                    gain_pref + "FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "Atom_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count),
                    "Instance",
                    comp_pref
                    + "FloatValue_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count)
                    + "_Mass",
                    gain_pref + "hasMass",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    gain_pref + "hasValue",
                    "Data Property",
                    comp_pref
                    + "FloatValue_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count)
                    + "_Mass",
                    "",
                    data["Atomic masses"][k],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    comp_pref
                    + "FloatValue_"
                    + calc_id
                    + "_"
                    + data["Atom types"][k]
                    + str(count)
                    + "_Mass",
                    "Instance",
                    unit_pref + "unit#Dalton",
                    gain_pref + "hasUnit",
                    "",
                    "",
                ]
            )
            count += 1

    @staticmethod
    def _write_metadata(spamwriter, calc_id, data):
        # These are the final parts of the ABox with the auxillary info like software used and job run date.
        spamwriter.writerow(
            [
                onto_comp + "#hasProgram",
                "Data Property",
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "",
                data["Program name"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                onto_comp + "#hasProgramVersion",
                "Data Property",
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "",
                data["Program version"].split("+")[0][-1],
                "",
            ]
        )
        spamwriter.writerow(
            [
                onto_comp + "#hasRunDate",
                "Data Property",
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "",
                data["Run date"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                data_pref + "OutputSource_" + calc_id + ".g09",
                "Instance",
                onto_comp + "#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "Instance",
                data_pref + "OutputSource_" + calc_id + ".g09",
                gain_pref + "hasOutputFile",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                data_pref + "OutputSource_" + calc_id + ".xml",
                "Instance",
                onto_comp + "#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "Instance",
                data_pref + "OutputSource_" + calc_id + ".xml",
                gain_pref + "hasOutputFile",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                data_pref + "OutputSource_" + calc_id + ".png",
                "Instance",
                onto_comp + "#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                comp_pref + "SourcePackage_" + calc_id + "_EnvironmentModule",
                "Instance",
                data_pref + "OutputSource_" + calc_id + ".png",
                gain_pref + "hasOutputFile",
                "",
                "",
            ]
        )
