import json
import csv
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.globals import aboxStages
from typing import List, Optional, Dict
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from enum import Enum


class OC_JSON_TO_OC_CSV_Handler(Handler):
    """Handler converting oc_json files to oc_csv.
    Inputs: List of oc json file paths
    Outputs: List of owl file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="OC_JSON_TO_OC_CSV",
            in_stage=aboxStages.OC_JSON,
            out_stage=aboxStages.OC_CSV,
            endpoints_proxy=endpoints_proxy,
            required_endpoints_config={
                abconf.WRITERS_PREFIXES_KEY: [
                    "comp_pref",
                    "ocompchem_data_pref",
                    "onto_comp",
                    "inst_spec",
                    "has_spec",
                    "gain_pref",
                    "table_pref",
                    "unit_pref",
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
        **handler_kwargs,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage.name.lower(),
                out_dir=out_dir,
            )
            self._oc_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **handler_kwargs,
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

    def _write_initial(self, spamwriter, jobIRI, calc_id, spec_IRI):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        inst_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["inst_spec"]
        has_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["has_spec"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

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
        if spec_IRI:  # If you have the ontospecies IRI, it puts it here.
            # Otherwise, it leaves it out.
            spamwriter.writerow([spec_IRI, "Instance", inst_spec, "", "", ""])
            spamwriter.writerow([jobIRI, "Instance", spec_IRI, has_spec, "", ""])
        spamwriter.writerow(
            [
                f"{comp_pref}InitializationModule_{calc_id}",
                "Instance",
                f"{onto_comp}#InitializationModule",
                "",
                "",
                "",
            ]
        )  # Sets up initialization.
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                f"{comp_pref}InitializationModule_{calc_id}",
                f"{onto_comp}#hasInitialization",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "Instance",
                f"{gain_pref}SourcePackage",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                f"{onto_comp}#hasEnvironment",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}MoleculeProperty_{calc_id}",
                "Instance",
                f"{gain_pref}MoleculeProperty",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}InitializationModule_{calc_id}",
                "Instance",
                f"{comp_pref}MoleculeProperty_{calc_id}",
                f"{gain_pref}hasMoleculeProperty",
                "",
                "",
            ]
        )

    def _write_mols(self, spamwriter, calc_id, data):
        # This section starts the representation of the molecule, namely dividing
        # the species into sub-molecules that contain the different atom types.
        # This will hopefully be changed by an update in OntoCompChem later.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        table_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["table_pref"]

        at_count = []
        for key, value in data["Atom counts"].items():
            temp = [key, value]
            at_count.append(temp)
        for k in range(
            len(at_count)
        ):  # For each atom in the molecule, make a molecule object
            # (This is the way it's done atm.)

            atom = at_count[k][0]
            count = str(float(at_count[k][1]))

            spamwriter.writerow(
                [
                    f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                    "Instance",
                    f"{gain_pref}Molecule",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}MoleculeProperty_{calc_id}_{atom}{count}",
                    "Instance",
                    f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                    f"{gain_pref}hasMolecule",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    "Instance",
                    f"{gain_pref}Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                    "Instance",
                    f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    f"{gain_pref}hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{table_pref}#{atom}",
                    "Instance",
                    f"{table_pref}#Element",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    "Instance",
                    f"{table_pref}#{atom}",
                    f"{gain_pref}isElement",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasNumberOfAtoms",
                    "Data Property",
                    f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    "",
                    at_count[k][1],
                    "",
                ]
            )

    def _write_level_of_theory(self, spamwriter, calc_id, data):
        # This section writes the information related to the level
        # of theory for the ABox (method and basis set).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        spamwriter.writerow(
            [
                f"{comp_pref}LevelOfTheory_{calc_id}",
                "Instance",
                f"{onto_comp}#LevelOfTheory",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}MethodologyFeature_{calc_id}_LevelofTheoryParameter",
                "Instance",
                f"{gain_pref}MethodologyFeature",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}InitializationModule_{calc_id}",
                "Instance",
                f"{comp_pref}MethodologyFeature_{calc_id}_LevelofTheoryParameter",
                f"{gain_pref}hasParameter",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_comp}#hasLevelOfTheory",
                "Data Property",
                f"{comp_pref}MethodologyFeature_{calc_id}_LevelofTheoryParameter",
                "",
                data["Method"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}BasisSet_{calc_id}",
                "Instance",
                f"{gain_pref}BasisSet",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}InitializationModule_{calc_id}",
                "Instance",
                f"{comp_pref}BasisSet_{calc_id}",
                f"{gain_pref}hasParameter",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{gain_pref}hasBasisSet",
                "Data Property",
                f"{comp_pref}BasisSet_{calc_id}",
                "",
                f'"{data["Basis set"]}"',
                "",
            ]
        )  # Note that the string formatting is used to escape the ',' in basis sets.

    def _write_name(self, spamwriter, calc_id, data):
        # This writes the name of the species, taken as the formula,
        # but with extraneous 1s removed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        spamwriter.writerow(
            [
                f"{gain_pref}hasName",
                "Data Property",
                f"{comp_pref}MoleculeProperty_{calc_id}",
                "",
                utilsfunc.formula_clean_re.sub("", data["Empirical formula"]),
                "",
            ]
        )

    def _write_frequencies(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the vibrations to the ABox (if they exist).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Frequencies" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}VibrationalAnalysis_{calc_id}",
                    "Instance",
                    f"{gain_pref}VibrationalAnalysis",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}VibrationalAnalysis_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Frequency_{calc_id}",
                    "Instance",
                    f"{gain_pref}Frequency",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}VibrationalAnalysis_{calc_id}",
                    "Instance",
                    f"{comp_pref}Frequency_{calc_id}",
                    f"{gain_pref}hasResult",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_comp}#hasFrequencies",
                    "Data Property",
                    f"{comp_pref}Frequency_{calc_id}",
                    "",
                    " ".join(str(i) for i in data["Frequencies"]),
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasVibrationCount",
                    "Data Property",
                    f"{comp_pref}Frequency_{calc_id}",
                    "",
                    data["Frequencies number"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}cm-1",
                    "Instance",
                    f"{unit_pref}qudt#FrequencyUnit",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Frequency_{calc_id}",
                    "Instance",
                    f"{gain_pref}cm-1",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

    def _write_rotations(self, spamwriter, jobIRI, calc_id, data):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Rotational constants" in data:
            # This section writes the rotational constants information
            # - rotational symmetry, rotational constants, and their values/units.

            comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
            onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
            gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
            unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

            spamwriter.writerow(
                [
                    f"{comp_pref}RotationalConstants_{calc_id}",
                    "Instance",
                    f"{onto_comp}#RotationalConstants",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}RotationalConstants_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_comp}#hasRotationalConstants",
                    "Data Property",
                    f"{comp_pref}RotationalConstants_{calc_id}",
                    "",
                    " ".join(str(i) for i in data["Rotational constants"]),
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_comp}#hasRotationalConstantsCount",
                    "Data Property",
                    f"{comp_pref}RotationalConstants_{calc_id}",
                    "",
                    data["Rotational constants number"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}RotationalConstants_{calc_id}",
                    "Instance",
                    f"{unit_pref}unit#GigaHertz",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

        if "Rotational symmetry number" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}RotationalSymmetry_{calc_id}",
                    "Instance",
                    f"{onto_comp}#RotationalSymmetry",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}RotationalSymmetry_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{onto_comp}#hasRotationalSymmetryNumber",
                    "Data Property",
                    f"{comp_pref}RotationalSymmetry_{calc_id}",
                    "",
                    int(data["Rotational symmetry number"]),
                    "",
                ]
            )

    def _write_geom_type(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the geometry type information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        spamwriter.writerow(
            [
                f"{comp_pref}GeometryType_{calc_id}",
                "Instance",
                f"{onto_comp}#GeometryType",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                f"{comp_pref}GeometryType_{calc_id}",
                f"{gain_pref}isCalculationOn",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_comp}#hasGeometryType",
                "Data Property",
                f"{comp_pref}GeometryType_{calc_id}",
                "",
                data["Geometry type"],
                "",
            ]
        )

    def _write_zpe(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the zero-point energy information (if it exists).
        # Note that this requires a frequency calculation to be computed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic and ZPE energy" in data and "Electronic energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}ZeroPointEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#ZeroPointEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}ZeroPointEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}ZeroPointEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                    f"{gain_pref}hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                    "",
                    data["Electronic and ZPE energy"] - data["Electronic energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
        elif "Electronic and ZPE energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#ElectronicAndZPEEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                    f"{gain_pref}hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                    "",
                    data["Electronic and ZPE energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

    def _write_scf(self, spamwriter, jobIRI, calc_id, data):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic energy" in data:
            # This section writes the electronic (SCF) energy information.
            spamwriter.writerow(
                [
                    f"{comp_pref}ScfEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#ScfEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}ScfEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}ScfEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                    f"{gain_pref}hasElectronicEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                    "",
                    data["Electronic energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

    def _write_occ(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the information on the occupied orbitals:
        # HOMO, HOMO-1, HOMO-2 energies.
        # HOMO
        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "HOMO energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#HomoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}HomoEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                    f"{onto_comp}#hasHomoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                    "",
                    data["HOMO energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
        # HOMO-1
        if "HOMO-1 energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#HomoMinusOneEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                    f"{onto_comp}#hasHomoMinusOneEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                    "",
                    data["HOMO-1 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
        # HOMO-2
        if "HOMO-2 energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#HomoMinusTwoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                    f"{onto_comp}#hasHomoMinusTwoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                    "",
                    data["HOMO-2 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

    def _write_virt(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the information on the unoccupied (virtual)
        # orbitals: LUMO, LUMO+1, LUMO+2 energies.
        # LUMO

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "LUMO energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#LumoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}LumoEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                    f"{onto_comp}#hasLumoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                    "",
                    data["LUMO energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
        # LUMO+1
        if "LUMO+1 energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#LumoPlusOneEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                    f"{onto_comp}#hasLumoPlusOneEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                    "",
                    data["LUMO+1 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
        # LUMO+2
        if "LUMO+2 energy" in data:
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                    "Instance",
                    f"{onto_comp}#LumoPlusTwoEnergy",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    jobIRI,
                    "Instance",
                    f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                    f"{gain_pref}isCalculationOn",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                    f"{onto_comp}#hasLumoPlusTwoEnergy",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                    "",
                    data["LUMO+2 energy"],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                    "Instance",
                    f"{unit_pref}unit#Hartree",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )

    def _write_geom_opt(self, spamwriter, jobIRI, calc_id, data):
        # This section writes the geometry optimization, spin multiplicity
        # and formal charge information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        spamwriter.writerow(
            [
                f"{comp_pref}GeometryOptimization_{calc_id}",
                "Instance",
                f"{gain_pref}GeometryOptimization",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                jobIRI,
                "Instance",
                f"{comp_pref}GeometryOptimization_{calc_id}",
                f"{gain_pref}isCalculationOn",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}Molecule_{calc_id}",
                "Instance",
                f"{gain_pref}Molecule",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}GeometryOptimization_{calc_id}",
                "Instance",
                f"{comp_pref}Molecule_{calc_id}",
                f"{gain_pref}hasMolecule",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_comp}#hasSpinMultiplicity",
                "Data Property",
                f"{comp_pref}Molecule_{calc_id}",
                "",
                data["Spin multiplicity"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
                "Instance",
                f"{gain_pref}IntegerValue",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}Molecule_{calc_id}",
                "Instance",
                f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
                f"{gain_pref}hasFormalCharge",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{gain_pref}hasValue",
                "Data Property",
                f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
                "",
                data["Formal charge"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
                "Instance",
                f"{gain_pref}atomicUnit",
                f"{gain_pref}hasUnit",
                "",
                "",
            ]
        )

    def _write_atom_info(self, spamwriter, calc_id, data):
        # This section writes the atom coordinates and masses information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]
        table_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["table_pref"]

        count = 1  # This count essentially counts the indices of the atoms starting
        # with 1. Basically, this additional number helps uniquely assign an IRI
        # to each atom.
        coord_string = ["x3", "y3", "z3"]  # How the coordinates are labeled.
        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        # Coordinates.
        for k in range(len(data["Atom types"])):

            atom = data["Atom types"][k]
            atom_id = f"{calc_id}_{atom}{count}"

            spamwriter.writerow(
                [
                    f"{comp_pref}Atom_{atom_id}",
                    "Instance",
                    f"{gain_pref}Atom",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Molecule_{calc_id}",
                    "Instance",
                    f"{comp_pref}Atom_{atom_id}",
                    f"{gain_pref}hasAtom",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Atom_{atom_id}",
                    "Instance",
                    f"{table_pref}#{atom}",
                    f"{gain_pref}isElement",
                    "",
                    "",
                ]
            )
            for i in range(3):
                spamwriter.writerow(
                    [
                        f"{comp_pref}FloatValue_{atom_id}_{coord_string[i]}Coordinate",
                        "Instance",
                        f"{gain_pref}FloatValue",
                        "",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                        "Instance",
                        f"{comp_pref}FloatValue_{atom_id}_{coord_string[i]}Coordinate",
                        f"{gain_pref}hasAtomCoordinate{coords[i]}",
                        "",
                        "",
                    ]
                )
                spamwriter.writerow(
                    [
                        f"{gain_pref}hasValue",
                        "Data Property",
                        f"{comp_pref}FloatValue_{atom_id}_{coord_string[i]}Coordinate",
                        "",
                        data["Geometry"][k][i],
                        "",
                    ]
                )
            # Write atom masses.
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{atom_id}_Mass",
                    "Instance",
                    f"{gain_pref}FloatValue",
                    "",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}Atom_{atom_id}",
                    "Instance",
                    f"{comp_pref}FloatValue_{atom_id}_Mass",
                    f"{gain_pref}hasMass",
                    "",
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{gain_pref}hasValue",
                    "Data Property",
                    f"{comp_pref}FloatValue_{atom_id}_Mass",
                    "",
                    data["Atomic masses"][k],
                    "",
                ]
            )
            spamwriter.writerow(
                [
                    f"{comp_pref}FloatValue_{atom_id}_Mass",
                    "Instance",
                    f"{unit_pref}unit#Dalton",
                    f"{gain_pref}hasUnit",
                    "",
                    "",
                ]
            )
            count += 1

    def _write_metadata(self, spamwriter, calc_id, data):
        # These are the final parts of the ABox with the
        # auxillary info like software used and job run date.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        ocompchem_data_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY][
            "ocompchem_data_pref"
        ]

        spamwriter.writerow(
            [
                f"{onto_comp}#hasProgram",
                "Data Property",
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "",
                data["Program name"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_comp}#hasProgramVersion",
                "Data Property",
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "",
                data["Program version"].split("+")[0][-1],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{onto_comp}#hasRunDate",
                "Data Property",
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "",
                data["Run date"],
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
                "Instance",
                f"{onto_comp}#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "Instance",
                f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
                f"{gain_pref}hasOutputFile",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
                "Instance",
                f"{onto_comp}#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "Instance",
                f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
                f"{gain_pref}hasOutputFile",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
                "Instance",
                f"{onto_comp}#OutputSource",
                "",
                "",
                "",
            ]
        )
        spamwriter.writerow(
            [
                f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
                "Instance",
                f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
                f"{gain_pref}hasOutputFile",
                "",
                "",
            ]
        )
