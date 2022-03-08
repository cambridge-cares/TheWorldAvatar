import json
import chemaboxwriters.common.globals as globals
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.globals import aboxStages
from typing import List, Optional, Dict
import chemaboxwriters.common.endpoints_proxy as endp
import chemaboxwriters.common.aboxconfig as abconf
from enum import Enum

Abox_Writer = utilsfunc.Abox_csv_writer


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
            required_configs={
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

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as aboxwriter:
            aboxwriter.write_header()

            self._write_initial(aboxwriter, entryIRI, calc_id, spec_IRI)
            self._write_mols(aboxwriter, calc_id, data)
            self._write_level_of_theory(aboxwriter, calc_id, data)
            self._write_name(aboxwriter, calc_id, data)
            self._write_frequencies(aboxwriter, entryIRI, calc_id, data)
            self._write_rotations(aboxwriter, entryIRI, calc_id, data)
            self._write_geom_type(aboxwriter, entryIRI, calc_id, data)
            self._write_zpe(aboxwriter, entryIRI, calc_id, data)
            self._write_scf(aboxwriter, entryIRI, calc_id, data)
            self._write_occ(aboxwriter, entryIRI, calc_id, data)
            self._write_virt(aboxwriter, entryIRI, calc_id, data)
            self._write_geom_opt(aboxwriter, entryIRI, calc_id, data)
            self._write_atom_info(aboxwriter, calc_id, data)
            self._write_metadata(aboxwriter, calc_id, data)

    def _write_initial(self, aboxwriter: Abox_Writer, jobIRI, calc_id, spec_IRI):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        inst_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["inst_spec"]
        has_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["has_spec"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        # This is all the initialization part of the ABox
        aboxwriter.write_imports(
            abox_name="ABoxOntoCompChem",
            importing=onto_comp,
            relation="http://www.w3.org/2002/07/owl#imports",
        )
        aboxwriter.write_imports(
            abox_name="ABoxOntoCompChem", importing=comp_pref[:-1], relation="base"
        )
        aboxwriter.write_instance(inst_iri=jobIRI, inst_class=onto_comp + "#G09")
        if spec_IRI:  # If you have the ontospecies IRI, it puts it here.
            # Otherwise, it leaves it out.
            aboxwriter.write_instance(inst_iri=spec_IRI, inst_class=inst_spec)
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI, trg_inst_iri=spec_IRI, relation=has_spec
            )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}InitializationModule_{calc_id}",
            inst_class=f"{onto_comp}#InitializationModule",
        )  # Sets up initialization.
        aboxwriter.write_object_property(
            src_inst_iri=jobIRI,
            trg_inst_iri=f"{comp_pref}InitializationModule_{calc_id}",
            relation=f"{onto_comp}#hasInitialization",
        )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            inst_class=f"{gain_pref}SourcePackage",
        )
        aboxwriter.write_object_property(
            src_inst_iri=jobIRI,
            trg_inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            relation=f"{onto_comp}#hasEnvironment",
        )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            inst_class=f"{gain_pref}MoleculeProperty",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}InitializationModule_{calc_id}",
            trg_inst_iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            relation=f"{gain_pref}hasMoleculeProperty",
        )

    def _write_mols(self, aboxwriter: Abox_Writer, calc_id, data):
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

            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                inst_class=f"{gain_pref}Molecule",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}MoleculeProperty_{calc_id}_{atom}{count}",
                trg_inst_iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                relation=f"{gain_pref}hasMolecule",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                inst_class=f"{gain_pref}Atom",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                trg_inst_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                relation=f"{gain_pref}hasAtom",
            )
            aboxwriter.write_instance(
                inst_iri=f"{table_pref}#{atom}", inst_class=f"{table_pref}#Element"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                trg_inst_iri=f"{table_pref}#{atom}",
                relation=f"{gain_pref}isElement",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                relation=f"{gain_pref}hasNumberOfAtoms",
                value=at_count[k][1],
            )

    def _write_level_of_theory(self, aboxwriter: Abox_Writer, calc_id, data):
        # This section writes the information related to the level
        # of theory for the ABox (method and basis set).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}LevelOfTheory_{calc_id}",
            inst_class=f"{onto_comp}#LevelOfTheory",
        )
        lvl_theory = "LevelofTheoryParameter"
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}MethodologyFeature_{calc_id}_{lvl_theory}",
            inst_class=f"{gain_pref}MethodologyFeature",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}InitializationModule_{calc_id}",
            trg_inst_iri=f"{comp_pref}MethodologyFeature_{calc_id}_{lvl_theory}",
            relation=f"{gain_pref}hasParameter",
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}MethodologyFeature_{calc_id}_{lvl_theory}",
            relation=f"{onto_comp}#hasLevelOfTheory",
            value=data["Method"],
        )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}BasisSet_{calc_id}", inst_class=f"{gain_pref}BasisSet"
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}InitializationModule_{calc_id}",
            trg_inst_iri=f"{comp_pref}BasisSet_{calc_id}",
            relation=f"{gain_pref}hasParameter",
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}BasisSet_{calc_id}",
            relation=f"{gain_pref}hasBasisSet",
            value=f'"{data["Basis set"]}"',
        )  # Note that the string formatting is used to escape the ',' in basis sets.

    def _write_name(self, aboxwriter: Abox_Writer, calc_id, data):
        # This writes the name of the species, taken as the formula,
        # but with extraneous 1s removed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            relation=f"{gain_pref}hasName",
            value=utilsfunc.formula_clean_re.sub("", data["Empirical formula"]),
        )

    def _write_frequencies(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the vibrations to the ABox (if they exist).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Frequencies" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                inst_class=f"{gain_pref}VibrationalAnalysis",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}Frequency_{calc_id}",
                inst_class=f"{gain_pref}Frequency",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                trg_inst_iri=f"{comp_pref}Frequency_{calc_id}",
                relation=f"{gain_pref}hasResult",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}Frequency_{calc_id}",
                relation=f"{onto_comp}#hasFrequencies",
                value=" ".join(str(i) for i in data["Frequencies"]),
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}Frequency_{calc_id}",
                relation=f"{gain_pref}hasVibrationCount",
                value=data["Frequencies number"],
            )
            aboxwriter.write_instance(
                inst_iri=f"{gain_pref}cm-1", inst_class=f"{unit_pref}qudt#FrequencyUnit"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Frequency_{calc_id}",
                trg_inst_iri=f"{gain_pref}cm-1",
                relation=f"{gain_pref}hasUnit",
            )

    def _write_rotations(self, aboxwriter, jobIRI, calc_id, data):

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

            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                inst_class=f"{onto_comp}#RotationalConstants",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                relation=f"{onto_comp}#hasRotationalConstants",
                value=" ".join(str(i) for i in data["Rotational constants"]),
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                relation=f"{onto_comp}#hasRotationalConstantsCount",
                value=data["Rotational constants number"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                trg_inst_iri=f"{unit_pref}unit#GigaHertz",
                relation=f"{gain_pref}hasUnit",
            )

        if "Rotational symmetry number" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                inst_class=f"{onto_comp}#RotationalSymmetry",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                relation=f"{onto_comp}#hasRotationalSymmetryNumber",
                value=int(data["Rotational symmetry number"]),
            )

    def _write_geom_type(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the geometry type information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}GeometryType_{calc_id}",
            inst_class=f"{onto_comp}#GeometryType",
        )
        aboxwriter.write_object_property(
            src_inst_iri=jobIRI,
            trg_inst_iri=f"{comp_pref}GeometryType_{calc_id}",
            relation=f"{gain_pref}isCalculationOn",
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}GeometryType_{calc_id}",
            relation=f"{onto_comp}#hasGeometryType",
            value=data["Geometry type"],
        )

    def _write_zpe(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the zero-point energy information (if it exists).
        # Note that this requires a frequency calculation to be computed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic and ZPE energy" in data and "Electronic energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                inst_class=f"{onto_comp}#ZeroPointEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                relation=f"{gain_pref}hasElectronicEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["Electronic and ZPE energy"] - data["Electronic energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )
        elif "Electronic and ZPE energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                inst_class=f"{onto_comp}#ElectronicAndZPEEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                relation=f"{gain_pref}hasElectronicEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["Electronic and ZPE energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )

    def _write_scf(self, aboxwriter, jobIRI, calc_id, data):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic energy" in data:
            # This section writes the electronic (SCF) energy information.
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}ScfEnergy_{calc_id}",
                inst_class=f"{onto_comp}#ScfEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}ScfEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}ScfEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                relation=f"{gain_pref}hasElectronicEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["Electronic energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )

    def _write_occ(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the information on the occupied orbitals:
        # HOMO, HOMO-1, HOMO-2 energies.
        # HOMO
        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "HOMO energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}HomoEnergy_{calc_id}",
                inst_class=f"{onto_comp}#HomoEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}HomoEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}HomoEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                relation=f"{onto_comp}#hasHomoEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["HOMO energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )
        # HOMO-1
        if "HOMO-1 energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                inst_class=f"{onto_comp}#HomoMinusOneEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                relation=f"{onto_comp}#hasHomoMinusOneEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["HOMO-1 energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )
        # HOMO-2
        if "HOMO-2 energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                inst_class=f"{onto_comp}#HomoMinusTwoEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                relation=f"{onto_comp}#hasHomoMinusTwoEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["HOMO-2 energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )

    def _write_virt(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the information on the unoccupied (virtual)
        # orbitals: LUMO, LUMO+1, LUMO+2 energies.
        # LUMO

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "LUMO energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}LumoEnergy_{calc_id}",
                inst_class=f"{onto_comp}#LumoEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}LumoEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}LumoEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                relation=f"{onto_comp}#hasLumoEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["LUMO energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )
        # LUMO+1
        if "LUMO+1 energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                inst_class=f"{onto_comp}#LumoPlusOneEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                relation=f"{onto_comp}#hasLumoPlusOneEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["LUMO+1 energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )
        # LUMO+2
        if "LUMO+2 energy" in data:
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                inst_class=f"{onto_comp}#LumoPlusTwoEnergy",
            )
            aboxwriter.write_object_property(
                src_inst_iri=jobIRI,
                trg_inst_iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                relation=f"{gain_pref}isCalculationOn",
            )
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                relation=f"{onto_comp}#hasLumoPlusTwoEnergy",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                relation=f"{gain_pref}hasValue",
                value=data["LUMO+2 energy"],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                trg_inst_iri=f"{unit_pref}unit#Hartree",
                relation=f"{gain_pref}hasUnit",
            )

    def _write_geom_opt(self, aboxwriter, jobIRI, calc_id, data):
        # This section writes the geometry optimization, spin multiplicity
        # and formal charge information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            inst_class=f"{gain_pref}GeometryOptimization",
        )
        aboxwriter.write_object_property(
            src_inst_iri=jobIRI,
            trg_inst_iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            relation=f"{gain_pref}isCalculationOn",
        )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}Molecule_{calc_id}", inst_class=f"{gain_pref}Molecule"
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            trg_inst_iri=f"{comp_pref}Molecule_{calc_id}",
            relation=f"{gain_pref}hasMolecule",
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}Molecule_{calc_id}",
            relation=f"{onto_comp}#hasSpinMultiplicity",
            value=data["Spin multiplicity"],
        )
        aboxwriter.write_instance(
            inst_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            inst_class=f"{gain_pref}IntegerValue",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}Molecule_{calc_id}",
            trg_inst_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            relation=f"{gain_pref}hasFormalCharge",
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            relation=f"{gain_pref}hasValue",
            value=data["Formal charge"],
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            trg_inst_iri=f"{gain_pref}atomicUnit",
            relation=f"{gain_pref}hasUnit",
        )

    def _write_atom_info(self, aboxwriter, calc_id, data):
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

            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}Atom_{atom_id}", inst_class=f"{gain_pref}Atom"
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Molecule_{calc_id}",
                trg_inst_iri=f"{comp_pref}Atom_{atom_id}",
                relation=f"{gain_pref}hasAtom",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Atom_{atom_id}",
                trg_inst_iri=f"{table_pref}#{atom}",
                relation=f"{gain_pref}isElement",
            )
            for i in range(3):
                aboxwriter.write_instance(
                    inst_iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    inst_class=f"{gain_pref}FloatValue",
                )
                aboxwriter.write_object_property(
                    src_inst_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    trg_inst_iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    relation=f"{gain_pref}hasAtomCoordinate{coords[i]}",
                )
                aboxwriter.write_data_property(
                    inst_iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    relation=f"{gain_pref}hasValue",
                    value=data["Geometry"][k][i],
                )
            # Write atom masses.
            aboxwriter.write_instance(
                inst_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                inst_class=f"{gain_pref}FloatValue",
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}Atom_{atom_id}",
                trg_inst_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                relation=f"{gain_pref}hasMass",
            )
            aboxwriter.write_data_property(
                inst_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                relation=f"{gain_pref}hasValue",
                value=data["Atomic masses"][k],
            )
            aboxwriter.write_object_property(
                src_inst_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                trg_inst_iri=f"{unit_pref}unit#Dalton",
                relation=f"{gain_pref}hasUnit",
            )
            count += 1

    def _write_metadata(self, aboxwriter, calc_id, data):
        # These are the final parts of the ABox with the
        # auxillary info like software used and job run date.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        ocompchem_data_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY][
            "ocompchem_data_pref"
        ]

        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            relation=f"{onto_comp}#hasProgram",
            value=data["Program name"],
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            relation=f"{onto_comp}#hasProgramVersion",
            value=data["Program version"].split("+")[0][-1],
        )
        aboxwriter.write_data_property(
            inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            relation=f"{onto_comp}#hasRunDate",
            value=data["Run date"],
        )
        aboxwriter.write_instance(
            inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
            inst_class=f"{onto_comp}#OutputSource",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
            relation=f"{gain_pref}hasOutputFile",
        )
        aboxwriter.write_instance(
            inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
            inst_class=f"{onto_comp}#OutputSource",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
            relation=f"{gain_pref}hasOutputFile",
        )
        aboxwriter.write_instance(
            inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
            inst_class=f"{onto_comp}#OutputSource",
        )
        aboxwriter.write_object_property(
            src_inst_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_inst_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
            relation=f"{gain_pref}hasOutputFile",
        )
