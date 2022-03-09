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

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            writer.write_header()

            self._write_initial(writer, entryIRI, calc_id, spec_IRI)
            self._write_mols(writer, calc_id, data)
            self._write_level_of_theory(writer, calc_id, data)
            self._write_name(writer, calc_id, data)
            self._write_frequencies(writer, entryIRI, calc_id, data)
            self._write_rotations(writer, entryIRI, calc_id, data)
            self._write_geom_type(writer, entryIRI, calc_id, data)
            self._write_zpe(writer, entryIRI, calc_id, data)
            self._write_scf(writer, entryIRI, calc_id, data)
            self._write_occ(writer, entryIRI, calc_id, data)
            self._write_virt(writer, entryIRI, calc_id, data)
            self._write_geom_opt(writer, entryIRI, calc_id, data)
            self._write_atom_info(writer, calc_id, data)
            self._write_metadata(writer, calc_id, data)

    def _write_initial(self, writer: Abox_Writer, jobIRI, calc_id, spec_IRI):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        inst_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["inst_spec"]
        has_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["has_spec"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        # This is all the initialization part of the ABox
        abox_name = "ABoxOntoCompChem"
        init_mod_iri = f"{comp_pref}InitializationModule_{calc_id}"
        writer.write_imports(name=abox_name, importing=onto_comp)
        writer.write_imports(name=abox_name, importing=comp_pref[:-1], rel="base")
        writer.write_inst(iri=jobIRI, type=onto_comp + "#G09")
        if spec_IRI:  # If you have the ontospecies IRI, it puts it here.
            # Otherwise, it leaves it out.
            writer.write_inst(iri=spec_IRI, type=inst_spec)
            writer.write_obj_prop(src_iri=jobIRI, trg_iri=spec_IRI, rel=has_spec)
        # Sets up initialization.
        writer.write_inst(
            iri=f"{init_mod_iri}", type=f"{onto_comp}#InitializationModule"
        )
        writer.write_obj_prop(
            src_iri=jobIRI,
            trg_iri=f"{init_mod_iri}",
            rel=f"{onto_comp}#hasInitialization",
        )
        writer.write_inst(
            iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            type=f"{gain_pref}SourcePackage",
        )
        writer.write_obj_prop(
            src_iri=jobIRI,
            trg_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            rel=f"{onto_comp}#hasEnvironment",
        )
        writer.write_inst(
            iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            type=f"{gain_pref}MoleculeProperty",
        )
        writer.write_obj_prop(
            src_iri=f"{init_mod_iri}",
            trg_iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            rel=f"{gain_pref}hasMoleculeProperty",
        )

    def _write_mols(self, writer: Abox_Writer, calc_id, data):
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

            writer.write_inst(
                iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                type=f"{gain_pref}Molecule",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}MoleculeProperty_{calc_id}_{atom}{count}",
                trg_iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                rel=f"{gain_pref}hasMolecule",
            )
            writer.write_inst(
                iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                type=f"{gain_pref}Atom",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Molecule_{calc_id}_{atom}{count}",
                trg_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                rel=f"{gain_pref}hasAtom",
            )
            writer.write_inst(iri=f"{table_pref}#{atom}", type=f"{table_pref}#Element")
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                trg_iri=f"{table_pref}#{atom}",
                rel=f"{gain_pref}isElement",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                rel=f"{gain_pref}hasNumberOfAtoms",
                value=at_count[k][1],
                data_type="Integer",
            )

    def _write_level_of_theory(self, writer: Abox_Writer, calc_id, data):
        # This section writes the information related to the level
        # of theory for the ABox (method and basis set).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        init_mod_iri = f"{comp_pref}InitializationModule_{calc_id}"
        lvl_theory = "LevelofTheoryParameter"
        meth_iri = f"{comp_pref}MethodologyFeature_{calc_id}_{lvl_theory}"

        writer.write_inst(
            iri=f"{comp_pref}LevelOfTheory_{calc_id}",
            type=f"{onto_comp}#LevelOfTheory",
        )
        writer.write_inst(iri=f"{meth_iri}", type=f"{gain_pref}MethodologyFeature")
        writer.write_obj_prop(
            src_iri=f"{init_mod_iri}",
            trg_iri=f"{meth_iri}",
            rel=f"{gain_pref}hasParameter",
        )
        writer.write_data_prop(
            iri=f"{meth_iri}",
            rel=f"{onto_comp}#hasLevelOfTheory",
            value=data["Method"],
        )
        writer.write_inst(
            iri=f"{comp_pref}BasisSet_{calc_id}", type=f"{gain_pref}BasisSet"
        )
        writer.write_obj_prop(
            src_iri=f"{init_mod_iri}",
            trg_iri=f"{comp_pref}BasisSet_{calc_id}",
            rel=f"{gain_pref}hasParameter",
        )
        writer.write_data_prop(
            iri=f"{comp_pref}BasisSet_{calc_id}",
            rel=f"{gain_pref}hasBasisSet",
            value=f'"{data["Basis set"]}"',
        )  # Note that the string formatting is used to escape the ',' in basis sets.

    def _write_name(self, writer: Abox_Writer, calc_id, data):
        # This writes the name of the species, taken as the formula,
        # but with extraneous 1s removed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]

        writer.write_data_prop(
            iri=f"{comp_pref}MoleculeProperty_{calc_id}",
            rel=f"{gain_pref}hasName",
            value=utilsfunc.formula_clean_re.sub("", data["Empirical formula"]),
        )

    def _write_frequencies(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the vibrations to the ABox (if they exist).

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Frequencies" in data:
            writer.write_inst(
                iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                type=f"{gain_pref}VibrationalAnalysis",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}Frequency_{calc_id}",
                type=f"{gain_pref}Frequency",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}VibrationalAnalysis_{calc_id}",
                trg_iri=f"{comp_pref}Frequency_{calc_id}",
                rel=f"{gain_pref}hasResult",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}Frequency_{calc_id}",
                rel=f"{onto_comp}#hasFrequencies",
                value=" ".join(str(i) for i in data["Frequencies"]),
            )
            writer.write_data_prop(
                iri=f"{comp_pref}Frequency_{calc_id}",
                rel=f"{gain_pref}hasVibrationCount",
                value=data["Frequencies number"],
            )
            writer.write_inst(
                iri=f"{gain_pref}cm-1", type=f"{unit_pref}qudt#FrequencyUnit"
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Frequency_{calc_id}",
                trg_iri=f"{gain_pref}cm-1",
                rel=f"{gain_pref}hasUnit",
            )

    def _write_rotations(self, writer: Abox_Writer, jobIRI, calc_id, data):

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

            writer.write_inst(
                iri=f"{comp_pref}RotationalConstants_{calc_id}",
                type=f"{onto_comp}#RotationalConstants",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}RotationalConstants_{calc_id}",
                rel=f"{onto_comp}#hasRotationalConstants",
                value=" ".join(str(i) for i in data["Rotational constants"]),
            )
            writer.write_data_prop(
                iri=f"{comp_pref}RotationalConstants_{calc_id}",
                rel=f"{onto_comp}#hasRotationalConstantsCount",
                value=data["Rotational constants number"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}RotationalConstants_{calc_id}",
                trg_iri=f"{unit_pref}unit#GigaHertz",
                rel=f"{gain_pref}hasUnit",
            )

        if "Rotational symmetry number" in data:
            writer.write_inst(
                iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                type=f"{onto_comp}#RotationalSymmetry",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}RotationalSymmetry_{calc_id}",
                rel=f"{onto_comp}#hasRotationalSymmetryNumber",
                value=str(int(data["Rotational symmetry number"])),
            )

    def _write_geom_type(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the geometry type information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        writer.write_inst(
            iri=f"{comp_pref}GeometryType_{calc_id}",
            type=f"{onto_comp}#GeometryType",
        )
        writer.write_obj_prop(
            src_iri=jobIRI,
            trg_iri=f"{comp_pref}GeometryType_{calc_id}",
            rel=f"{gain_pref}isCalculationOn",
        )
        writer.write_data_prop(
            iri=f"{comp_pref}GeometryType_{calc_id}",
            rel=f"{onto_comp}#hasGeometryType",
            value=data["Geometry type"],
        )

    def _write_zpe(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the zero-point energy information (if it exists).
        # Note that this requires a frequency calculation to be computed.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic and ZPE energy" in data and "Electronic energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                type=f"{onto_comp}#ZeroPointEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}ZeroPointEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                rel=f"{gain_pref}hasElectronicEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["Electronic and ZPE energy"] - data["Electronic energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_ZeroPointEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )
        elif "Electronic and ZPE energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                type=f"{onto_comp}#ElectronicAndZPEEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}ElectronicAndZPEEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                rel=f"{gain_pref}hasElectronicEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["Electronic and ZPE energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )

    def _write_scf(self, writer: Abox_Writer, jobIRI, calc_id, data):

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "Electronic energy" in data:
            # This section writes the electronic (SCF) energy information.
            writer.write_inst(
                iri=f"{comp_pref}ScfEnergy_{calc_id}",
                type=f"{onto_comp}#ScfEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}ScfEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}ScfEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                rel=f"{gain_pref}hasElectronicEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["Electronic energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_ScfEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )

    def _write_occ(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the information on the occupied orbitals:
        # HOMO, HOMO-1, HOMO-2 energies.
        # HOMO
        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "HOMO energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}HomoEnergy_{calc_id}",
                type=f"{onto_comp}#HomoEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}HomoEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}HomoEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                rel=f"{onto_comp}#hasHomoEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["HOMO energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_HomoEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )
        # HOMO-1
        if "HOMO-1 energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                type=f"{onto_comp}#HomoMinusOneEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}HomoMinusOneEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                rel=f"{onto_comp}#hasHomoMinusOneEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["HOMO-1 energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusOneEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )
        # HOMO-2
        if "HOMO-2 energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                type=f"{onto_comp}#HomoMinusTwoEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}HomoMinusTwoEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                rel=f"{onto_comp}#hasHomoMinusTwoEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["HOMO-2 energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_HomoMinusTwoEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )

    def _write_virt(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the information on the unoccupied (virtual)
        # orbitals: LUMO, LUMO+1, LUMO+2 energies.
        # LUMO

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        unit_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unit_pref"]

        if "LUMO energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}LumoEnergy_{calc_id}",
                type=f"{onto_comp}#LumoEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}LumoEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}LumoEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                rel=f"{onto_comp}#hasLumoEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["LUMO energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_LumoEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )
        # LUMO+1
        if "LUMO+1 energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                type=f"{onto_comp}#LumoPlusOneEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}LumoPlusOneEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                rel=f"{onto_comp}#hasLumoPlusOneEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["LUMO+1 energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusOneEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )
        # LUMO+2
        if "LUMO+2 energy" in data:
            writer.write_inst(
                iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                type=f"{onto_comp}#LumoPlusTwoEnergy",
            )
            writer.write_obj_prop(
                src_iri=jobIRI,
                trg_iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                rel=f"{gain_pref}isCalculationOn",
            )
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}LumoPlusTwoEnergy_{calc_id}",
                trg_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                rel=f"{onto_comp}#hasLumoPlusTwoEnergy",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                rel=f"{gain_pref}hasValue",
                value=data["LUMO+2 energy"],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{calc_id}_LumoPlusTwoEnergy",
                trg_iri=f"{unit_pref}unit#Hartree",
                rel=f"{gain_pref}hasUnit",
            )

    def _write_geom_opt(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the geometry optimization, spin multiplicity
        # and formal charge information.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]

        writer.write_inst(
            iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            type=f"{gain_pref}GeometryOptimization",
        )
        writer.write_obj_prop(
            src_iri=jobIRI,
            trg_iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            rel=f"{gain_pref}isCalculationOn",
        )
        writer.write_inst(
            iri=f"{comp_pref}Molecule_{calc_id}", type=f"{gain_pref}Molecule"
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}GeometryOptimization_{calc_id}",
            trg_iri=f"{comp_pref}Molecule_{calc_id}",
            rel=f"{gain_pref}hasMolecule",
        )
        writer.write_data_prop(
            iri=f"{comp_pref}Molecule_{calc_id}",
            rel=f"{onto_comp}#hasSpinMultiplicity",
            value=data["Spin multiplicity"],
        )
        writer.write_inst(
            iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            type=f"{gain_pref}IntegerValue",
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}Molecule_{calc_id}",
            trg_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            rel=f"{gain_pref}hasFormalCharge",
        )
        writer.write_data_prop(
            iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            rel=f"{gain_pref}hasValue",
            value=data["Formal charge"],
            data_type="Integer",
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}IntegerValue_{calc_id}_FormalCharge",
            trg_iri=f"{gain_pref}atomicUnit",
            rel=f"{gain_pref}hasUnit",
        )

    def _write_atom_info(self, writer: Abox_Writer, calc_id, data):
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

            writer.write_inst(iri=f"{comp_pref}Atom_{atom_id}", type=f"{gain_pref}Atom")
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Molecule_{calc_id}",
                trg_iri=f"{comp_pref}Atom_{atom_id}",
                rel=f"{gain_pref}hasAtom",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Atom_{atom_id}",
                trg_iri=f"{table_pref}#{atom}",
                rel=f"{gain_pref}isElement",
            )
            for i in range(3):
                writer.write_inst(
                    iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    type=f"{gain_pref}FloatValue",
                )
                writer.write_obj_prop(
                    src_iri=f"{comp_pref}Atom_{calc_id}_{atom}{count}",
                    trg_iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    rel=f"{gain_pref}hasAtomCoordinate{coords[i]}",
                )
                writer.write_data_prop(
                    iri=(
                        f"{comp_pref}FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    rel=f"{gain_pref}hasValue",
                    value=data["Geometry"][k][i],
                )
            # Write atom masses.
            writer.write_inst(
                iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                type=f"{gain_pref}FloatValue",
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}Atom_{atom_id}",
                trg_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                rel=f"{gain_pref}hasMass",
            )
            writer.write_data_prop(
                iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                rel=f"{gain_pref}hasValue",
                value=data["Atomic masses"][k],
            )
            writer.write_obj_prop(
                src_iri=f"{comp_pref}FloatValue_{atom_id}_Mass",
                trg_iri=f"{unit_pref}unit#Dalton",
                rel=f"{gain_pref}hasUnit",
            )
            count += 1

    def _write_metadata(self, writer: Abox_Writer, calc_id, data):
        # These are the final parts of the ABox with the
        # auxillary info like software used and job run date.

        comp_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["comp_pref"]
        gain_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["gain_pref"]
        onto_comp = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_comp"]
        ocompchem_data_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY][
            "ocompchem_data_pref"
        ]

        writer.write_data_prop(
            iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            rel=f"{onto_comp}#hasProgram",
            value=data["Program name"],
        )
        writer.write_data_prop(
            iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            rel=f"{onto_comp}#hasProgramVersion",
            value=data["Program version"].split("+")[0][-1],
        )
        writer.write_data_prop(
            iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            rel=f"{onto_comp}#hasRunDate",
            value=data["Run date"],
        )
        writer.write_inst(
            iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
            type=f"{onto_comp}#OutputSource",
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.g09",
            rel=f"{gain_pref}hasOutputFile",
        )
        writer.write_inst(
            iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
            type=f"{onto_comp}#OutputSource",
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.xml",
            rel=f"{gain_pref}hasOutputFile",
        )
        writer.write_inst(
            iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
            type=f"{onto_comp}#OutputSource",
        )
        writer.write_obj_prop(
            src_iri=f"{comp_pref}SourcePackage_{calc_id}_EnvironmentModule",
            trg_iri=f"{ocompchem_data_pref}OutputSource_{calc_id}.png",
            rel=f"{gain_pref}hasOutputFile",
        )
