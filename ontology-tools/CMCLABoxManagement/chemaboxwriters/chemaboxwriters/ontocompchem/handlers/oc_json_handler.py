import json
import chemaboxwriters.common.params as params
from chemaboxwriters.common.handler import Handler
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.handlers import CCLOG_SOURCE_LOCATION
from chemaboxwriters.ontocompchem.handlers.qc_json_handler import (
    PNG_SOURCE_LOCATION,
    XML_SOURCE_LOCATION,
)
from chemaboxwriters.ontocompchem.abox_stages import OC_ABOX_STAGES
from typing import List


Abox_Writer = utilsfunc.Abox_csv_writer


HANDLER_PREFIXES = {
    "comp_pref": {"required": True},
    "ocompchem_data_pref": {"required": True},
    "onto_comp": {"required": True},
    "inst_spec": {"required": True},
    "has_spec": {"required": True},
    "gain_pref": {"required": True},
    "table_pref": {"required": True},
    "unit_pref": {"required": True},
}


class OC_JSON_TO_OC_CSV_Handler(Handler):
    """Handler converting oc_json files to oc_csv.
    Inputs: List of oc json file paths
    Outputs: List of owl file paths
    """

    def __init__(self) -> None:
        super().__init__(
            name="OC_JSON_TO_OC_CSV",
            in_stage=OC_ABOX_STAGES.oc_json,  # type: ignore
            out_stage=OC_ABOX_STAGES.oc_csv,  # type: ignore
            prefixes=HANDLER_PREFIXES,
        )

    def handle_input(
        self,
        inputs: List[str],
        out_dir: str,
        input_type: str,
        dry_run: bool,
    ) -> List[str]:

        outputs: List[str] = []
        for json_file_path in inputs:
            out_file_path = utilsfunc.get_out_file_path(
                input_file_path=json_file_path,
                file_extension=self._out_stage,
                out_dir=out_dir,
            )
            self._oc_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
            )
            outputs.append(out_file_path)
        return outputs

    def _oc_csvwriter(self, file_path: str, output_file_path: str) -> None:

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        spec_IRI = data[params.SPECIES_IRI]
        calc_id = data[params.ENTRY_UUID]
        entryIRI = data[params.ENTRY_IRI]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            for prefix_name in self._handler_prefixes._parameters:
                prefix_value = self.get_prefix_value(name=prefix_name)
                if prefix_value is not None:
                    writer.register_prefix(name=prefix_name, value=prefix_value)

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
            self._write_orb_en(writer, entryIRI, calc_id, data)
            self._write_geom_opt(writer, entryIRI, calc_id, data)
            self._write_atom_info(writer, calc_id, data)
            self._write_metadata(writer, calc_id, data)

    def _write_initial(self, writer: Abox_Writer, jobIRI, calc_id, spec_IRI):

        # This is all the initialization part of the ABox
        abox_name = "ABoxOntoCompChem"
        init_mod_iri = f"comp_pref:InitializationModule_{calc_id}"
        writer.write_imports(name=abox_name, importing="onto_comp:").add_imports(
            importing="comp_pref_no_slash:", rel="base"
        )
        writer.write_inst(iri=jobIRI, type="onto_comp:" + "#G09")
        if spec_IRI:  # If you have the ontospecies IRI, it puts it here.
            # Otherwise, it leaves it out.
            writer.write_inst(iri=spec_IRI, type="inst_spec:").add_obj_prop(
                iri=jobIRI,
                rel="has_spec:",
            )
        # Sets up initialization.
        writer.write_inst(
            iri=f"{init_mod_iri}", type="onto_comp:#InitializationModule"
        ).add_obj_prop(iri=jobIRI, rel="onto_comp:#hasInitialization")

        writer.write_inst(
            iri=f"comp_pref:SourcePackage_{calc_id}_EnvironmentModule",
            type="gain_pref:SourcePackage",
        ).add_obj_prop(
            iri=jobIRI,
            rel="onto_comp:#hasEnvironment",
        )
        writer.write_inst(
            iri=f"comp_pref:MoleculeProperty_{calc_id}",
            type="gain_pref:MoleculeProperty",
        ).add_obj_prop(
            iri=f"{init_mod_iri}",
            rel="gain_pref:hasMoleculeProperty",
        )

    def _write_mols(self, writer: Abox_Writer, calc_id, data):
        # This section starts the representation of the molecule, namely dividing
        # the species into sub-molecules that contain the different atom types.
        # This will hopefully be changed by an update in OntoCompChem later.

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
                iri=f"comp_pref:Molecule_{calc_id}_{atom}{count}",
                type="gain_pref:Molecule",
            ).add_obj_prop(
                iri=f"comp_pref:MoleculeProperty_{calc_id}_{atom}{count}",
                rel="gain_pref:hasMolecule",
            )
            writer.write_inst(
                iri=f"comp_pref:Atom_{calc_id}_{atom}{count}",
                type="gain_pref:Atom",
            ).add_obj_prop(
                iri=f"comp_pref:Molecule_{calc_id}_{atom}{count}",
                rel="gain_pref:hasAtom",
            )
            writer.write_inst(
                iri=f"table_pref:#{atom}", type="table_pref:#Element"
            ).add_obj_prop(
                iri=f"comp_pref:Atom_{calc_id}_{atom}{count}",
                rel="gain_pref:isElement",
                store_inst=True,
            ).add_data_prop(
                rel="gain_pref:hasNumberOfAtoms",
                value=at_count[k][1],
                data_type="Integer",
            )

    def _write_level_of_theory(self, writer: Abox_Writer, calc_id, data):
        # This section writes the information related to the level
        # of theory for the ABox (method and basis set).

        init_mod_iri = f"comp_pref:InitializationModule_{calc_id}"
        lvl_theory = "LevelofTheoryParameter"
        meth_iri = f"comp_pref:MethodologyFeature_{calc_id}_{lvl_theory}"

        writer.write_inst(
            iri=f"comp_pref:LevelOfTheory_{calc_id}",
            type="onto_comp:#LevelOfTheory",
        )
        writer.write_inst(
            iri=f"{meth_iri}", type="gain_pref:MethodologyFeature"
        ).add_obj_prop(
            iri=f"{init_mod_iri}",
            rel="gain_pref:hasParameter",
        ).add_data_prop(
            rel="onto_comp:#hasLevelOfTheory",
            value=data["Method"],
        )
        writer.write_inst(
            iri=f"comp_pref:BasisSet_{calc_id}", type="gain_pref:BasisSet"
        ).add_obj_prop(
            iri=f"{init_mod_iri}",
            rel="gain_pref:hasParameter",
        ).add_data_prop(
            rel="gain_pref:hasBasisSet",
            value=f'"{data["Basis set"]}"',
        )  # Note that the string formatting is used to escape the ',' in basis sets.

    def _write_name(self, writer: Abox_Writer, calc_id, data):
        # This writes the name of the species, taken as the formula,
        # but with extraneous 1s removed.

        writer.write_data_prop(
            iri=f"comp_pref:MoleculeProperty_{calc_id}",
            rel="gain_pref:hasName",
            value=utilsfunc.formula_clean_re.sub("", data["Empirical formula"]),
        )

    def _write_frequencies(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the vibrations to the ABox (if they exist).

        if "Frequencies" in data:
            writer.write_inst(
                iri=f"comp_pref:VibrationalAnalysis_{calc_id}",
                type="gain_pref:VibrationalAnalysis",
            ).add_obj_prop(
                iri=jobIRI,
                rel="gain_pref:isCalculationOn",
            )
            writer.write_inst(
                iri=f"comp_pref:Frequency_{calc_id}",
                type="gain_pref:Frequency",
            ).add_obj_prop(
                iri=f"comp_pref:VibrationalAnalysis_{calc_id}",
                rel="gain_pref:hasResult",
            ).add_data_prop(
                rel="onto_comp:#hasFrequencies",
                value=" ".join(str(i) for i in data["Frequencies"]),
            ).add_data_prop(
                rel="gain_pref:hasVibrationCount",
                value=data["Frequencies number"],
            )
            writer.write_inst(
                iri="gain_pref:cm-1", type="unit_pref:qudt#FrequencyUnit"
            ).add_obj_prop(
                iri=f"comp_pref:Frequency_{calc_id}",
                rel="gain_pref:hasUnit",
            )

    def _write_rotations(self, writer: Abox_Writer, jobIRI, calc_id, data):

        if "Rotational constants" in data:
            # This section writes the rotational constants information
            # - rotational symmetry, rotational constants, and their values/units.

            writer.write_inst(
                iri=f"comp_pref:RotationalConstants_{calc_id}",
                type="onto_comp:#RotationalConstants",
            ).add_obj_prop(iri=jobIRI, rel="gain_pref:isCalculationOn",).add_data_prop(
                rel="onto_comp:#hasRotationalConstants",
                value=" ".join(str(i) for i in data["Rotational constants"]),
            ).add_data_prop(
                rel="onto_comp:#hasRotationalConstantsCount",
                value=data["Rotational constants number"],
            ).add_obj_prop(
                iri="unit_pref:unit#GigaHertz",
                rel="gain_pref:hasUnit",
                reverse=True,
            )

        if "Rotational symmetry number" in data:
            writer.write_inst(
                iri=f"comp_pref:RotationalSymmetry_{calc_id}",
                type="onto_comp:#RotationalSymmetry",
            ).add_obj_prop(iri=jobIRI, rel="gain_pref:isCalculationOn",).add_data_prop(
                rel="onto_comp:#hasRotationalSymmetryNumber",
                value=str(int(data["Rotational symmetry number"])),
            )

    def _write_geom_type(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the geometry type information.

        writer.write_inst(
            iri=f"comp_pref:GeometryType_{calc_id}",
            type="onto_comp:#GeometryType",
        ).add_obj_prop(iri=jobIRI, rel="gain_pref:isCalculationOn",).add_data_prop(
            rel="onto_comp:#hasGeometryType",
            value=data["Geometry type"],
        )

    def _write_zpe(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the zero-point energy information (if it exists).
        # Note that this requires a frequency calculation to be computed.

        if "Electronic and ZPE energy" in data and "Electronic energy" in data:
            writer.write_inst(
                iri=f"comp_pref:ZeroPointEnergy_{calc_id}",
                type="onto_comp:#ZeroPointEnergy",
            ).add_obj_prop(
                iri=jobIRI,
                rel="gain_pref:isCalculationOn",
            )
            writer.write_inst(
                iri=f"comp_pref:FloatValue_{calc_id}_ZeroPointEnergy",
                type="gain_pref:FloatValue",
            ).add_obj_prop(
                iri=f"comp_pref:ZeroPointEnergy_{calc_id}",
                rel="gain_pref:hasElectronicEnergy",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data["Electronic and ZPE energy"] - data["Electronic energy"],
            ).add_obj_prop(
                iri="unit_pref:unit#Hartree", rel="gain_pref:hasUnit", reverse=True
            )
        elif "Electronic and ZPE energy" in data:
            writer.write_inst(
                iri=f"comp_pref:ElectronicAndZPEEnergy_{calc_id}",
                type="onto_comp:#ElectronicAndZPEEnergy",
            ).add_obj_prop(
                iri=jobIRI,
                rel="gain_pref:isCalculationOn",
            )
            writer.write_inst(
                iri=f"comp_pref:FloatValue_{calc_id}_ElectronicAndZPEEnergy",
                type="gain_pref:FloatValue",
            ).add_obj_prop(
                iri=f"comp_pref:ElectronicAndZPEEnergy_{calc_id}",
                rel="gain_pref:hasElectronicEnergy",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data["Electronic and ZPE energy"],
            ).add_obj_prop(
                iri="unit_pref:unit#Hartree", rel="gain_pref:hasUnit", reverse=True
            )

    def _write_scf(self, writer: Abox_Writer, jobIRI, calc_id, data):

        if "Electronic energy" in data:
            # This section writes the electronic (SCF) energy information.
            writer.write_inst(
                iri=f"comp_pref:ScfEnergy_{calc_id}",
                type="onto_comp:#ScfEnergy",
            ).add_obj_prop(
                iri=jobIRI,
                rel="gain_pref:isCalculationOn",
            )
            writer.write_inst(
                iri=f"comp_pref:FloatValue_{calc_id}_ScfEnergy",
                type="gain_pref:FloatValue",
            ).add_obj_prop(
                iri=f"comp_pref:ScfEnergy_{calc_id}",
                rel="gain_pref:hasElectronicEnergy",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data["Electronic energy"],
            ).add_obj_prop(
                iri="unit_pref:unit#Hartree", rel="gain_pref:hasUnit", reverse=True
            )

    def _write_orb_en(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the information on the orbital energies:
        # HOMO, HOMO-1, HOMO-2
        # LUMO, LUMO+1, LUMO+2  energies.

        orbitals_en_keys = {
            "HOMO energy": {"type": "HomoEnergy", "predicate": "hasHomoEnergy"},
            "HOMO-1 energy": {
                "type": "HomoMinusOneEnergy",
                "predicate": "hasHomoMinusOneEnergy",
            },
            "HOMO-2 energy": {
                "type": "HomoMinusTwoEnergy",
                "predicate": "hasHomoMinusTwoEnergy",
            },
            "LUMO energy": {"type": "LumoEnergy", "predicate": "hasLumoEnergy"},
            "LUMO+1 energy": {
                "type": "LumoPlusOneEnergy",
                "predicate": "hasLumoPlusOneEnergy",
            },
            "LUMO+2 energy": {
                "type": "LumoPlusTwoEnergy",
                "predicate": "hasLumoPlusTwoEnergy",
            },
        }

        for orb_en_data, orb_ont in orbitals_en_keys.items():
            if orb_en_data not in data:
                continue

            orb_en_type = orb_ont["type"]
            orb_en_pred = orb_ont["predicate"]

            writer.write_inst(
                iri=f"comp_pref:{orb_en_type}_{calc_id}",
                type=f"onto_comp:#{orb_en_type}",
            ).add_obj_prop(
                iri=jobIRI,
                rel="gain_pref:isCalculationOn",
            )
            writer.write_inst(
                iri=f"comp_pref:FloatValue_{calc_id}_{orb_en_type}",
                type="gain_pref:FloatValue",
            ).add_obj_prop(
                iri=f"comp_pref:{orb_en_type}_{calc_id}",
                rel=f"onto_comp:#{orb_en_pred}",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data[orb_en_data],
            ).add_obj_prop(
                iri="unit_pref:unit#Hartree", rel="gain_pref:hasUnit", reverse=True
            )

    def _write_geom_opt(self, writer: Abox_Writer, jobIRI, calc_id, data):
        # This section writes the geometry optimization, spin multiplicity
        # and formal charge information.

        writer.write_inst(
            iri=f"comp_pref:GeometryOptimization_{calc_id}",
            type="gain_pref:GeometryOptimization",
        ).add_obj_prop(
            iri=jobIRI,
            rel="gain_pref:isCalculationOn",
        )
        writer.write_inst(
            iri=f"comp_pref:Molecule_{calc_id}", type="gain_pref:Molecule"
        ).add_obj_prop(
            iri=f"comp_pref:GeometryOptimization_{calc_id}",
            rel="gain_pref:hasMolecule",
        ).add_data_prop(
            rel="onto_comp:#hasSpinMultiplicity",
            value=data["Spin multiplicity"],
        )
        writer.write_inst(
            iri=f"comp_pref:IntegerValue_{calc_id}_FormalCharge",
            type="gain_pref:IntegerValue",
        ).add_obj_prop(
            iri=f"comp_pref:Molecule_{calc_id}",
            rel="gain_pref:hasFormalCharge",
        ).add_data_prop(
            rel="gain_pref:hasValue",
            value=data["Formal charge"],
            data_type="Integer",
        ).add_obj_prop(
            iri="gain_pref:atomicUnit", rel="gain_pref:hasUnit", reverse=True
        )

    def _write_atom_info(self, writer: Abox_Writer, calc_id, data):
        # This section writes the atom coordinates and masses information.

        count = 1  # This count essentially counts the indices of the atoms starting
        # with 1. Basically, this additional number helps uniquely assign an IRI
        # to each atom.
        coord_string = ["x3", "y3", "z3"]  # How the coordinates are labeled.
        coords = ["X", "Y", "Z"]  # The three cartesian corrdinates.
        # Coordinates.
        for k in range(len(data["Atom types"])):

            atom = data["Atom types"][k]
            atom_id = f"{calc_id}_{atom}{count}"

            writer.write_inst(
                iri=f"comp_pref:Atom_{atom_id}", type="gain_pref:Atom"
            ).add_obj_prop(
                iri=f"comp_pref:Molecule_{calc_id}",
                rel="gain_pref:hasAtom",
            ).add_obj_prop(
                iri=f"table_pref:#{atom}", rel="gain_pref:isElement", reverse=True
            )
            for i in range(3):
                writer.write_inst(
                    iri=(
                        f"comp_pref:FloatValue_{atom_id}_"
                        f"{coord_string[i]}Coordinate"
                    ),
                    type="gain_pref:FloatValue",
                ).add_obj_prop(
                    iri=f"comp_pref:Atom_{calc_id}_{atom}{count}",
                    rel=f"gain_pref:hasAtomCoordinate{coords[i]}",
                ).add_data_prop(
                    rel="gain_pref:hasValue",
                    value=data["Geometry"][k][i],
                )
            # Write atom masses.
            writer.write_inst(
                iri=f"comp_pref:FloatValue_{atom_id}_Mass",
                type="gain_pref:FloatValue",
            ).add_obj_prop(
                iri=f"comp_pref:Atom_{atom_id}",
                rel="gain_pref:hasMass",
            ).add_data_prop(
                rel="gain_pref:hasValue",
                value=data["Atomic masses"][k],
            ).add_obj_prop(
                iri="unit_pref:unit#Dalton", rel="gain_pref:hasUnit", reverse=True
            )
            count += 1

    def _write_metadata(self, writer: Abox_Writer, calc_id, data):
        # These are the final parts of the ABox with the
        # auxillary info like software used and job run date.
        writer.write_data_prop(
            iri=f"comp_pref:SourcePackage_{calc_id}_EnvironmentModule",
            rel="onto_comp:#hasProgram",
            value=data["Program name"],
        ).add_data_prop(
            rel="onto_comp:#hasProgramVersion",
            value=data["Program version"].split("+")[0][-1],
        ).add_data_prop(
            rel="onto_comp:#hasRunDate",
            value=data["Run date"],
        )

        if CCLOG_SOURCE_LOCATION in data:
            writer.write_obj_prop(
                src_iri=f"comp_pref:SourcePackage_{calc_id}_EnvironmentModule",
                rel="gain_pref:hasOutputFile",
                trg_iri=data[CCLOG_SOURCE_LOCATION],
            )

        if PNG_SOURCE_LOCATION in data:
            writer.write_obj_prop(
                src_iri=f"comp_pref:SourcePackage_{calc_id}_EnvironmentModule",
                rel="gain_pref:hasOutputFile",
                trg_iri=data[PNG_SOURCE_LOCATION],
            )

        if XML_SOURCE_LOCATION in data:
            writer.write_obj_prop(
                src_iri=f"comp_pref:SourcePackage_{calc_id}_EnvironmentModule",
                rel="gain_pref:hasOutputFile",
                trg_iri=data[PNG_SOURCE_LOCATION],
            )
