# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import chemaboxwriters.kgoperations.querytemplates as qtmpl
import chemaboxwriters.common.params as params
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.handler import Handler
from typing import List
from chemaboxwriters.ontomops.abox_stages import OM_ABOX_STAGES
import logging

logger = logging.getLogger(__name__)

Abox_Writer = utilsfunc.Abox_csv_writer


HANDLER_PREFIXES = {
    "onto_spec": {"required": True},
    "onto_mops": {"required": True},
    "mops_pref": {"required": True},
    "rdf_pref": {"required": True},
    "uom_pref": {"required": True},
    "unres_pref": {"required": True},
}


class OM_JSON_TO_OM_CSV_Handler(Handler):
    """Handler converting ontomops om_json files to om_csv.
    Inputs: List of om_json file paths
    Outputs: List of om_csv file paths
    """

    def __init__(self) -> None:

        super().__init__(
            name="OM_JSON_TO_OM_CSV",
            in_stage=OM_ABOX_STAGES.om_json,  # type: ignore
            out_stage=OM_ABOX_STAGES.om_csv,  # type: ignore
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
            self._om_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
            )
            outputs.append(out_file_path)
        return outputs

    def _om_csvwriter(self, file_path: str, output_file_path) -> None:

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[params.ENTRY_UUID]
        mops_id = data[params.ENTRY_IRI]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            for prefix_name in self._handler_prefixes._parameters:
                prefix_value = self.get_prefix_value(name=prefix_name)
                if prefix_value is not None:
                    writer.register_prefix(name=prefix_name, value=prefix_value)

            self._write_initial(writer, gen_id, mops_id, data)
            self._write_provenance(writer, gen_id, mops_id, data)
            self._write_mol_weight(writer, gen_id, mops_id, data)
            self._write_charge(writer, gen_id, mops_id, data)
            self._write_cavity(writer, gen_id, mops_id, data)
            asmodel_uuid = self._get_assembly_model_uuid(gen_id, data)
            self._write_assembly_model(writer, gen_id, asmodel_uuid, mops_id, data)
            self._write_cbu_gbu(writer, gen_id, asmodel_uuid, mops_id, data)

    def _write_initial(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        writer.write_header()

        # Write the main initialization for now.
        writer.write_imports(name="ABoxOntoMOPs", importing="onto_mops:").add_imports(
            importing="mops_pref:", rel="base"
        )

        writer.write_inst(
            iri=f"mops_pref:{mops_id}",
            type="onto_mops:#MetalOrganicPolyhedra",
        ).add_data_prop(  # label for the MOP
            rel="rdf_pref:#label",
            value=data["Mops_Label"],
        ).add_data_prop(  # Chemical formula for the MOP
            rel="onto_mops:#hasMOPFormula",
            value=data["Mops_Formula"],
        ).add_data_prop(
            rel="onto_mops:#hasCCDCNumber",
            value=data["Mops_CCDC_Number"],
        )
        if "Mops_xyz_geometry_file_url" in data:
            writer.write_data_prop(
                iri=f"mops_pref:{mops_id}",
                rel="onto_mops:#hasXYZGeometryFile",
                value=data["Mops_xyz_geometry_file_url"],
            )

    def _write_provenance(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        # Write the Provenance of the MOPs.
        writer.write_inst(
            iri=f"mops_pref:Provenance_{gen_id}",
            type="onto_mops:#Provenance",
        ).add_obj_prop(  # Connect the Provenance to the MOPs instance.
            iri=f"mops_pref:{mops_id}",
            rel="onto_mops:#hasProvenance",
        ).add_data_prop(
            rel="onto_mops:#hasReferenceDOI",
            value=data["Mops_Reference_DOI"],
        )

    def _write_mol_weight(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        # Write the Molecular Weight section for the MOPs.
        writer.write_inst(
            iri=f"mops_pref:MolecularWeight_{gen_id}",
            type="onto_spec:#MolecularWeight",
        ).add_obj_prop(
            iri=f"mops_pref:{mops_id}",
            rel="onto_spec:#hasMolecularWeight",
        )  # Link the Molecular Weight object to the MOPs.
        writer.write_inst(
            iri=f"uom_pref:Measure_MolecularWeight_{gen_id}",
            type="uom_pref:Measure",
        ).add_obj_prop(
            iri=f"mops_pref:MolecularWeight_{gen_id}",
            rel="uom_pref:hasValue",
        ).add_data_prop(
            rel="uom_pref:hasNumericalValue",
            value=data["Mops_Molecular_Weight"],
        )  # Link the Numerical Value of Molecular Weight to the Measure.
        writer.write_inst(
            iri="uom_pref:MolarMassUnit", type="uom_pref:Unit"
        ).add_obj_prop(
            iri=f"uom_pref:Measure_MolecularWeight_{gen_id}",
            rel="uom_pref:hasUnit",
        )

    def _write_charge(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        # Write the Charge section for the MOPs.
        writer.write_inst(
            iri=f"mops_pref:Charge_{gen_id}", type="onto_spec:#Charge"
        ).add_obj_prop(
            iri=f"mops_pref:{mops_id}",
            rel="onto_spec:#hasCharge",
        )
        writer.write_inst(
            iri=f"uom_pref:Measure_Charge_{gen_id}",
            type="uom_pref:Measure",
        ).add_obj_prop(
            iri=f"mops_pref:Charge_{gen_id}",
            rel="uom_pref:hasValue",
        ).add_data_prop(
            rel="uom_pref:hasNumericalValue",
            value=data["Mops_Charge"],
        )  # Link the Numerical Value of Charge to the Measure.

        # Take the elementary charge Unit instance from our extension
        # of the UOM ontology.
        writer.write_inst(
            iri="unres_pref:elementary_charge", type="uom_pref:Unit"
        ).add_obj_prop(
            iri=f"uom_pref:Measure_Charge_{gen_id}",
            rel="uom_pref:hasUnit",
        )

    def _write_cavity(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        # Write the Cavity section for the MOPs.
        writer.write_inst(
            iri=f"mops_pref:Cavity_{gen_id}", type="onto_mops:#Cavity"
        ).add_obj_prop(
            iri=f"mops_pref:{mops_id}",
            rel="onto_mops:#hasCavity",
        )
        writer.write_inst(
            iri=f"mops_pref:Volume_{gen_id}", type="onto_mops:#Volume"
        ).add_obj_prop(
            iri=f"mops_pref:Cavity_{gen_id}",
            rel="onto_mops:#hasMOPCavityVolume",
        )  # Link the Volume object to the cavity.
        writer.write_inst(
            iri=f"uom_pref:Measure_Volume_{gen_id}",
            type="uom_pref:Measure",
        ).add_obj_prop(
            iri=f"mops_pref:Volume_{gen_id}",
            rel="uom_pref:hasValue",
        ).add_data_prop(
            rel="uom_pref:hasNumericalValue",
            value=data["Mops_CavityVolume"],
        )  # Link the Numerical Value of Volume to the Measure.
        writer.write_inst(
            iri="uom_pref:cubicNanometre", type="uom_pref:Unit"
        ).add_obj_prop(
            iri=f"uom_pref:Measure_Volume_{gen_id}",
            rel="uom_pref:hasUnit",
        )

    def _get_assembly_model_uuid(self, gen_id, data) -> str:

        assemblymodel = None
        if assemblymodel is None:
            symmetry = data["Mops_Symmetry_Point_Group"]
            gbu_properties = []
            for cbu in data["Mops_Chemical_Building_Units"]:
                gbu_properties.append(
                    {
                        "modularity": cbu["GenericUnitModularity"],
                        "planarity": cbu["GenericUnitPlanarity"],
                        "gbu_number": cbu["GenericUnitNumber"],
                    },
                )

            response = self.do_remote_store_query(
                endpoint_prefix="omops",
                query_str=qtmpl.get_assemblyModel(
                    gbu_properties=gbu_properties, mops_symmetry=symmetry
                ),
            )
            if response:
                assemblymodel = response[0]["AssemblyModel"]

        if assemblymodel is None:
            asmodel_uuid = gen_id
        else:
            asmodel_uuid = assemblymodel.split("_")[-1]

        return asmodel_uuid

    def _write_assembly_model(
        self, writer: Abox_Writer, gen_id, asmodel_uuid, mops_id, data
    ) -> None:

        # Write the Assembly Model initialization and shape/symmetry
        # related instances.
        writer.write_inst(
            iri=f"mops_pref:AssemblyModel_{asmodel_uuid}",
            type="onto_mops:#AssemblyModel",
        ).add_obj_prop(
            iri=f"mops_pref:{mops_id}",
            rel="onto_mops:#hasAssemblyModel",
        ).add_data_prop(
            rel="onto_mops:#hasSymmetryPointGroup",
            value=data["Mops_Symmetry_Point_Group"],
        )  # Write the Symmetry point group for the MOPs.
        writer.write_inst(
            iri=f"mops_pref:{data['Mops_Polyhedral_Shape']}_{gen_id}",
            type=f"onto_mops:#{data['Mops_Polyhedral_Shape']}",
        ).add_obj_prop(
            iri=f"mops_pref:AssemblyModel_{asmodel_uuid}",
            rel="onto_mops:#hasPolyhedralShape",
        ).add_data_prop(
            rel="onto_mops:#hasSymbol",
            value=data["Mops_Polyhedral_Shape_Symbol"],
        )

    def _write_cbu_gbu(
        self, writer: Abox_Writer, gen_id, asmodel_uuid, mops_id, data
    ) -> None:

        # Write the information about the Chemical and Generic Building units.
        for i, cbu_i in enumerate(
            data["Mops_Chemical_Building_Units"]
        ):  # We will loop through all the building units in the JSON.

            writer.write_inst(
                iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                type="onto_mops:#ChemicalBuildingUnit",
            ).add_obj_prop(
                iri=f"mops_pref:{mops_id}",
                rel="onto_mops:#hasChemicalBuildingUnit",
            ).add_data_prop(
                rel="onto_mops:#hasCBUFormula",
                value=cbu_i["CBU_Formula"],
            )  # CBU Formula
            bind_dir = "BindingDirection"
            writer.write_inst(
                iri=f"mops_pref:{cbu_i[bind_dir]}Binding_{gen_id}",
                type=f"onto_mops:#{cbu_i[bind_dir]}Binding",
            ).add_obj_prop(
                iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                rel="onto_mops:#hasBindingDirection",
                store_inst=True,
            ).add_obj_prop(
                iri=f"{cbu_i['OntoSpecies_IRI']}",
                rel="onto_spec:#hasUniqueSpecies",
                reverse=True,
            )  # Connect CBU to OntoSpecies entry.

            writer.write_inst(
                iri=f"mops_pref:{cbu_i['Binding_Site']}Site_{gen_id}",
                type=f"onto_mops:#{cbu_i['Binding_Site']}Site",
            ).add_obj_prop(
                iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                rel="onto_mops:#hasBindingSite",
            ).add_data_prop(
                rel="rdf_pref:#label",
                value=f"{cbu_i['Binding_Site_Label']}",
            ).add_data_prop(
                rel="onto_mops:#hasOuterCoordinationNumber",
                value=f"{cbu_i['Binding_SiteCoordNumber']}",
            )

            writer.write_inst(
                iri=f"mops_pref:Core_{gen_id}_{i}",
                type="onto_mops:#Core",
            ).add_obj_prop(
                iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                rel="onto_mops:#hasCore",
            ).add_data_prop(
                rel="rdf_pref:#label",
                value=f"{cbu_i['CoreLabel']}",
            )  # Attach label to Core.
            writer.write_inst(
                iri=f"mops_pref:Substituent_Core_{gen_id}_{i}",
                type="onto_mops:#Substituent",
            ).add_obj_prop(
                iri=f"mops_pref:Core_{gen_id}_{i}",
                rel="onto_mops:#hasSubstituent",
            ).add_data_prop(
                rel="rdf_pref:#label",
                value=f"{cbu_i['CoreSubstituentLabel']}",
            )  # Attach label to Core Substituent.

            writer.write_inst(
                iri=f"mops_pref:Spacer_{gen_id}_{i}",
                type="onto_mops:#Spacer",
            ).add_obj_prop(
                iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                rel="onto_mops:#hasSpacer",
            ).add_data_prop(
                rel="rdf_pref:#label",
                value=f"{cbu_i['SpacerLabel']}",
            )  # Attach label to Spacer.
            writer.write_inst(
                iri=f"mops_pref:Substituent_Spacer_{gen_id}_{i}",
                type="onto_mops:#Substituent",
            ).add_obj_prop(
                iri=f"mops_pref:Spacer_{gen_id}_{i}",
                rel="onto_mops:#hasSubstituent",
            ).add_data_prop(
                rel="rdf_pref:#label",
                value=f"{cbu_i['SpacerSubstituentLabel']}",
            )  # Attach label to Spacer Substituent.

            gbu = "GenericBuildingUnit"

            writer.write_inst(
                iri=f"mops_pref:{gbu}_{asmodel_uuid}_{i}",
                type=f"onto_mops:#{gbu}",
            ).add_obj_prop(
                iri=f"mops_pref:AssemblyModel_{asmodel_uuid}",
                rel=f"onto_mops:#has{gbu}",
            ).add_data_prop(
                rel="onto_mops:#hasPlanarity",
                value=f"{cbu_i['GenericUnitPlanarity']}",
            ).add_data_prop(
                rel="onto_mops:#hasModularity",
                value=f"{cbu_i['GenericUnitModularity']}",
            )  # Modularity of GBU.
            writer.write_inst(
                iri=f"mops_pref:{gbu}Number_{asmodel_uuid}_{i}",
                type=f"onto_mops:#{gbu}Number",
            ).add_obj_prop(
                iri=f"mops_pref:AssemblyModel_{asmodel_uuid}",
                rel=f"onto_mops:#has{gbu}Number",
            ).add_obj_prop(
                iri=f"mops_pref:{gbu}_{asmodel_uuid}_{i}",
                rel="onto_mops:#isNumberOf",
                reverse=True,
            ).add_data_prop(
                rel="onto_spec:#value",
                value=f"{cbu_i['GenericUnitNumber']}",
            )  # Give the GBU Number its value.

            writer.write_obj_prop(
                src_iri=f"mops_pref:ChemicalBuildingUnit_{gen_id}_{i}",
                trg_iri=f"mops_pref:{gbu}_{asmodel_uuid}_{i}",
                rel="onto_mops:#isFunctioningAs",
            )  # Connect the CBU to its corresonding GBU
