# -*- coding: utf-8 -*-
"""
Created on Thu Mar  4 16:10:02 2021

@author: angir
"""

import json
import chemaboxwriters.kgoperations.querytemplates as qtmpl
import chemaboxwriters.common.globals as globals
import chemaboxwriters.common.utilsfunc as utilsfunc
from chemaboxwriters.common.handler import Handler
from typing import List, Optional, Dict
import chemaboxwriters.common.aboxconfig as abconf
import chemaboxwriters.common.endpoints_proxy as endp
from enum import Enum
import logging

logger = logging.getLogger(__name__)


class OM_JSON_TO_OM_CSV_Handler(Handler):
    """Handler converting ontomops om_json files to om_csv.
    Inputs: List of om_json file paths
    Outputs: List of om_csv file paths
    """

    def __init__(
        self,
        endpoints_proxy: Optional[endp.Endpoints_proxy] = None,
    ) -> None:
        super().__init__(
            name="OM_JSON_TO_OM_CSV",
            in_stage=globals.aboxStages.OM_JSON,
            out_stage=globals.aboxStages.OM_CSV,
            endpoints_proxy=endpoints_proxy,
            required_configs={
                abconf.WRITERS_PREFIXES_KEY: [
                    "onto_spec",
                    "onto_mops",
                    "mops_pref",
                    "rdf_pref",
                    "uom_pref",
                    "unres_pref",
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
            self._om_csvwriter(
                file_path=json_file_path,
                output_file_path=out_file_path,
                **self._handler_kwargs,
            )
            outputs.append(out_file_path)
        return outputs

    def _om_csvwriter(
        self, file_path: str, output_file_path: str, *args, **kwargs
    ) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        rdf_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["rdf_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        uom_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["uom_pref"]
        unres_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unres_pref"]

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[globals.ENTRY_UUID]
        mops_id = data[globals.ENTRY_IRI]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:
            assemblymodel = None
            query_endpoints = self.endpoints_config.get(abconf.QUERY_SETTINGS_KEY, {})
            omops_query_endpoint = query_endpoints.get(abconf.OMOPS_QUERY_ENDPOINT_KEY)
            if omops_query_endpoint is None:
                logger.warning(
                    (
                        "Couldn't query for the assembly model IRI, The query "
                        "endpoint not specified in the aboxwriters config file."
                    )
                )
            else:
                search1 = qtmpl.get_assembly_iri(
                    omops_query_endpoint,
                    data["Mops_Chemical_Building_Units"][0]["GenericUnitModularity"],
                    data["Mops_Chemical_Building_Units"][0]["GenericUnitPlanarity"],
                    data["Mops_Chemical_Building_Units"][0]["GenericUnitNumber"],
                    data["Mops_Symmetry_Point_Group"],
                )

                search2 = qtmpl.get_assembly_iri(
                    omops_query_endpoint,
                    data["Mops_Chemical_Building_Units"][1]["GenericUnitModularity"],
                    data["Mops_Chemical_Building_Units"][1]["GenericUnitPlanarity"],
                    data["Mops_Chemical_Building_Units"][1]["GenericUnitNumber"],
                    data["Mops_Symmetry_Point_Group"],
                )
                if search1 and search2:
                    assemblymodel = list(set(search1).intersection(search2))[0]

            if assemblymodel is None:
                asmodel_uuid = gen_id
            else:
                asmodel_uuid = assemblymodel.split("_")[-1]

            writer.write_header()

            # Write the main initialization for now.
            writer.write_imports(
                name="ABoxOntoMOPs",
                importing=onto_mops,
                rel="http://www.w3.org/2002/07/owl#imports",
            )
            writer.write_imports(name="ABoxOntoMOPs", importing=mops_pref, rel="base")
            writer.write_inst(
                iri=f"{mops_pref}{mops_id}",
                type=f"{onto_mops}#MetalOrganicPolyhedra",
            )

            # Write the properties directly connected to MOPS instance
            # that then terminate.
            writer.write_data_prop(
                iri=f"{mops_pref}{mops_id}",
                rel=f"{rdf_pref}#label",
                value=data["Mops_Label"],
            )  # label for the MOP
            writer.write_data_prop(
                iri=f"{mops_pref}{mops_id}",
                rel=f"{onto_mops}#hasMOPFormula",
                value=data["Mops_Formula"],
            )  # Chemical formula for the MOP
            writer.write_data_prop(
                iri=f"{mops_pref}{mops_id}",
                rel=f"{onto_mops}#hasCCDCNumber",
                value=data["Mops_CCDC_Number"],
            )  # CCDC No. for the MOP
            #
            #  XYZ Geometry in string form for the MOP.

            # Write the Provenance of the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}Provenance_{gen_id}",
                type=f"{onto_mops}#Provenance",
            )  # Initialize the Provenance object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}{mops_id}",
                trg_iri=f"{mops_pref}Provenance_{gen_id}",
                rel=f"{onto_mops}#hasProvenance",
            )  # Connect the Provenance to the MOPs instance.
            writer.write_data_prop(
                iri=f"{mops_pref}Provenance_{gen_id}",
                rel=f"{onto_mops}#hasReferenceDOI",
                value=data["Mops_Reference_DOI"],
            )

            # Write the Molecular Weight section for the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}MolecularWeight_{gen_id}",
                type=f"{onto_spec}#MolecularWeight",
            )  # Initialize the Molecular Weight object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}{mops_id}",
                trg_iri=f"{mops_pref}MolecularWeight_{gen_id}",
                rel=f"{onto_spec}#hasMolecularWeight",
            )  # Link the Molecular Weight object to the MOPs.
            writer.write_inst(
                iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                type=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Molecular Weight.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}MolecularWeight_{gen_id}",
                trg_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                rel=f"{uom_pref}hasValue",
            )  # Link the Measure to the Molecular Weight instance.
            writer.write_data_prop(
                iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                rel=f"{uom_pref}hasNumericalValue",
                value=data["Mops_Molecular_Weight"],
            )  # Link the Numerical Value of Molecular Weight to the Measure.
            writer.write_inst(
                iri=f"{uom_pref}MolarMassUnit", type=f"{uom_pref}Unit"
            )  # Take the MolarMass Unit instance from the UOM ontology.
            writer.write_obj_prop(
                src_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                trg_iri=f"{uom_pref}MolarMassUnit",
                rel=f"{uom_pref}hasUnit",
            )

            # Write the Charge section for the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}Charge_{gen_id}", type=f"{onto_spec}#Charge"
            )  # Initialize the Charge object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}{mops_id}",
                trg_iri=f"{mops_pref}Charge_{gen_id}",
                rel=f"{onto_spec}#hasCharge",
            )  # Link the Charge object to the MOPs.
            writer.write_inst(
                iri=f"{uom_pref}Measure_Charge_{gen_id}",
                type=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Charge.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}Charge_{gen_id}",
                trg_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                rel=f"{uom_pref}hasValue",
            )  # Link the Measure to the Charge instance.
            writer.write_data_prop(
                iri=f"{uom_pref}Measure_Charge_{gen_id}",
                rel=f"{uom_pref}hasNumericalValue",
                value=data["Mops_Charge"],
            )  # Link the Numerical Value of Charge to the Measure.
            writer.write_inst(
                iri=f"{unres_pref}elementary_charge", type=f"{uom_pref}Unit"
            )  # Take the elementary charge Unit instance from our extension
            # of the UOM ontology.
            writer.write_obj_prop(
                src_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                trg_iri=f"{unres_pref}elementary_charge",
                rel=f"{uom_pref}hasUnit",
            )

            # Write the Cavity section for the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}Cavity_{gen_id}", type=f"{onto_mops}#Cavity"
            )  # Initialize the Cavity object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}{mops_id}",
                trg_iri=f"{mops_pref}Cavity_{gen_id}",
                rel=f"{onto_mops}#hasCavity",
            )  # Link the Cavity object to the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}Volume_{gen_id}", type=f"{onto_mops}#Volume"
            )  # Initialize the Volume object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}Cavity_{gen_id}",
                trg_iri=f"{mops_pref}Volume_{gen_id}",
                rel=f"{onto_mops}#hasMOPCavityVolume",
            )  # Link the Volume object to the cavity.
            writer.write_inst(
                iri=f"{uom_pref}Measure_Volume_{gen_id}",
                type=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Volume.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}Volume_{gen_id}",
                trg_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                rel=f"{uom_pref}hasValue",
            )  # Link the Measure to the Volume instance.
            writer.write_data_prop(
                iri=f"{uom_pref}Measure_Volume_{gen_id}",
                rel=f"{uom_pref}hasNumericalValue",
                value=data["Mops_CavityVolume"],
            )  # Link the Numerical Value of Volume to the Measure.
            writer.write_inst(
                iri=f"{uom_pref}cubicNanometre", type=f"{uom_pref}Unit"
            )  # Take the Cubic Nanometre Unit instance from the UOM ontology.
            writer.write_obj_prop(
                src_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                trg_iri=f"{uom_pref}cubicNanometre",
                rel=f"{uom_pref}hasUnit",
            )

            # Write the Assembly Model initialization and shape/symmetry
            # related instances.
            writer.write_inst(
                iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                type=f"{onto_mops}#AssemblyModel",
            )  # Initialize the Assembly Model object for the MOPs.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}{mops_id}",
                trg_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                rel=f"{onto_mops}#hasAssemblyModel",
            )  # Connect the MOPs instance to the Assembly Model instance.
            writer.write_data_prop(
                iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                rel=f"{onto_mops}#hasSymmetryPointGroup",
                value=data["Mops_Symmetry_Point_Group"],
            )  # Write the Symmetry point group for the MOPs.
            writer.write_inst(
                iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                type=f"{onto_mops}#{data['Mops_Polyhedral_Shape']}",
            )  # Initialize an instance of Polyhedral Shape that is the given
            # shape from the JSON file.
            writer.write_obj_prop(
                src_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                trg_iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                rel=f"{onto_mops}#hasPolyhedralShape",
            )  # Connect the Assembly model to polyhedral shape.
            writer.write_data_prop(
                iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                rel=f"{onto_mops}#hasSymbol",
                value=data["Mops_Polyhedral_Shape_Symbol"],
            )

            # Write the information about the Chemical and Generic Building units.
            for i in range(
                len(data["Mops_Chemical_Building_Units"])
            ):  # We will loop through all the building units in the JSON.

                cbu_i = data["Mops_Chemical_Building_Units"][i]

                writer.write_inst(
                    iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    type=f"{onto_mops}#ChemicalBuildingUnit",
                )  # Instantiate the Chemical Building Unit.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}{mops_id}",
                    trg_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasChemicalBuildingUnit",
                )  # Connect the CBU instance to the MOPs instance.
                writer.write_data_prop(
                    iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasCBUFormula",
                    value=cbu_i["CBU_Formula"],
                )  # CBU Formula
                bind_dir = "BindingDirection"
                writer.write_inst(
                    iri=f"{mops_pref}{cbu_i[bind_dir]}Binding_{gen_id}",
                    type=f"{onto_mops}#{cbu_i[bind_dir]}Binding",
                )  # Instantiate the binding direction for the CBU
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}{cbu_i[bind_dir]}Binding_{gen_id}",
                    rel=f"{onto_mops}#hasBindingDirection",
                )
                # Connect Binding direction instance to CBU instance.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{cbu_i['OntoSpecies_IRI']}",
                    rel=f"{onto_spec}#hasUniqueSpecies",
                )  # Connect CBU to OntoSpecies entry.

                writer.write_inst(
                    iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    type=f"{onto_mops}#{cbu_i['Binding_Site']}Site",
                )  # Instantiate the binding site for the CBU.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    rel=f"{onto_mops}#hasBindingSite",
                )
                # Connect Binding site instance to CBU instance.
                writer.write_data_prop(
                    iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    rel=f"{rdf_pref}#label",
                    value=f"{cbu_i['Binding_Site_Label']}",
                )  # label for the Binding Site.
                writer.write_data_prop(
                    iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    rel=f"{onto_mops}#hasOuterCoordinationNumber",
                    value=f"{cbu_i['Binding_SiteCoordNumber']}",
                )

                writer.write_inst(
                    iri=f"{mops_pref}Core_{gen_id}_{i}",
                    type=f"{onto_mops}#Core",
                )
                # Instantiate the Core for this CBU.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasCore",
                )  # Connect the Core instance to the CBU instance.
                writer.write_data_prop(
                    iri=f"{mops_pref}Core_{gen_id}_{i}",
                    rel=f"{rdf_pref}#label",
                    value=f"{cbu_i['CoreLabel']}",
                )  # Attach label to Core.
                writer.write_inst(
                    iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    type=f"{onto_mops}#Substituent",
                )  # Instantiate the Core Substituent.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasSubstituent",
                )  # Connect the Core Substituent to the Core.
                writer.write_data_prop(
                    iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    rel=f"{rdf_pref}#label",
                    value=f"{cbu_i['CoreSubstituentLabel']}",
                )  # Attach label to Core Substituent.

                writer.write_inst(
                    iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    type=f"{onto_mops}#Spacer",
                )
                # Instantiate the Spacer for this CBU.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasSpacer",
                )  # Connect the Spacer instance to the CBU instance.
                writer.write_data_prop(
                    iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    rel=f"{rdf_pref}#label",
                    value=f"{cbu_i['SpacerLabel']}",
                )  # Attach label to Spacer.
                writer.write_inst(
                    iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    type=f"{onto_mops}#Substituent",
                )  # Instantiate the Spacer Substituent.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    rel=f"{onto_mops}#hasSubstituent",
                )  # Connect the Spacer Substituent to the Core.
                writer.write_data_prop(
                    iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    rel=f"{rdf_pref}#label",
                    value=f"{cbu_i['SpacerSubstituentLabel']}",
                )  # Attach label to Spacer Substituent.

                gbu = "GenericBuildingUnit"

                writer.write_inst(
                    iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    type=f"{onto_mops}#{gbu}",
                )  # Instantiate the corresponding Generic Building Unit.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                    trg_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#has{gbu}",
                )  # Connect the GBU instance to the Assembly Model instance.
                writer.write_data_prop(
                    iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#hasPlanarity",
                    value=f"{cbu_i['GenericUnitPlanarity']}",
                )  # Planarity of GBU.
                writer.write_data_prop(
                    iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#hasModularity",
                    value=f"{cbu_i['GenericUnitModularity']}",
                )  # Modularity of GBU.
                writer.write_inst(
                    iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    type=f"{onto_mops}#{gbu}Number",
                )  # Instantiate the corresponding Generic Building Unit Number.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                    trg_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#has{gbu}Number",
                )  # Connect the GBU Number instance to the Assembly Model instance.
                writer.write_obj_prop(
                    src_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    trg_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#isNumberOf",
                )  # Connect the GBU Number to its GBU.
                writer.write_data_prop(
                    iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    rel=f"{onto_spec}#value",
                    value=f"{cbu_i['GenericUnitNumber']}",
                )  # Give the GBU Number its value.

                writer.write_obj_prop(
                    src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    rel=f"{onto_mops}#isFunctioningAs",
                )  # Connect the CBU to its corresonding GBU
