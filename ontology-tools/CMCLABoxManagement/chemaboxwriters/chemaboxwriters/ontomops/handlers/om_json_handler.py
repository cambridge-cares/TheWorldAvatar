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

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as aboxwriter:
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

            aboxwriter.write_header()

            # Write the main initialization for now.
            aboxwriter.write_imports(
                abox_name="ABoxOntoMOPs",
                importing=onto_mops,
                relation="http://www.w3.org/2002/07/owl#imports",
            )
            aboxwriter.write_imports(
                abox_name="ABoxOntoMOPs", importing=mops_pref, relation="base"
            )
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}{mops_id}",
                inst_class=f"{onto_mops}#MetalOrganicPolyhedra",
            )

            # Write the properties directly connected to MOPS instance
            # that then terminate.
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}{mops_id}",
                relation=f"{rdf_pref}#label",
                value=data["Mops_Label"],
                data_type="String",
            )  # label for the MOP
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}{mops_id}",
                relation=f"{onto_mops}#hasMOPFormula",
                value=data["Mops_Formula"],
                data_type="String",
            )  # Chemical formula for the MOP
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}{mops_id}",
                relation=f"{onto_mops}#hasCCDCNumber",
                value=data["Mops_CCDC_Number"],
                data_type="String",
            )  # CCDC No. for the MOP
            #
            #  XYZ Geometry in string form for the MOP.

            # Write the Provenance of the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}Provenance_{gen_id}",
                inst_class=f"{onto_mops}#Provenance",
            )  # Initialize the Provenance object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}{mops_id}",
                trg_inst_iri=f"{mops_pref}Provenance_{gen_id}",
                relation=f"{onto_mops}#hasProvenance",
            )  # Connect the Provenance to the MOPs instance.
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}Provenance_{gen_id}",
                relation=f"{onto_mops}#hasReferenceDOI",
                value=data["Mops_Reference_DOI"],
                data_type="String",
            )

            # Write the Molecular Weight section for the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}MolecularWeight_{gen_id}",
                inst_class=f"{onto_spec}#MolecularWeight",
            )  # Initialize the Molecular Weight object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}{mops_id}",
                trg_inst_iri=f"{mops_pref}MolecularWeight_{gen_id}",
                relation=f"{onto_spec}#hasMolecularWeight",
            )  # Link the Molecular Weight object to the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                inst_class=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Molecular Weight.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}MolecularWeight_{gen_id}",
                trg_inst_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                relation=f"{uom_pref}hasValue",
            )  # Link the Measure to the Molecular Weight instance.
            aboxwriter.write_data_property(
                inst_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                relation=f"{uom_pref}hasNumericalValue",
                value=data["Mops_Molecular_Weight"],
                data_type="String",
            )  # Link the Numerical Value of Molecular Weight to the Measure.
            aboxwriter.write_instance(
                inst_iri=f"{uom_pref}MolarMassUnit", inst_class=f"{uom_pref}Unit"
            )  # Take the MolarMass Unit instance from the UOM ontology.
            aboxwriter.write_object_property(
                src_inst_iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
                trg_inst_iri=f"{uom_pref}MolarMassUnit",
                relation=f"{uom_pref}hasUnit",
            )

            # Write the Charge section for the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}Charge_{gen_id}", inst_class=f"{onto_spec}#Charge"
            )  # Initialize the Charge object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}{mops_id}",
                trg_inst_iri=f"{mops_pref}Charge_{gen_id}",
                relation=f"{onto_spec}#hasCharge",
            )  # Link the Charge object to the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                inst_class=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Charge.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}Charge_{gen_id}",
                trg_inst_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                relation=f"{uom_pref}hasValue",
            )  # Link the Measure to the Charge instance.
            aboxwriter.write_data_property(
                inst_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                relation=f"{uom_pref}hasNumericalValue",
                value=data["Mops_Charge"],
                data_type="String",
            )  # Link the Numerical Value of Charge to the Measure.
            aboxwriter.write_instance(
                inst_iri=f"{unres_pref}elementary_charge", inst_class=f"{uom_pref}Unit"
            )  # Take the elementary charge Unit instance from our extension
            # of the UOM ontology.
            aboxwriter.write_object_property(
                src_inst_iri=f"{uom_pref}Measure_Charge_{gen_id}",
                trg_inst_iri=f"{unres_pref}elementary_charge",
                relation=f"{uom_pref}hasUnit",
            )

            # Write the Cavity section for the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}Cavity_{gen_id}", inst_class=f"{onto_mops}#Cavity"
            )  # Initialize the Cavity object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}{mops_id}",
                trg_inst_iri=f"{mops_pref}Cavity_{gen_id}",
                relation=f"{onto_mops}#hasCavity",
            )  # Link the Cavity object to the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}Volume_{gen_id}", inst_class=f"{onto_mops}#Volume"
            )  # Initialize the Volume object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}Cavity_{gen_id}",
                trg_inst_iri=f"{mops_pref}Volume_{gen_id}",
                relation=f"{onto_mops}#hasMOPCavityVolume",
            )  # Link the Volume object to the cavity.
            aboxwriter.write_instance(
                inst_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                inst_class=f"{uom_pref}Measure",
            )  # This is the Measure from the Ontology of Units of Measure
            # that we will use for Volume.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}Volume_{gen_id}",
                trg_inst_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                relation=f"{uom_pref}hasValue",
            )  # Link the Measure to the Volume instance.
            aboxwriter.write_data_property(
                inst_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                relation=f"{uom_pref}hasNumericalValue",
                value=data["Mops_CavityVolume"],
                data_type="String",
            )  # Link the Numerical Value of Volume to the Measure.
            aboxwriter.write_instance(
                inst_iri=f"{uom_pref}cubicNanometre", inst_class=f"{uom_pref}Unit"
            )  # Take the Cubic Nanometre Unit instance from the UOM ontology.
            aboxwriter.write_object_property(
                src_inst_iri=f"{uom_pref}Measure_Volume_{gen_id}",
                trg_inst_iri=f"{uom_pref}cubicNanometre",
                relation=f"{uom_pref}hasUnit",
            )

            # Write the Assembly Model initialization and shape/symmetry
            # related instances.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                inst_class=f"{onto_mops}#AssemblyModel",
            )  # Initialize the Assembly Model object for the MOPs.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}{mops_id}",
                trg_inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                relation=f"{onto_mops}#hasAssemblyModel",
            )  # Connect the MOPs instance to the Assembly Model instance.
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                relation=f"{onto_mops}#hasSymmetryPointGroup",
                value=data["Mops_Symmetry_Point_Group"],
                data_type="String",
            )  # Write the Symmetry point group for the MOPs.
            aboxwriter.write_instance(
                inst_iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                inst_class=f"{onto_mops}#{data['Mops_Polyhedral_Shape']}",
            )  # Initialize an instance of Polyhedral Shape that is the given
            # shape from the JSON file.
            aboxwriter.write_object_property(
                src_inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                trg_inst_iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                relation=f"{onto_mops}#hasPolyhedralShape",
            )  # Connect the Assembly model to polyhedral shape.
            aboxwriter.write_data_property(
                inst_iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
                relation=f"{onto_mops}#hasSymbol",
                value=data["Mops_Polyhedral_Shape_Symbol"],
                data_type="String",
            )

            # Write the information about the Chemical and Generic Building units.
            for i in range(
                len(data["Mops_Chemical_Building_Units"])
            ):  # We will loop through all the building units in the JSON.

                cbu_i = data["Mops_Chemical_Building_Units"][i]

                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    inst_class=f"{onto_mops}#ChemicalBuildingUnit",
                )  # Instantiate the Chemical Building Unit.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}{mops_id}",
                    trg_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasChemicalBuildingUnit",
                )  # Connect the CBU instance to the MOPs instance.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasCBUFormula",
                    value=cbu_i["CBU_Formula"],
                )  # CBU Formula
                bind_dir = "BindingDirection"
                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}{cbu_i[bind_dir]}Binding_{gen_id}",
                    inst_class=f"{onto_mops}#{cbu_i[bind_dir]}Binding",
                )  # Instantiate the binding direction for the CBU
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}{cbu_i[bind_dir]}Binding_{gen_id}",
                    relation=f"{onto_mops}#hasBindingDirection",
                )
                # Connect Binding direction instance to CBU instance.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{cbu_i['OntoSpecies_IRI']}",
                    relation=f"{onto_spec}#hasUniqueSpecies",
                )  # Connect CBU to OntoSpecies entry.

                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    inst_class=f"{onto_mops}#{cbu_i['Binding_Site']}Site",
                )  # Instantiate the binding site for the CBU.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    relation=f"{onto_mops}#hasBindingSite",
                )
                # Connect Binding site instance to CBU instance.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    relation=f"{rdf_pref}#label",
                    value=f"{cbu_i['Binding_Site_Label']}",
                    data_type="String",
                )  # label for the Binding Site.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                    relation=f"{onto_mops}#hasOuterCoordinationNumber",
                    value=f"{cbu_i['Binding_SiteCoordNumber']}",
                    data_type="String",
                )

                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    inst_class=f"{onto_mops}#Core",
                )
                # Instantiate the Core for this CBU.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasCore",
                )  # Connect the Core instance to the CBU instance.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    relation=f"{rdf_pref}#label",
                    value=f"{cbu_i['CoreLabel']}",
                    data_type="String",
                )  # Attach label to Core.
                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    inst_class=f"{onto_mops}#Substituent",
                )  # Instantiate the Core Substituent.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}Core_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasSubstituent",
                )  # Connect the Core Substituent to the Core.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                    relation=f"{rdf_pref}#label",
                    value=f"{cbu_i['CoreSubstituentLabel']}",
                    data_type="String",
                )  # Attach label to Core Substituent.

                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    inst_class=f"{onto_mops}#Spacer",
                )
                # Instantiate the Spacer for this CBU.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasSpacer",
                )  # Connect the Spacer instance to the CBU instance.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    relation=f"{rdf_pref}#label",
                    value=f"{cbu_i['SpacerLabel']}",
                    data_type="String",
                )  # Attach label to Spacer.
                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    inst_class=f"{onto_mops}#Substituent",
                )  # Instantiate the Spacer Substituent.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    relation=f"{onto_mops}#hasSubstituent",
                )  # Connect the Spacer Substituent to the Core.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                    relation=f"{rdf_pref}#label",
                    value=f"{cbu_i['SpacerSubstituentLabel']}",
                    data_type="String",
                )  # Attach label to Spacer Substituent.

                gbu = "GenericBuildingUnit"

                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    inst_class=f"{onto_mops}#{gbu}",
                )  # Instantiate the corresponding Generic Building Unit.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                    trg_inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#has{gbu}",
                )  # Connect the GBU instance to the Assembly Model instance.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#hasPlanarity",
                    value=f"{cbu_i['GenericUnitPlanarity']}",
                    data_type="String",
                )  # Planarity of GBU.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#hasModularity",
                    value=f"{cbu_i['GenericUnitModularity']}",
                    data_type="String",
                )  # Modularity of GBU.
                aboxwriter.write_instance(
                    inst_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    inst_class=f"{onto_mops}#{gbu}Number",
                )  # Instantiate the corresponding Generic Building Unit Number.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                    trg_inst_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#has{gbu}Number",
                )  # Connect the GBU Number instance to the Assembly Model instance.
                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    trg_inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#isNumberOf",
                )  # Connect the GBU Number to its GBU.
                aboxwriter.write_data_property(
                    inst_iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                    relation=f"{onto_spec}#value",
                    value=f"{cbu_i['GenericUnitNumber']}",
                    data_type="String",
                )  # Give the GBU Number its value.

                aboxwriter.write_object_property(
                    src_inst_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                    trg_inst_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                    relation=f"{onto_mops}#isFunctioningAs",
                )  # Connect the CBU to its corresonding GBU
