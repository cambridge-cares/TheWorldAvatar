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

Abox_Writer = utilsfunc.Abox_csv_writer


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

        with open(file_path, "r") as file_handle:
            data = json.load(file_handle)

        gen_id = data[globals.ENTRY_UUID]
        mops_id = data[globals.ENTRY_IRI]

        with utilsfunc.Abox_csv_writer(file_path=output_file_path) as writer:

            self._write_initial(writer, gen_id, mops_id, data)
            self._write_provenance(writer, gen_id, mops_id, data)
            self._write_mol_weight(writer, gen_id, mops_id, data)
            self._write_charge(writer, gen_id, mops_id, data)
            self._write_cavity(writer, gen_id, mops_id, data)
            asmodel_uuid = self._get_assembly_model_uuid(gen_id, data)
            self._write_assembly_model(writer, gen_id, asmodel_uuid, mops_id, data)
            self._write_cbu_gbu(writer, gen_id, asmodel_uuid, mops_id, data)

    def _write_initial(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        rdf_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["rdf_pref"]

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
        ).add_data_prop(  # label for the MOP
            rel=f"{rdf_pref}#label",
            value=data["Mops_Label"],
        ).add_data_prop(  # Chemical formula for the MOP
            rel=f"{onto_mops}#hasMOPFormula",
            value=data["Mops_Formula"],
        ).add_data_prop(
            rel=f"{onto_mops}#hasCCDCNumber",
            value=data["Mops_CCDC_Number"],
        )  # CCDC No. for the MOP
        #

    def _write_provenance(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]

        # Write the Provenance of the MOPs.
        writer.write_inst(
            iri=f"{mops_pref}Provenance_{gen_id}",
            type=f"{onto_mops}#Provenance",
        ).add_obj_prop(  # Connect the Provenance to the MOPs instance.
            iri=f"{mops_pref}{mops_id}",
            rel=f"{onto_mops}#hasProvenance",
        ).add_data_prop(
            rel=f"{onto_mops}#hasReferenceDOI",
            value=data["Mops_Reference_DOI"],
        )

    def _write_mol_weight(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        uom_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["uom_pref"]

        # Write the Molecular Weight section for the MOPs.
        writer.write_inst(
            iri=f"{mops_pref}MolecularWeight_{gen_id}",
            type=f"{onto_spec}#MolecularWeight",
        ).add_obj_prop(
            iri=f"{mops_pref}{mops_id}",
            rel=f"{onto_spec}#hasMolecularWeight",
        )  # Link the Molecular Weight object to the MOPs.
        writer.write_inst(
            iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
            type=f"{uom_pref}Measure",
        ).add_obj_prop(
            iri=f"{mops_pref}MolecularWeight_{gen_id}",
            rel=f"{uom_pref}hasValue",
        ).add_data_prop(
            rel=f"{uom_pref}hasNumericalValue",
            value=data["Mops_Molecular_Weight"],
        )  # Link the Numerical Value of Molecular Weight to the Measure.
        writer.write_inst(
            iri=f"{uom_pref}MolarMassUnit", type=f"{uom_pref}Unit"
        ).add_obj_prop(
            iri=f"{uom_pref}Measure_MolecularWeight_{gen_id}",
            rel=f"{uom_pref}hasUnit",
        )

    def _write_charge(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]
        uom_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["uom_pref"]
        unres_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["unres_pref"]

        # Write the Charge section for the MOPs.
        writer.write_inst(
            iri=f"{mops_pref}Charge_{gen_id}", type=f"{onto_spec}#Charge"
        ).add_obj_prop(
            iri=f"{mops_pref}{mops_id}",
            rel=f"{onto_spec}#hasCharge",
        )
        writer.write_inst(
            iri=f"{uom_pref}Measure_Charge_{gen_id}",
            type=f"{uom_pref}Measure",
        ).add_obj_prop(
            iri=f"{mops_pref}Charge_{gen_id}",
            rel=f"{uom_pref}hasValue",
        ).add_data_prop(
            rel=f"{uom_pref}hasNumericalValue",
            value=data["Mops_Charge"],
        )  # Link the Numerical Value of Charge to the Measure.

        # Take the elementary charge Unit instance from our extension
        # of the UOM ontology.
        writer.write_inst(
            iri=f"{unres_pref}elementary_charge", type=f"{uom_pref}Unit"
        ).add_obj_prop(
            iri=f"{uom_pref}Measure_Charge_{gen_id}",
            rel=f"{uom_pref}hasUnit",
        )

    def _write_cavity(self, writer: Abox_Writer, gen_id, mops_id, data) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        uom_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["uom_pref"]

        # Write the Cavity section for the MOPs.
        writer.write_inst(
            iri=f"{mops_pref}Cavity_{gen_id}", type=f"{onto_mops}#Cavity"
        ).add_obj_prop(
            iri=f"{mops_pref}{mops_id}",
            rel=f"{onto_mops}#hasCavity",
        )
        writer.write_inst(
            iri=f"{mops_pref}Volume_{gen_id}", type=f"{onto_mops}#Volume"
        ).add_obj_prop(
            iri=f"{mops_pref}Cavity_{gen_id}",
            rel=f"{onto_mops}#hasMOPCavityVolume",
        )  # Link the Volume object to the cavity.
        writer.write_inst(
            iri=f"{uom_pref}Measure_Volume_{gen_id}",
            type=f"{uom_pref}Measure",
        ).add_obj_prop(
            iri=f"{mops_pref}Volume_{gen_id}",
            rel=f"{uom_pref}hasValue",
        ).add_data_prop(
            rel=f"{uom_pref}hasNumericalValue",
            value=data["Mops_CavityVolume"],
        )  # Link the Numerical Value of Volume to the Measure.
        writer.write_inst(
            iri=f"{uom_pref}cubicNanometre", type=f"{uom_pref}Unit"
        ).add_obj_prop(
            iri=f"{uom_pref}Measure_Volume_{gen_id}",
            rel=f"{uom_pref}hasUnit",
        )

    def _get_assembly_model_uuid(self, gen_id, data) -> str:

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

        return asmodel_uuid

    def _write_assembly_model(
        self, writer: Abox_Writer, gen_id, asmodel_uuid, mops_id, data
    ) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]

        # Write the Assembly Model initialization and shape/symmetry
        # related instances.
        writer.write_inst(
            iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
            type=f"{onto_mops}#AssemblyModel",
        ).add_obj_prop(
            iri=f"{mops_pref}{mops_id}",
            rel=f"{onto_mops}#hasAssemblyModel",
        ).add_data_prop(
            rel=f"{onto_mops}#hasSymmetryPointGroup",
            value=data["Mops_Symmetry_Point_Group"],
        )  # Write the Symmetry point group for the MOPs.
        writer.write_inst(
            iri=f"{mops_pref}{data['Mops_Polyhedral_Shape']}_{gen_id}",
            type=f"{onto_mops}#{data['Mops_Polyhedral_Shape']}",
        ).add_obj_prop(
            iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
            rel=f"{onto_mops}#hasPolyhedralShape",
        ).add_data_prop(
            rel=f"{onto_mops}#hasSymbol",
            value=data["Mops_Polyhedral_Shape_Symbol"],
        )

    def _write_cbu_gbu(
        self, writer: Abox_Writer, gen_id, asmodel_uuid, mops_id, data
    ) -> None:

        onto_mops = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_mops"]
        mops_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["mops_pref"]
        rdf_pref = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["rdf_pref"]
        onto_spec = self._endpoints_config[abconf.WRITERS_PREFIXES_KEY]["onto_spec"]

        # Write the information about the Chemical and Generic Building units.
        for i in range(
            len(data["Mops_Chemical_Building_Units"])
        ):  # We will loop through all the building units in the JSON.

            cbu_i = data["Mops_Chemical_Building_Units"][i]

            writer.write_inst(
                iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                type=f"{onto_mops}#ChemicalBuildingUnit",
            ).add_obj_prop(
                iri=f"{mops_pref}{mops_id}",
                rel=f"{onto_mops}#hasChemicalBuildingUnit",
            ).add_data_prop(
                rel=f"{onto_mops}#hasCBUFormula",
                value=cbu_i["CBU_Formula"],
            )  # CBU Formula
            bind_dir = "BindingDirection"
            writer.write_inst(
                iri=f"{mops_pref}{cbu_i[bind_dir]}Binding_{gen_id}",
                type=f"{onto_mops}#{cbu_i[bind_dir]}Binding",
            ).add_obj_prop(
                iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                rel=f"{onto_mops}#hasBindingDirection",
                store_inst=True,
            ).add_obj_prop(
                iri=f"{cbu_i['OntoSpecies_IRI']}",
                rel=f"{onto_spec}#hasUniqueSpecies",
                reverse=True,
            )  # Connect CBU to OntoSpecies entry.

            writer.write_inst(
                iri=f"{mops_pref}{cbu_i['Binding_Site']}Site_{gen_id}",
                type=f"{onto_mops}#{cbu_i['Binding_Site']}Site",
            ).add_obj_prop(
                iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                rel=f"{onto_mops}#hasBindingSite",
            ).add_data_prop(
                rel=f"{rdf_pref}#label",
                value=f"{cbu_i['Binding_Site_Label']}",
            ).add_data_prop(
                rel=f"{onto_mops}#hasOuterCoordinationNumber",
                value=f"{cbu_i['Binding_SiteCoordNumber']}",
            )

            writer.write_inst(
                iri=f"{mops_pref}Core_{gen_id}_{i}",
                type=f"{onto_mops}#Core",
            ).add_obj_prop(
                iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                rel=f"{onto_mops}#hasCore",
            ).add_data_prop(
                rel=f"{rdf_pref}#label",
                value=f"{cbu_i['CoreLabel']}",
            )  # Attach label to Core.
            writer.write_inst(
                iri=f"{mops_pref}Substituent_Core_{gen_id}_{i}",
                type=f"{onto_mops}#Substituent",
            ).add_obj_prop(
                iri=f"{mops_pref}Core_{gen_id}_{i}",
                rel=f"{onto_mops}#hasSubstituent",
            ).add_data_prop(
                rel=f"{rdf_pref}#label",
                value=f"{cbu_i['CoreSubstituentLabel']}",
            )  # Attach label to Core Substituent.

            writer.write_inst(
                iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                type=f"{onto_mops}#Spacer",
            ).add_obj_prop(
                iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                rel=f"{onto_mops}#hasSpacer",
            ).add_data_prop(
                rel=f"{rdf_pref}#label",
                value=f"{cbu_i['SpacerLabel']}",
            )  # Attach label to Spacer.
            writer.write_inst(
                iri=f"{mops_pref}Substituent_Spacer_{gen_id}_{i}",
                type=f"{onto_mops}#Substituent",
            ).add_obj_prop(
                iri=f"{mops_pref}Spacer_{gen_id}_{i}",
                rel=f"{onto_mops}#hasSubstituent",
            ).add_data_prop(
                rel=f"{rdf_pref}#label",
                value=f"{cbu_i['SpacerSubstituentLabel']}",
            )  # Attach label to Spacer Substituent.

            gbu = "GenericBuildingUnit"

            writer.write_inst(
                iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                type=f"{onto_mops}#{gbu}",
            ).add_obj_prop(
                iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                rel=f"{onto_mops}#has{gbu}",
            ).add_data_prop(
                rel=f"{onto_mops}#hasPlanarity",
                value=f"{cbu_i['GenericUnitPlanarity']}",
            ).add_data_prop(
                rel=f"{onto_mops}#hasModularity",
                value=f"{cbu_i['GenericUnitModularity']}",
            )  # Modularity of GBU.
            writer.write_inst(
                iri=f"{mops_pref}{gbu}Number_{asmodel_uuid}_{i}",
                type=f"{onto_mops}#{gbu}Number",
            ).add_obj_prop(
                iri=f"{mops_pref}AssemblyModel_{asmodel_uuid}",
                rel=f"{onto_mops}#has{gbu}Number",
            ).add_obj_prop(
                iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                rel=f"{onto_mops}#isNumberOf",
                reverse=True,
            ).add_data_prop(
                rel=f"{onto_spec}#value",
                value=f"{cbu_i['GenericUnitNumber']}",
            )  # Give the GBU Number its value.

            writer.write_obj_prop(
                src_iri=f"{mops_pref}ChemicalBuildingUnit_{gen_id}_{i}",
                trg_iri=f"{mops_pref}{gbu}_{asmodel_uuid}_{i}",
                rel=f"{onto_mops}#isFunctioningAs",
            )  # Connect the CBU to its corresonding GBU
