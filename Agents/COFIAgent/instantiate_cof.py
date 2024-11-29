from __future__ import annotations
from twa.kg_operations import PySparqlClient
import json
import logging
from typing import Dict, Any, Set
from ontocof import (
    OntoCOF,
    FrameworkConstruct,
    AssemblyModel,
    Linkage,
    Precursor,
    CrystalStructure,
    GenericBuildingUnit,
    LinkageGenericBuildingUnit,
    GBUNumber,
    LGBUNumber,
    BindingSite,
    CondensateSpecies,
    StochiometricCoefficient,
)

# Configure logging
logging.basicConfig(level=logging.INFO)

class FrameworkLoader:
    def __init__(
        self,
        sparql_endpoint: str,
        assembly_model_json_path: str,
        gbu_lgbu_json_path: str,
        gbu_lgbu_numbers_json_path: str,
        linkage_json_path: str,
        precursor_json_path: str,
        binding_site_json_path: str,
        crystal_occurrences_json_path: str,
    ):
        # Existing initialization code
        self.sparql_client = PySparqlClient(sparql_endpoint, sparql_endpoint)
        OntoCOF.export_to_triple_store(self.sparql_client)
        # Load data files
        with open(assembly_model_json_path, "r") as f:
            self.assembly_model_data = json.load(f)
        with open(gbu_lgbu_json_path, "r") as f:
            self.gbu_lgbu_data = json.load(f)
        with open(gbu_lgbu_numbers_json_path, "r") as f:
            self.gbu_lgbu_numbers_data = json.load(f)
        with open(linkage_json_path, "r") as f:
            self.linkage_data = json.load(f)
        with open(precursor_json_path, "r") as f:
            self.precursor_data = json.load(f)
        with open(binding_site_json_path, "r") as f:
            self.binding_site_data = json.load(f)
        with open(crystal_occurrences_json_path, "r") as f:
            self.crystal_occurrences_data = json.load(f)
        # Initialize caches
        self.framework_construct_cache: Dict[str, FrameworkConstruct] = {}
        self.assembly_model_cache: Dict[str, AssemblyModel] = {}
        self.gbu_cache: Dict[str, GenericBuildingUnit] = {}
        self.lgbu_cache: Dict[str, LinkageGenericBuildingUnit] = {}
        self.lgbu_label_cache: Dict[str, LinkageGenericBuildingUnit] = {}
        self.gbu_number_cache: Dict[str, GBUNumber] = {}
        self.lgbu_number_cache: Dict[str, LGBUNumber] = {}
        self.binding_site_cache: Dict[str, BindingSite] = {}
        self.condensate_species_cache: Dict[str, CondensateSpecies] = {}
        self.stoichiometric_coefficient_cache: Dict[str, StochiometricCoefficient] = {}
        self.linkage_cache: Dict[str, Linkage] = {}
        self.precursor_cache: Dict[str, Precursor] = {}
        self.crystal_structure_cache: Dict[str, CrystalStructure] = {}  # Add cache for CrystalStructure

    def load_data(self, json_path: str):
        """
        Load data from a JSON file and push it to the knowledge graph.
        
        :param json_path: Path to the JSON file containing data to instantiate.
        """
        with open(json_path, "r") as f:
            data = json.load(f)

        total_frameworks = len(data)
        for idx, item in enumerate(data, start=1):
            # Process and create instances
            framework_construct = self._create_framework_construct(item)
            
            # Check if this is the last FrameworkConstruct
            if idx == total_frameworks:
                logging.info(f"Instantiating the last FrameworkConstruct ({idx}/{total_frameworks}): {framework_construct.instance_iri}")
            else:
                logging.info(f"Instantiating FrameworkConstruct {idx}/{total_frameworks}: {framework_construct.instance_iri}")
            
            # Push the instance to the triple store
            framework_construct.push_to_kg(
                self.sparql_client, recursive_depth=-1
            )

    def _create_framework_construct(self, item: dict) -> FrameworkConstruct:
        fc_id = item["FrameworkConstruct"]

        # Check if the FrameworkConstruct instance already exists in cache
        if fc_id in self.framework_construct_cache:
            return self.framework_construct_cache[fc_id]

        # Existing code to create AssemblyModel and Linkage
        assembly_model_id = item["hasAssemblyModel"]["AssemblyModel"]
        assembly_model = self._create_assembly_model(assembly_model_id)

        linkage_id = item["hasLinkage"]["Linkage"]
        linkage = self._create_linkage(linkage_id, assembly_model)

        # Create sets of precursors and crystal structures
        precursors = {
            self._create_precursor(p['Precursor'], assembly_model)
            for p in item["hasPrecursor"]
        }

        crystal_structures = {
            self._create_crystal_structure(cs['CrystalStructure'])
            for cs in item["hasCrystalOccurrence"]
        }

        # Create the FrameworkConstruct instance
        framework_construct = FrameworkConstruct(
            instance_iri=f"{OntoCOF.base_url}{fc_id}",
            hasNumberOfPrecursors=item["hasNumberOfPrecursors"],
            hasAssemblyModel=assembly_model,
            hasLinkage=linkage,
            hasPrecursor=precursors,
            hasCrystalOccurrence=crystal_structures,
        )

        # Add to cache
        self.framework_construct_cache[fc_id] = framework_construct

        return framework_construct

    def _create_assembly_model(self, assembly_model_id: str) -> AssemblyModel:
        """
        Create or retrieve an AssemblyModel instance.

        :param assembly_model_id: ID of the AssemblyModel.
        :return: AssemblyModel instance.
        """
        # Check if the AssemblyModel instance already exists in cache
        if assembly_model_id in self.assembly_model_cache:
            return self.assembly_model_cache[assembly_model_id]

        # Find the assembly model data
        assembly_model_data = next(
            (item for item in self.assembly_model_data if item["AssemblyModel"] == assembly_model_id), None
        )
        if assembly_model_data is None:
            raise ValueError(f"AssemblyModel data not found for ID: {assembly_model_id}")

        # Create instances of linked classes
        # hasGenericBuildingUnit
        generic_building_units = {
            self._create_generic_building_unit(gbu['GenericBuildingUnit'])
            for gbu in assembly_model_data.get("hasGenericBuildingUnit", [])
        }

        # hasLinkageGenericBuildingUnit
        linkage_generic_building_units = {
            self._create_linkage_generic_building_unit(lgbu['LinkageGenericBuildingUnit'])
            for lgbu in assembly_model_data.get("hasLinkageGenericBuildingUnit", [])
        }

        # hasGBUNumber
        gbu_numbers = {
            self._create_gbu_number(gbun['GBUNumber'])
            for gbun in assembly_model_data.get("hasGBUNumber", [])
        }

        # hasLGBUNumber
        lgbu_numbers = {
            self._create_lgbu_number(lgbun['LGBUNumber'])
            for lgbun in assembly_model_data.get("hasLGBUNumber", [])
        }

        # Create the AssemblyModel instance
        assembly_model = AssemblyModel(
            instance_iri=f"{OntoCOF.base_url}{assembly_model_id}",
            hasGenericBuildingUnit=generic_building_units,
            hasLinkageGenericBuildingUnit=linkage_generic_building_units,
            hasGBUNumber=gbu_numbers,
            hasLGBUNumber=lgbu_numbers,
            hasAssemblyModelLabel=assembly_model_data.get("hasAssemblyModelLabel"),
            hasAssemblyModelShortFormula=assembly_model_data.get("hasAssemblyModelShortFormula"),
            hasAssemblyModelFormula=assembly_model_data.get("hasAssemblyModelFormula"),
            hasRCSRReference=assembly_model_data.get("hasRCSRReference"),
            hasFrameworkName=set(assembly_model_data.get("hasFrameworkName", [])),
            hasPeriodicity=assembly_model_data.get("hasPeriodicity"),
            hasFrameworkNotation=assembly_model_data.get("hasFrameworkNotation"),
        )

        # Add to cache
        self.assembly_model_cache[assembly_model_id] = assembly_model

        return assembly_model

    def _create_generic_building_unit(self, gbu_id: str) -> GenericBuildingUnit:
        """
        Create or retrieve a GenericBuildingUnit instance.

        :param gbu_id: ID of the GenericBuildingUnit.
        :return: GenericBuildingUnit instance.
        """
        if gbu_id in self.gbu_cache:
            return self.gbu_cache[gbu_id]

        # Find the GBU data
        gbu_data = next(
            (item for item in self.gbu_lgbu_data if item.get("GenericBuildingUnit") == gbu_id), None
        )
        if gbu_data is None:
            raise ValueError(f"GenericBuildingUnit data not found for ID: {gbu_id}")

        # Create the GenericBuildingUnit instance
        gbu_instance = GenericBuildingUnit(
            instance_iri=f"{OntoCOF.base_url}{gbu_id}",
            hasGBULabel=gbu_data.get("hasGBULabel"),
            hasModularity=gbu_data.get("hasModularity"),
            hasPlanarity=gbu_data.get("hasPlanarity"),
        )
        self.gbu_cache[gbu_id] = gbu_instance
        return gbu_instance

    def _create_linkage_generic_building_unit(self, lgbu_id: str) -> LinkageGenericBuildingUnit:
        """
        Create or retrieve a LinkageGenericBuildingUnit instance.

        :param lgbu_id: ID of the LinkageGenericBuildingUnit.
        :return: LinkageGenericBuildingUnit instance.
        """
        if lgbu_id in self.lgbu_cache:
            return self.lgbu_cache[lgbu_id]

        # Find the LGBU data
        lgbu_data = next(
            (item for item in self.gbu_lgbu_data if item.get("LinkageGenericBuildingUnit") == lgbu_id), None
        )
        if lgbu_data is None:
            raise ValueError(f"LinkageGenericBuildingUnit data not found for ID: {lgbu_id}")

        # Create the LinkageGenericBuildingUnit instance
        lgbu_instance = LinkageGenericBuildingUnit(
            instance_iri=f"{OntoCOF.base_url}{lgbu_id}",
            hasLGBULabel=lgbu_data.get("hasLGBULabel"),
            hasModularity=lgbu_data.get("hasModularity"),
            hasPlanarity=lgbu_data.get("hasPlanarity"),
        )
        self.lgbu_cache[lgbu_id] = lgbu_instance

        # Store in label cache
        lgbu_labels = lgbu_instance.hasLGBULabel
        if lgbu_labels:
            # Extract the label from the set
            lgbu_label = next(iter(lgbu_labels))
            self.lgbu_label_cache[lgbu_label] = lgbu_instance

        return lgbu_instance
    
    def _create_gbu_number(self, gbu_number_id: str) -> GBUNumber:
        """
        Create or retrieve a GBUNumber instance.

        :param gbu_number_id: ID of the GBUNumber.
        :return: GBUNumber instance.
        """
        if gbu_number_id in self.gbu_number_cache:
            return self.gbu_number_cache[gbu_number_id]

        # Find the GBUNumber data
        gbu_number_data = next(
            (item for item in self.gbu_lgbu_numbers_data if item.get("GBUNumber") == gbu_number_id), None
        )
        if gbu_number_data is None:
            raise ValueError(f"GBUNumber data not found for ID: {gbu_number_id}")

        # Get the GenericBuildingUnit instance (match by IRI)
        gbu_id = gbu_number_data.get("isNumberOf")
        gbu_instance = self._create_generic_building_unit(gbu_id)

        # Create the GBUNumber instance
        gbu_number_instance = GBUNumber(
            instance_iri=f"{OntoCOF.base_url}{gbu_number_id}",
            isNumberOfGBU=gbu_instance,
            hasValue=gbu_number_data.get("hasValue")
        )
        self.gbu_number_cache[gbu_number_id] = gbu_number_instance
        return gbu_number_instance

    def _create_lgbu_number(self, lgbu_number_id: str) -> LGBUNumber:
        """
        Create or retrieve a LGBUNumber instance.

        :param lgbu_number_id: ID of the LGBUNumber.
        :return: LGBUNumber instance.
        """
        if lgbu_number_id in self.lgbu_number_cache:
            return self.lgbu_number_cache[lgbu_number_id]

        # Find the LGBUNumber data
        lgbu_number_data = next(
            (item for item in self.gbu_lgbu_numbers_data if item.get("LGBUNumber") == lgbu_number_id), None
        )
        if lgbu_number_data is None:
            raise ValueError(f"LGBUNumber data not found for ID: {lgbu_number_id}")

        # Get the LinkageGenericBuildingUnit instance (match by IRI)
        lgbu_id = lgbu_number_data.get("isNumberOf")
        lgbu_instance = self._create_linkage_generic_building_unit(lgbu_id)

        # Create the LGBUNumber instance
        lgbu_number_instance = LGBUNumber(
            instance_iri=f"{OntoCOF.base_url}{lgbu_number_id}",
            isNumberOfLGBU=lgbu_instance,
            hasValue=lgbu_number_data.get("hasValue")
        )
        self.lgbu_number_cache[lgbu_number_id] = lgbu_number_instance
        return lgbu_number_instance

    def _create_linkage(self, linkage_id: str, assembly_model: AssemblyModel) -> Linkage:
        """
        Create or retrieve a Linkage instance.

        :param linkage_id: ID of the Linkage.
        :param assembly_model: The AssemblyModel instance associated with the Linkage.
        :return: Linkage instance.
        """
        if linkage_id in self.linkage_cache:
            return self.linkage_cache[linkage_id]

        # Find the linkage data
        linkage_data = next(
            (item for item in self.linkage_data if item["Linkage"] == linkage_id), None
        )
        if linkage_data is None:
            raise ValueError(f"Linkage data not found for ID: {linkage_id}")

        # Handle involvesBindingSite
        involves_binding_site_data = linkage_data.get("involvesBindingSite", [])
        binding_sites = {
            self._create_binding_site(binding_site_data)
            for binding_site_data in involves_binding_site_data
        }

        # Handle involvesCondensateSpecies
        condensate_species_data = linkage_data.get("haveCondensateSpecies")
        condensate_species_instance = None
        if condensate_species_data:
            condensate_species_instance = self._create_condensate_species(condensate_species_data)

        # Handle functionsAsLGBU
        functions_as_lgbu_label = linkage_data.get("functionsAsLGBU")
        functions_as_lgbu = None
        if functions_as_lgbu_label:
            # Get the LGBUs associated with the assembly_model
            assembly_model_lgbus: Set[LinkageGenericBuildingUnit] = assembly_model.hasLinkageGenericBuildingUnit
            for lgbu in assembly_model_lgbus:
                lgbu_labels = lgbu.hasLGBULabel
                if lgbu_labels:
                    lgbu_label = next(iter(lgbu_labels))
                    if lgbu_label == functions_as_lgbu_label:
                        functions_as_lgbu = lgbu
                        break
            if functions_as_lgbu is None:
                raise ValueError(f"LinkageGenericBuildingUnit with label {functions_as_lgbu_label} not found in AssemblyModel {assembly_model.instance_iri}")

        # Handle hasStochiometricCoefficient
        stoch_coeff_data_list = linkage_data.get("hasStochiometricCoefficient", [])
        stoch_coefficients = set()
        for stoch_coeff_data in stoch_coeff_data_list:
            stoch_coeff_id = stoch_coeff_data["StochiometricCoefficient"]
            stoch_coeff_data_args = {
                'instance_iri': f"{OntoCOF.base_url}{stoch_coeff_id}",
                'hasValue': stoch_coeff_data.get('hasValue'),
            }

            # Link to BindingSite if applicable
            if "isValueOfBindingSite" in stoch_coeff_data:
                binding_site_id = stoch_coeff_data["isValueOfBindingSite"]
                # Retrieve the existing BindingSite instance
                binding_site_instance = next(
                    (bs for bs in binding_sites if bs.instance_iri == f"{OntoCOF.base_url}{binding_site_id}"), None
                )
                if binding_site_instance is None:
                    # Create if not found
                    binding_site_instance = self._create_binding_site({"BindingSite": binding_site_id})
                    binding_sites.add(binding_site_instance)
                stoch_coeff_data_args['isValueOfBindingSite'] = binding_site_instance

            # Link to CondensateSpecies if applicable
            if "isValueOfCondensateSpecies" in stoch_coeff_data:
                condensate_species_id = stoch_coeff_data["isValueOfCondensateSpecies"]
                if condensate_species_instance is None or condensate_species_instance.instance_iri != f"{OntoCOF.base_url}{condensate_species_id}":
                    condensate_species_instance = self._create_condensate_species({
                        "CondensateSpecies": condensate_species_id
                    })
                stoch_coeff_data_args['isValueOfCondensateSpecies'] = condensate_species_instance

            # Create the StochiometricCoefficient instance with all arguments
            stoch_coeff_instance = StochiometricCoefficient(**stoch_coeff_data_args)

            # Add to set of StochiometricCoefficients
            stoch_coefficients.add(stoch_coeff_instance)
            # Add to cache
            self.stoichiometric_coefficient_cache[stoch_coeff_id] = stoch_coeff_instance

        # Now create the Linkage instance with all required fields
        linkage_instance = Linkage(
            instance_iri=f"{OntoCOF.base_url}{linkage_id}",
            hasReactionName=linkage_data.get("hasReactionName"),
            hasReactionNote=linkage_data.get("hasReactionNote"),
            hasLinkageLabel=linkage_data.get("hasLinkageLabel"),
            hasLinkageFormula=linkage_data.get("hasLinkageFormula"),
            hasFormalCharge=linkage_data.get("hasFormalCharge"),
            hasLinkageName=linkage_data.get("hasLinkageName"),
            functionsAsLGBU=functions_as_lgbu,
            involvesBindingSite=binding_sites,
            involvesCondensateSpecies=condensate_species_instance,
            hasStochiometricCoefficient=stoch_coefficients
        )

        # Add to cache
        self.linkage_cache[linkage_id] = linkage_instance
        return linkage_instance

    def _create_binding_site(self, binding_site_data: dict) -> BindingSite:
        binding_site_id = binding_site_data["BindingSite"]
        if binding_site_id in self.binding_site_cache:
            return self.binding_site_cache[binding_site_id]

        # Find the binding site data from self.binding_site_data
        binding_site_info = next(
            (item for item in self.binding_site_data if item["BindingSite"] == binding_site_id),
            None,
        )
        if binding_site_info is None:
            raise ValueError(f"BindingSite data not found for ID: {binding_site_id}")

        # Extract properties
        hasChemicalFormula = binding_site_info.get("hasChemicalFormula", set())
        hasBindingSiteLabel = binding_site_info.get("hasBindingSiteLabel", set())
        hasBindingSiteName = binding_site_info.get("hasBindingSiteName", set())
        hasBindingSiteIndex = binding_site_info.get("hasBindingSiteIndex", set())
        hasDentation = binding_site_info.get("hasDentation", set())
        hasDummyLabel = binding_site_info.get("hasDummyLabel", set())
        hasBindingSiteComplementaryDummy = binding_site_info.get("hasBindingSiteComplementaryDummy", set())

        # Create BindingSite instance
        binding_site_instance = BindingSite(
            instance_iri=f"{OntoCOF.base_url}{binding_site_id}",
            hasChemicalFormula=hasChemicalFormula,
            hasBindingSiteLabel=hasBindingSiteLabel,
            hasBindingSiteName=hasBindingSiteName,
            hasBindingSiteIndex=hasBindingSiteIndex if bool(hasBindingSiteIndex) else set(),
            hasDentation=hasDentation if bool(hasDentation) else set(),
            hasDummyLabel=hasDummyLabel if bool(hasDummyLabel) else set(),
            hasBindingSiteComplementaryDummy=hasBindingSiteComplementaryDummy,
        )

        # Add to cache
        self.binding_site_cache[binding_site_id] = binding_site_instance
        return binding_site_instance


    def _create_condensate_species(self, condensate_species_data: dict) -> CondensateSpecies:
        condensate_species_id = condensate_species_data["CondensateSpecies"]
        if condensate_species_id in self.condensate_species_cache:
            return self.condensate_species_cache[condensate_species_id]

        condensate_species_instance = CondensateSpecies(
            instance_iri=f"{OntoCOF.base_url}{condensate_species_id}",
            hasChemicalFormula=condensate_species_data.get("hasChemicalFormula", set()),
            hasChemicalName=condensate_species_data.get("hasChemicalName", set())
        )

        # Add to cache
        self.condensate_species_cache[condensate_species_id] = condensate_species_instance
        return condensate_species_instance


    def _create_precursor(self, precursor_id: str, assembly_model: AssemblyModel) -> Precursor:
        if precursor_id in self.precursor_cache:
            return self.precursor_cache[precursor_id]

        # Find the precursor data
        precursor_data = next(
            (item for item in self.precursor_data if item["Precursor"] == precursor_id), None
        )
        if precursor_data is None:
            raise ValueError(f"Precursor data not found for ID: {precursor_id}")

        # Extract properties
        hasPrecursorLabel = precursor_data.get("hasPrecursorLabel")
        hasCoreMolFile = precursor_data.get("hasCoreMolFile")
        hasCoreInpFile = precursor_data.get("hasCoreInpFile")
        functionsAsGBU_label = precursor_data.get("functionsAsGBU")
        hasBindingSite_data = precursor_data.get("hasBindingSite", [])

        # Create BindingSite instances
        binding_sites = {
            self._create_binding_site(binding_site_data)
            for binding_site_data in hasBindingSite_data
        }

        # Match functionsAsGBU_label to the correct GenericBuildingUnit in the assembly model
        functions_as_gbu = None
        if functionsAsGBU_label:
            # Get the GBUs associated with the assembly_model
            assembly_model_gbus: Set[GenericBuildingUnit] = assembly_model.hasGenericBuildingUnit
            for gbu in assembly_model_gbus:
                gbu_labels = gbu.hasGBULabel
                if gbu_labels:
                    gbu_label = next(iter(gbu_labels))
                    if gbu_label == functionsAsGBU_label:
                        functions_as_gbu = gbu
                        break
            if functions_as_gbu is None:
                raise ValueError(f"GenericBuildingUnit with label {functionsAsGBU_label} not found in AssemblyModel {assembly_model.instance_iri}")

        # Create the Precursor instance
        precursor_instance = Precursor(
            instance_iri=f"{OntoCOF.base_url}{precursor_id}",
            hasPrecursorLabel=hasPrecursorLabel,
            hasCoreMolFile=hasCoreMolFile,
            hasCoreInpFile=hasCoreInpFile,
            functionsAsGBU=functions_as_gbu,
            hasBindingSite=binding_sites,
        )

        # Add to cache
        self.precursor_cache[precursor_id] = precursor_instance
        return precursor_instance

    def _create_crystal_structure(self, crystal_structure_id: str) -> CrystalStructure:
        if crystal_structure_id in self.crystal_structure_cache:
            return self.crystal_structure_cache[crystal_structure_id]

        # Find the crystal structure data
        crystal_data = next(
            (item for item in self.crystal_occurrences_data if item["CrystalStructure"] == crystal_structure_id),
            None,
        )
        if crystal_data is None:
            raise ValueError(f"CrystalStructure data not found for ID: {crystal_structure_id}")

        # Extract properties
        hasDOI = crystal_data.get("hasDOI")
        hasCrystalName = crystal_data.get("hasCrystalName")
        hasInterlayerStacking = crystal_data.get("hasInterlayerStacking")

        # Create the CrystalStructure instance
        crystal_structure_instance = CrystalStructure(
            instance_iri=f"{OntoCOF.base_url}{crystal_structure_id}",
            hasDOI=hasDOI,
            hasCrystalName=hasCrystalName,
            hasInterlayerStacking=hasInterlayerStacking,
        )

        # Add to cache
        self.crystal_structure_cache[crystal_structure_id] = crystal_structure_instance
        return crystal_structure_instance

if __name__ == "__main__":
    # Define the SPARQL endpoint and JSON input paths
    sparql_endpoint = "http://68.183.227.15:3838/blazegraph/namespace/ontocofs/sparql"
    framework_construct_json_path = "cof_data/cof_FrameworkConstruct.json"
    assembly_model_json_path = "cof_data/cof_AM.json"
    gbu_lgbu_json_path = "cof_data/cof_L-GBU.json"
    gbu_lgbu_numbers_json_path = "cof_data/cof_L-GBUNumbers.json"
    linkage_json_path = "cof_data/cof_LFR.json"
    precursor_json_path = "cof_data/cof_Precursors.json"
    binding_site_json_path = "cof_data/cof_BindingSites.json"
    crystal_occurrences_json_path = "cof_data/cof_Crystals.json"

    # Create the FrameworkLoader instance
    data_loader = FrameworkLoader(
        sparql_endpoint,
        assembly_model_json_path,
        gbu_lgbu_json_path,
        gbu_lgbu_numbers_json_path,
        linkage_json_path,
        precursor_json_path,
        binding_site_json_path,
        crystal_occurrences_json_path,
    )

    # Load the JSON data and push it to the knowledge graph
    data_loader.load_data(framework_construct_json_path)