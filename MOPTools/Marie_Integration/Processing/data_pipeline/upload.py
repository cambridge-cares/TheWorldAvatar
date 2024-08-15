# Import relevant packages
from __future__                     import annotations
from twa.data_model.base_ontology   import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
from twa.data_model.iris            import TWA_BASE_URL
from twa.kg_operations              import PySparqlClient
from typing                         import ClassVar
from pydantic                       import Field
from twa.kg_operations              import PySparqlClient
from rdflib                         import Graph, URIRef, Literal
from rdflib.namespace               import RDF
from typing import Optional
import re
import os
import sys
PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from rework_ontomops.update_kg  import config_a_box_updates
import json
import csv
from io import StringIO
import pandas as pd

def read_json_file(file_path):
    """
    Reads a JSON file and returns the data as a dictionary.

    Args:
    file_path (str): The path to the JSON file.

    Returns:
    dict: The data parsed from the JSON file.
    """
    with open(file_path, 'r') as file:
        data            = json.load(file)
        #data2           = pd.read_json(file)
        # Set display options to show the full DataFrame
        pd.set_option('display.max_columns', None)
        pd.set_option('display.max_rows', None)
        pd.set_option('display.max_colwidth', None)
        pd.set_option('display.width', None)
        #data2           = data2.at[0, 'Synthesis']
    return data


# Your ontology needs to inherit the BaseOntology class
class OntoSyn(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoSyn'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

    ###-----------------------------------------------------------------------------------------------
    # Other Ontologies
class OntoLab(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'OntoLab'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoSpecies(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontospecies/"
    namespace:          ClassVar[str]           = 'OntoSpecies.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoMOPs(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'ontomops'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoKin(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontokin/"
    namespace:          ClassVar[str]           = 'OntoKin.owl'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class BIBO(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://purl.org/ontology/"
    namespace:          ClassVar[str]           = 'bibo'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OM2(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.ontology-of-units-of-measure.org/resource/"
    namespace:          ClassVar[str]           = 'om-2'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoReaction(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = TWA_BASE_URL
    namespace:          ClassVar[str]           = 'ontoreaction'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'
    
class OntoCapeMaterial(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/material/"
    namespace:          ClassVar[str]           = 'material.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoCapePhaseSystem(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/material/phase_system/"
    namespace:          ClassVar[str]           = 'phase_system.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoCapeSystem(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.theworldavatar.com/ontology/ontocape/upper_level/"
    namespace:          ClassVar[str]           = 'system.owl#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class SKOS(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.w3.org/2004/02/skos/"
    namespace:          ClassVar[str]           = 'core#'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class RDFS(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "http://www.w3.org/2000/01/"
    namespace:          ClassVar[str]           = 'rdf-schema'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

    ###-----------------------------------------------------------------------------------------------
    # Classes:

class ChemicalTransformation(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    isDescribedBy                       : Optional[IsDescribedBy[ChemicalSynthesis]]                    = set()
    hasChemicalInput                    : Optional[HasChemicalInput[ChemicalInput]]                     = set()
    hasChemicalOutput                   : Optional[HasChemicalOutput[ChemicalOutput]]                   = set()

class ChemicalSynthesis(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasSynthesisStep                    : Optional[HasSynthesisStep[SynthesisStep]]                     = set()
    hasFirstStep                        : Optional[HasFirstStep[SynthesisStep]]                         = set()
    hasEquipment                        : Optional[HasEquipment[LabEquipment]]                          = set()
    retrievedFrom                       : Optional[RetrievedFrom[Document]]                             = set()

class SynthesisStep(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasContainerVessel                  : Optional[HasContainerVessel[Vessel]]                          = None

class Product(BaseClass):
    rdfs_isDefinedBy                    = OntoKin

class ChemicalOutput(Product):
    rdfs_isDefinedBy                    = OntoSyn
    # should be with OntoMop
    isRepresentedBy                     : Optional[IsRepresentedBy[MetalOrganicPolyhedron]]             = None
    #hasSynthesisYield                   : HasSynthesisYield[SynthesisYield]

class SynthesisYield(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasYieldMass                        : Optional[HasYieldMass[Mass]]                                  = None

class SynthesisReactant(BaseClass):
    rdfs_isDefinedBy                    = OntoReaction

class Add(SynthesisStep):       
    rdfs_isDefinedBy                    = OntoSyn
    hasAddedReactant                    : Optional[HasYieldMass[SynthesisReactant]]                     = None
    hasAddedSolvent                     : HasAddedSolvent[Solvent]
    isAdded                             : IsAdded[Material]

class Filter(SynthesisStep):        
    rdfs_isDefinedBy                    = OntoSyn
    isWashedWith                        : IsWashedWith[Material]
    hasWashingSolvent                   : HasWashingSolvent[Solvent]
    isRepeated                          : IsRepeated[int]

class Dry(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn

class Stir(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasStirringDuration                 : HasStirringDuration[Duration]

class Sonication(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasSonicationDuration               : HasSonicationDuration[Duration]

class HeatChill(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasHeatChillDuration                : HasHeatChillDuration[Duration]
    hasReactionTemperature              : HasReactionTemperature[Temperature]
    hasHeatChillRate                    : HasHeatChillRate[TemperatureRate]
    #hasHeatChillDevice                  : HasHeatChillDevice[HeatChillDevice]
    hasVacuum                           : HasVacuum[bool]
    isSealed                            : Optional[IsSealed[bool]]                                      = None

class LabEquipment(BaseClass):
    rdfs_isDefinedBy                    = OntoLab

class VesselEnvironment(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    label                               : str

class Vessel(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn

class Species(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    label                               : Label[str]
    altLabel                            : AltLabel[str]

class PhaseComponent(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    representsOccurenceOf               : Optional[RepresentsOccurenceOf[Species]]                      = None

class ScalarValue(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeSystem
    hasUnitOfMeasure                    : Optional[HasUnitOfMeasure[AmountOfSubstanceConcentration]]    = None
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = None

class PhaseComponentConcentration(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    hasValue                            : Optional[HasValue[ScalarValue]]                               = None

class Composition(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    comprisesDirectly                   : Optional[ComprisesDirectly[PhaseComponentConcentration]]      = None

class SinglePhase(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    isComposedOfSubsystem               : Optional[IsComposedOfSubsystem[PhaseComponent]]               = None
    hasComposition                      : Optional[HasComposition[Composition]]                         = None

class Material(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeMaterial
    thermodynamicBehaviour              : Optional[ThermodynamicBehaviour[SinglePhase]]                 = None

class ChemicalInput(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    referencesMaterial                  : Optional[ReferencesMaterial[Material]]                        = set()

class MetalOrganicPolyhedron(BaseClass):
    rdfs_isDefinedBy                    = OntoMOPs
    hasCCDCNumber                       : Optional[HasCCDCNumber[int]]                              = set()
    hasMOPFormula                       : Optional[HasMOPFormula[str]]                              = set()
    mopAltLabel                         : Optional[MopAltLabel[str]]                                = set()

class Solvent(BaseClass):
    rdfs_isDefinedBy                    = OntoKin

class HeatChillDevice(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn

class Document(BaseClass):
    rdfs_isDefinedBy                    = BIBO
    doi                                 : Optional[Doi[str]]                                        = set()

class Duration(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : HasValue[float]

class Temperature(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : HasValue[float]

class Mass(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : HasValue[float]

class TemperatureRate(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : HasValue[float]

class Yield(BaseClass):
    rdfs_isDefinedBy                    = OntoReaction

class AmountOfSubstanceFraction(BaseClass):
    rdfs_isDefinedBy                    = OM2

class AmountOfSubstanceConcentration(BaseClass):
    rdfs_isDefinedBy                    = OM2

    ###-----------------------------------------------------------------------------------------------
    # Datatype Properties:
class HasNitrogenAthmosphere(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoSyn

class HasVacuum(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoSyn
class IsSealed(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoSyn
class IsRepeated(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoSyn
class HasValue(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OM2

class AltLabel(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = SKOS

class MopAltLabel(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoMOPs

class Label(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = RDFS

class HasCCDCNumber(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoMOPs

class HasNumericalValue(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OM2

class HasMOPFormula(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs

class Doi(DatatypeProperty):
    rdfs_isDefinedBy                    = BIBO
    ###-----------------------------------------------------------------------------------------------
    # Object Properties:

class HasSynthesisStep(ObjectProperty):
    # The user MUST provide the ontology for which the concept `rdfs_isDefinedBy`
    # Since `rdfs_isDefinedBy` is already defined as a ClassVar, the user can just assign a value here
    rdfs_isDefinedBy                    = OntoSyn
    # 0 and None for field `range` indicates no cardinality restriction (which is also the default value)
    # Therefore, the below line is equivalent to `range: as_range(AnotherConcept)`

class HasFirstStep(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class ReferencesMaterial(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class ThermodynamicBehaviour(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeMaterial

class IsComposedOfSubsystem(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem

class HasComposition(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem

class ComprisesDirectly(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem

class RepresentsOccurenceOf(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem

class HasNextStep(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasContainerVessel(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasProduct(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class IsRepresentedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasSynthesisYield(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasYieldMass(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasSonicationDuration(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class YieldLimitingSpecies(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasAddedReactant(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasAddedSolvent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasReactantMaterialAmount(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasSolventMaterialAmount(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasEquipment(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasHeatChillDevice(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasReactionTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasHeatChillRate(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasHeatChillDuration(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasStirringDuration(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class IsAdded(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class IsWashedWith(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasWashingSolvent(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class RetrievedFrom(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class IsDescribedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasChemicalInput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasChemicalOutput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class HasSynthesisRole(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn

class Unit(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OM2

class HasUnitOfMeasure(DatatypeProperty):
    # Same as ObjectProperty, `rdfs_isDefinedBy` is a compulsory field
    rdfs_isDefinedBy                    = OntoCapeSystem

class UploadKG:
    """Parent class for the folder. Provides the following methods:
    - query_triple: Queries the KG based on provided triples in where clause (triple) and select statement(select_var). 
    """

    def __init__(self, query_endpoint, update_endpoint, kg_user, kg_password):
        self.prefix                     = """  PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                                               PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                                               PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
                                               PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"""
        a_box_updates_config            = config_a_box_updates("../OntoSynthesisConnection.env")
        self.sparql_client              = PySparqlClient(
        query_endpoint                  = a_box_updates_config.SPARQL_QUERY_ENDPOINT    ,
        update_endpoint                 = a_box_updates_config.SPARQL_UPDATE_ENDPOINT   ,
        kg_user                         = a_box_updates_config.KG_USERNAME              ,
        kg_password                     = a_box_updates_config.KG_PASSWORD              ,
        fs_url                          = ""                                            ,
        fs_user                         = ""                                            ,
        fs_pwd                          = ""
        )

    def query_triple(self, triple, select_var):
        """Queries the KG based on provided triples in where clause (triple) and select statement(select_var).
        Inputs: 
            - triple:     String in where clause.
            - select_var: String with return variables
        Outputs:
        """
        query_triple                = f"""
                                        {self.prefix}
                                        SELECT {select_var}
                                        WHERE {{
                                        {triple}
                                            }}
                                        """
        # query
        return self.sparql_client.perform_query(query_triple)
    
    def query_update(self, where_stat:str, delete_stat: str, insert_stat:str):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Please specify if object is literal => literal=True or not literal=False (Default)."""
        update_triple                = f"""
                                        DELETE  {{ {delete_stat} }}
                                        INSERT  {{ {insert_stat} }}
                                        WHERE   {{ {where_stat} }}
                                        """
        print(update_triple)
        self.sparql_client.perform_update(update_triple)
        
    
    def delete_connections(self, iri):
        "Delete all connections from and to the input IRI."
        delete_incoming_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        ?incomingIRI ?incomingP <{iri}> .
        }}
        """
        delete_outgoing_query       = f"""
        {self.prefix}
        DELETE WHERE {{
        <{iri}> ?outgoingP ?outgoingIRI .
        }}
        """
        self.sparql_client.perform_update(delete_incoming_query)
        self.sparql_client.perform_update(delete_outgoing_query)
        print(f"deleted outgoing: {delete_incoming_query}")

    def delete_triple(self, tsubject, tpredicate, tobject, literal=False):
        "Delete all connections to the input IRI."
        if literal:
            insert_string           = f"""<{tsubject}> <{tpredicate}> "{tobject}" ."""
        else:
            insert_string           = f"""<{tsubject}> <{tpredicate}> <{tobject}> ."""
        delete_query                = f"""
                                            {self.prefix}
                                            DELETE DATA {{
                                            {insert_string}
                                            }}
                                            """
        print(f"deleted triple: <{tsubject}>, <{tpredicate}>, {tobject}")
        self.sparql_client.perform_update(delete_query)

    def generate_triple(self, tsubject, tpredicate, tobject, literal=False):
        """takes subject, predicate and object of a tripple as input and instantiates a new triple. Please specify if object is literal => literal=True or not literal=False (Default)."""
        if literal:
            insert_string           = f"""<{tsubject}> <{tpredicate}> "{tobject}" ."""
        else:
            insert_string           = f"""<{tsubject}> <{tpredicate}> <{tobject}> ."""
        print(f"generated tripple: {insert_string}.")
        update_query                = f"""
        {self.prefix}
        INSERT DATA {{
            {insert_string}
        }}
        """
        self.sparql_client.perform_update(update_query)
        
def upload_vessels(client):
    vessel_ss_teflon                    = Vessel(label="Teflon-lined stainless-steel vessel")
    glass_vial                          = Vessel(label="glass vial")
    quartz_tube                         = Vessel(label="quartz tube")
    round_bottom_flask                  = Vessel(label="round bottom flask")
    glass_scintilation_vial             = Vessel(label="glass scintillation vial")
    pyrex_tube                          = Vessel(label="pyrex tube")

    vessel_ss_teflon.push_to_kg(client, recursive_depth=-1)
    glass_vial.push_to_kg(client, recursive_depth=-1)
    quartz_tube.push_to_kg(client, recursive_depth=-1)
    round_bottom_flask.push_to_kg(client, recursive_depth=-1)
    glass_scintilation_vial.push_to_kg(client, recursive_depth=-1)
    pyrex_tube.push_to_kg(client, recursive_depth=-1)

def extract_numbers_and_units_add(text):
    # Regular expression to find numbers followed by units
    pattern             = r'(\d*\.?\d+)\s*([a-zA-Z]+)'
    # Find all matches in the text
    matches             = re.findall(pattern, text)

    # Separate numbers and units
    numbers             = [float(match[0]) for match in matches]
    units               = [match[1] for match in matches]

    return numbers, units

def extract_numbers_and_units_temp(text):
    # Regular expression to find numbers followed by units
    pattern             = r'(\d*\.?\d+)\s*([^\d\s]+)'

    # Find all matches in the text
    matches             = re.findall(pattern, text)

    # Separate numbers and units
    numbers             = [float(match[0]) for match in matches]
    units               = [match[1] for match in matches]

    return numbers, units

def extract_numbers_and_units_rate(text):
    # Regular expression to find numbers followed by units
    pattern             = r'(\d*\.?\d+)\s*([°a-zA-Z/]+)'

    # Find all matches in the text
    matches             = re.findall(pattern, text)

    # Separate numbers and units
    numbers             = [float(match[0]) for match in matches]
    units               = [match[1] for match in matches]

    return numbers, units

class TextToCSV:
    def __init__(self, input_filename):
        self.input_filename = input_filename

    def read_and_clean_file(self):
        with open(self.input_filename, 'r') as file:
            lines = file.readlines()
        
        # Remove the "csv" line and any trailing newlines
        clean_lines = [line.strip() for line in lines if line.strip() != "```csv" and line.strip() != "```"]

        return clean_lines
    
    def extract_csv_entries(self, clean_lines):
        csv_content = '\n'.join(clean_lines)
        
        # Clean the content by removing extra spaces and quotes
        clean_content = csv_content.replace('" ', '"').replace(' "', '"')

        entries = []
        # Use StringIO to read the CSV content from a string
        with StringIO(clean_content) as file:
            reader = csv.DictReader(file)
            for row in reader:
                # Strip spaces and quotes from each value
                cleaned_row = {key.strip().strip('"'): value.strip().strip('"') for key, value in row.items()}
                entries.append(cleaned_row)
        return entries
    def filter_by_synthesis_role(self, entries):
        product_entries = []
        other_entries = []

        for entry in entries:
            if entry.get("Synthesis Role") == "Product":
                product_entries.append(entry)
            else:
                other_entries.append(entry)
        
        return product_entries, other_entries
    
def species_querying(client, species_label, recursion_counter):
    print(recursion_counter)
    print(species_label)
    # avoid linking all to N/A instance:
    if species_label[recursion_counter] == 'N/A':
        if recursion_counter < 2:
            return species_querying(client, species_label, recursion_counter=recursion_counter+1)
        else:
            return []

    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?Species WHERE {{
        ?Species a os:Species .
        VALUES ?Text {{"{species_label[recursion_counter]}"}}
        ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)|rdfs:label|rdf:label|skos:altLabel|<http://www.w3.org/2000/01/rdf-schema/label>) ?Text . 
        }}"""
    query_result                    = client.perform_query(query)
    print("queried result: ", query_result)
    # return if solution is found or maximum iterations are reached.
    if query_result              != [] or recursion_counter >= 2:
        print(f"Species querying for: {species_label[recursion_counter]}", query_result, "\n")
        return query_result
    else:
        return species_querying(client, species_label, recursion_counter=recursion_counter+1)

def mop_querying(client, CCDC_number, mop_formula, mop_name):
    CCDC_number             = remove_na(CCDC_number)
    mop_formula             = remove_na(mop_formula)
    mop_name                = remove_na(mop_name)
    if CCDC_number != "N/A":
        query = f"""
            PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
            PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
            PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
            SELECT ?mop
            WHERE {{
            ?mop a <https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedron>                        .
            VALUES ?Text {{"{CCDC_number, mop_formula, mop_name}"}}
            ?mop (<https://www.theworldavatar.com/kg/ontomops/hasMOPFormula>|<https://www.theworldavatar.com/kg/ontomops/mopAltLabel>|<https://www.theworldavatar.com/kg/ontomops/hasCCDCNumber>) ?Text .  
            }}"""
        out                     = client.perform_query(query)
    else:
        out                     = []
    return out

def transformation_querying(client, mop_name):
    query = f"""
        PREFIX om:      <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        PREFIX osyn:    <https://www.theworldavatar.com/kg/OntoSyn/>
        SELECT ?chemicalTrans
        WHERE {{
        ?chemicalTrans      osyn:hasChemicalOutput  ?chemicalOutput         .
        ?chemicalOutput     osyn:isRepresentedBy    ?mop                    .
                    ?mop a <https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedron>                        .
        VALUES ?Text {{"{mop_name}"}}
        ?mop (<https://www.theworldavatar.com/kg/ontomops/hasMOPFormula>|<https://www.theworldavatar.com/kg/ontomops/mopAltLabel>|<https://www.theworldavatar.com/kg/ontomops/hasCCDCNumber>) ?Text .  
        
                }}"""
    return  client.perform_query(query)

    


def get_client(name):
    a_box_updates_config                        = config_a_box_updates(f"../{name}.env")
    return                                        PySparqlClient(
        query_endpoint                          = a_box_updates_config.SPARQL_QUERY_ENDPOINT            ,
        update_endpoint                         = a_box_updates_config.SPARQL_UPDATE_ENDPOINT           ,
        kg_user                                 = a_box_updates_config.KG_USERNAME                      ,
        kg_password                             = a_box_updates_config.KG_PASSWORD                      ,
        fs_url                                  = ""                                                    ,
        fs_user                                 = ""                                                    ,
        fs_pwd                                  = ""        )

def unit_upload(client):
    duration_h                                  = "<http://www.ontology-of-units-of-measure.org/resource/om-2/hour>"
    duration_day                                = "<http://www.ontology-of-units-of-measure.org/resource/om-2/day>"
    duration_s                                  = "<http://www.ontology-of-units-of-measure.org/resource/om-2/second-Time>"
    temperature_K                               = "<http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin>"
    temperature_C                               = "<http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius>"
    temperature_rate_degh                       = "<http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerHour>"
    temperature_rate_degmin                     = "<http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerMinute-Time>"
    temperature_rate_degs                       = "<http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerSecond-Time>"
    mole_per_litre                              = "<http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre>"
    labels                                      = ["hour", "day", "second", "kelvin", "degree Celsius", "degree Celsius per hour", "degree Celsius per minute", "degree Celsius per second", "mole per litre"]
    subjects                                    = [duration_h, duration_day, duration_s, temperature_K, temperature_C, temperature_rate_degh, temperature_rate_degmin, temperature_rate_degs, mole_per_litre]
    for i, label in enumerate(labels):
        query                                   = insert_query_unit(subjects[i], label)
        client.perform_update(query)



def insert_query_unit(subject_syn, label_syn):
    return                                   f"""
                                                    PREFIX om:          <http://www.theworldavatar.com/ontology/ontomops/OntoMOPs.owl#>
                                                    PREFIX os:          <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
                                                    PREFIX osyn:        <https://www.theworldavatar.com/kg/ontosyn/OntoSyn.owl#>
                                                    PREFIX rdfs:        <http://www.w3.org/2000/01/rdf-schema#>
                                                    PREFIX xsd: 	    <http://www.w3.org/2001/XMLSchema#>
                                                    INSERT DATA {{
                                                    {subject_syn} rdfs:label 	"{label_syn}"        .
                                                    }}
                                                """
def heatchill_upload(client, heatchill_step):
    temp, temp_unit                             = extract_numbers_and_units_temp(heatchill_step["Target temperature"])
    heat_time, time_unit                        = extract_numbers_and_units_add(heatchill_step["Heat or cooling Time"])
    heat_rate, rate_unit                        = extract_numbers_and_units_rate(heatchill_step["Heating or cooling rate"])
    heatchill_device                            = heatchill_step["used Device"]
    sealed                                      = heatchill_step["sealed Vessel"]
    # Vessel:
    match heatchill_step['used Vessel']:
        case 'Teflon-lined stainless-steel vessel':
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_eb0f5942-d36b-47b1-86f0-725c1549fa2e"
        case 'glass vial':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_90589d23-44e8-4698-acdf-bee3e44df96f"
        case 'quartz tube': 
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_06304c23-7926-45d2-841d-690b5de16ed0"
        case 'round bottom flask':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_5a7d7ec9-44d5-4280-8467-f9f624374a9d"
        case 'glass scintillation vial':    
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_b67ea47b-7849-4aac-b0fd-e2715a4ac034"
        case 'pyrex tube':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_080ad74b-950d-4651-a87c-5aa96d5ffb52"

    vacuum                                      = heatchill_step["under Vacuum"]
    print(heatchill_step)
    print(heat_time[0], type(heat_time[0]))
    duration                                    = Duration(hasValue=heat_time[0])
    temperature                                 = Temperature(hasValue=temp)
    temp_rate                                   = TemperatureRate(hasValue=heat_rate)
    vessel                                      = Vessel.pull_from_kg(vessel_iri, client,recursive_depth=-1)[0]
    vessel.isClosed                             = heatchill_step["sealed Vessel"]
    heat_chill                                  = HeatChill(hasContainerVessel=vessel, hasHeatChillDuration=duration, hasReactionTemperature=temperature, hasHeatChillRate=temp_rate, hasVacuum=vacuum, isSealed=sealed)
    
    components = [duration, temperature, temp_rate, vessel, heat_chill]
    for component in components:
        push_component_to_kg(component, client)

def remove_na(input_candidate):
    if input_candidate == "N/A":
        return ""
    return input_candidate

def instantiate_input(chemical_formula, species_name, client_species, client_synthesis):
    triples                                                 = species_querying(client_species, species_name, 0)
    print("OntoSpecies results: ", triples)
    if triples == None or triples == []:
        triples                                             = species_querying(client_synthesis, species_name, 0)

        if triples == None or triples == []:
            species                                         = Species(label=chemical_formula, altLabel=species_name)

        else:
            print("Success: ", triples[0]["Species"])
            try:
                species                                     = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
                species.altLabel.update(species_name)
            except:
                # not found matching instance in either ontospecies or ontomops blazegraph => make a new entry!
                print("Could not find IRI. \n")
                species                                     = Species(label=chemical_formula, altLabel={species_name})
    else:
        species                                             = Species.pull_from_kg(triples[0]["Species"], client_species, recursive_depth=-1)[0]
        species.altLabel.update(species_name) 
    return species

def instantiate_output(ccdc_number, chemical_formula, mop_names, client_mop, client_synthesis):
    mop_iri                         = mop_querying(client_mop, ccdc_number, chemical_formula, mop_names)
    if mop_iri == [] or mop_iri == None:
        mop_iri                     = mop_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
        try:
            mop                     = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"], client_synthesis, recursive_depth=-1)
            mop.altLabel.update(mop_names)
        except:
            if ccdc_number == "N/A":
                mop                 = MetalOrganicPolyhedron(hasMOPFormula=chemical_formula, mopAltLabel=mop_names)
            else:
                mop                 = MetalOrganicPolyhedron(hasCCDCNumber=ccdc_number, hasMOPFormula=chemical_formula, mopAltLabel=mop_names)
    #mop.push_to_kg(sparql_client, recursive_depth=-1)
    else: 
        mop                         = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"], client_mop, recursive_depth=-1)
        mop.altLabel.update(mop_names)
    chemical_output                 = ChemicalOutput(isRepresentedBy=mop)

    return mop, chemical_output

def chemicals_upload(input_path, output_path):
    client_synthesis                            = get_client("OntoSynthesisConnection")
    client_species                              = get_client("OntoSpeciesConnection") 
    client_mop                                  = get_client("OntoMOPConnection") 
    print("Input path: ", input_path)
    # read in CSV:
    # path: "../Data/first10_prompt2/10.1021_ic402428m.txt" 
    csv_class                                   = TextToCSV(input_path)
    csv_lines                                   = csv_class.read_and_clean_file()
    csv_out                                     = csv_class.extract_csv_entries(csv_lines)
    product_entries, other_entries              = csv_class.filter_by_synthesis_role(csv_out)
    print("product entries: ", product_entries)
    print("other entries: ", other_entries)
    for nr, line in enumerate(product_entries):
        # start with instantiating the products and instantiate a synthesis instance for each product.
        mop_name                                = line["Chemical Name"]
        print("CSV row: ",line)
        mop, chemical_output                    = instantiate_output(line["CCDC Number"], line["Chemical Formula"], mop_name, client_mop, client_synthesis)
        chemical_synthesis                      = ChemicalSynthesis()
        chemical_transformation                 = ChemicalTransformation(hasChemicalOutput=chemical_output, isDescribedBy=chemical_synthesis)
        components                              = [chemical_output, mop, chemical_synthesis, chemical_transformation]
        for component in components:
            push_component_to_kg(component, client_synthesis)
    last_prod                                               = ""
    for nr, line in enumerate(other_entries):
        print("CSV row: ",line)
        # query both OntoSynthesis and OntoSpecies for existing instances and reuse IRI if one exists.
        species_name                                        = [line["Chemical Name"], line["Alternative Names"], line["Chemical Formula"]]      
        species                                             = instantiate_input(line["Chemical Formula"], species_name, client_species, client_synthesis)           
        phase_component                                     = PhaseComponent(representsOccurenceOf=species)
        single_phase                                        = SinglePhase(isComposedOfSubsystem=phase_component)     
        material                                            = Material(thermodynamicBehaviour=single_phase)
        chemical_input                                      = ChemicalInput(referencesMaterial=material)
        # query if product exists:      
        syn_prod                                            = transformation_querying(client_synthesis, line["Synthesis Product"])     
        if syn_prod != last_prod:
            if syn_prod == []:
                print(f"not able to link with MOP: {species_name}")
            else:
                print("queried IRI: ", syn_prod)
                chemical_transformation                     = ChemicalTransformation.pull_from_kg(syn_prod[0]["chemicalTrans"], client_synthesis, recursive_depth=-1)   

        chemical_transformation[0].hasChemicalInput.add(chemical_input)
        components                                          = [phase_component, single_phase, material, chemical_input, species,chemical_transformation[0]]
        # Loop through each component and call the function
        for component in components:
            push_component_to_kg(component, client_synthesis)
        last_prod                                           = syn_prod
            
def push_component_to_kg(component, client, recursive_depth=-1):
    g_to_remove, g_to_add                                   = component.push_to_kg(client, recursive_depth)
# uploaded: 10.1021_ja 0104352.txt

def main():
    sparql_client_synthesis                                 = get_client("OntoSynthesisConnection")
    sparql_client_species                                   = get_client("OntoSpeciesConnection") 
    sparql_client_mop                                       = get_client("OntoMOPConnection") 
    #upload_vessels(sparql_client)
    #unit_upload(sparql_client)
    #chemicals_upload("../Data/first10_prompt22/10.1021_ja 0104352.txt", "")
    #another_object_of_one_concept = species.pull_from_kg('https://iri-of-the-object-of-interest', sparql_client, recursive_depth=-1)
    # read in JSON:
    SynthesisJson                               = read_json_file("../Data/first10_prompt52/10.1021_ja 0104352.json")["Synthesis"]
    data2                                       = SynthesisJson[0]
    print("json file: ", data2)
    mop_name                                    = data2["product name"]
    mop_ccdc                                    = data2["product CCDC number"]
    add_json                                    = data2["Add"]
    add1                                        = add_json[0]
    addedAmount                                 = add1["added chemical amount"]
    number, units                               = extract_numbers_and_units_add(addedAmount)
    heatchill_json                              = data2["HeatChill"]

    for heatchill_step in heatchill_json:
        # remove non numerical entries
        if heatchill_step["Target temperature"] == "—" or heatchill_step["Target temperature"] == "room temperature":
            continue
        # target temperature

    # Convert main elements to a DataFrame
    #main_df         = pd.DataFrame({k: [v] for k, v in data.items() if not isinstance(v, list)})
    # Print main DataFrame
    #print(main_df)
    # Convert nested lists to DataFrames
    #add_df          = pd.DataFrame(data['Add'])
    #heatchill_df    = pd.DataFrame(data['HeatChill'])
    #filter_df       = pd.DataFrame(data['Filter'])
    #stirr_df        = pd.DataFrame(data['Stirr'])
    #sonicate_df     = pd.DataFrame(data['Sonicate'])

    #OntoSyn         = "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl"





if __name__ == "__main__":
    main()

