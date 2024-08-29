# Import relevant packages
from __future__ import annotations
from twa.data_model.base_ontology import BaseOntology, BaseClass, ObjectProperty, DatatypeProperty
from twa.data_model.iris import TWA_BASE_URL
from twa.kg_operations import PySparqlClient
from typing import ClassVar
from pydantic import Field
from twa.kg_operations import PySparqlClient
from rdflib import Graph, URIRef, Literal
from rdflib.namespace import RDF
from typing import Optional
import uuid
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
    namespace:          ClassVar[str]           = 'OntoSpecies.owl'
    owl_versionInfo:    ClassVar[str]           = '0.0.1'
    rdfs_comment:       ClassVar[str]           = 'Your ontology'

class OntoMOPs(BaseOntology):
    # Below fields can be set up to provide metadata for your ontology
    base_url:           ClassVar[str]           = "https://www.theworldavatar.com/kg/"
    namespace:          ClassVar[str]           = 'ontomops/'
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

# ---- om2 classes: ----
# General:
# Measure used to set the correct type, IRI needs to be changed afterwards
# Notes: use set as default instead of None to ensure one can add with classname.property.add(classname)
class UnitOfMeasure(BaseClass):
    rdfs_isDefinedBy                    = OM2
class Measure(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = set()
    hasUnit                             : Optional[HasUnit[UnitOfMeasure]]                              = set()
# Duration:
class DurationUnit(BaseClass):
    rdfs_isDefinedBy                    = OM2
class DurationValue(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = set()
    hasUnit                             : Optional[HasUnit[DurationUnit]]                               = set()
class Duration(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                                   = set()
# Temperature:
class TemperatureUnit(BaseClass):
    rdfs_isDefinedBy                    = OM2
class Temperature(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                                   = set()
class TemperatureValue(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = set()
    hasUnit                             : Optional[HasUnit[TemperatureUnit]]                            = set()
# Temperature Rate:
class TemperatureChangeRateUnit(BaseClass):
    rdfs_isDefinedBy                    = OM2
class TemperatureRate(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasValue                            : Optional[HasValue[Measure]]                                   = set()
class HeatChillValue(BaseClass):
    rdfs_isDefinedBy                    = OM2
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = set()
    hasUnit                             : Optional[HasUnit[DurationUnit]]                               = set()
class ChemicalTransformation(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    isDescribedBy                       : Optional[IsDescribedBy[ChemicalSynthesis]]                    = set()
    hasChemicalOutput                   : Optional[HasChemicalOutput[ChemicalOutput]]                   = set()
class ChemicalSynthesis(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasSynthesisStep                    : Optional[HasSynthesisStep[SynthesisStep]]                     = set()
    hasFirstStep                        : Optional[HasFirstStep[SynthesisStep]]                         = set()
    hasEquipment                        : Optional[HasEquipment[LabEquipment]]                          = set()
    hasChemicalInput                    : Optional[HasChemicalInput[ChemicalInput]]                     = set()
    retrievedFrom                       : Optional[RetrievedFrom[Document]]                             = set()
class SynthesisStep(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasVessel                           : Optional[HasVessel[Vessel]]                                   = set()
    hasOrder                            : Optional[HasOrder[int]]                                       = set()
    hasExecutionPoint                   : Optional[HasExecutionPoint[ExecutionPoint]]                   = set()
    hasVesselEnvironment                : Optional[HasVesselEnvironment[VesselEnvironment]]             = set()
    hasStepDuration                     : Optional[HasStepDuration[Duration]]                           = set()
class Product(BaseClass):
    rdfs_isDefinedBy                    = OntoKin
class ChemicalOutput(Product):
    rdfs_isDefinedBy                    = OntoSyn
    # should be with OntoMop
    isRepresentedBy                     : Optional[IsRepresentedBy[MetalOrganicPolyhedron]]             = set()
    #hasSynthesisYield                   : HasSynthesisYield[SynthesisYield]
class SynthesisYield(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    hasYieldMass                        : Optional[HasYieldMass[Mass]]                                  = set()
class SynthesisReactant(BaseClass):
    rdfs_isDefinedBy                    = OntoReaction
class Add(SynthesisStep):       
    rdfs_isDefinedBy                    = OntoSyn
    isDropwise                          : Optional[IsDropwise[bool]]                                    = set()
    hasAddedChemicalInput               : Optional[HasAddedChemicalInput[ChemicalInput]]                = set()
class Filter(SynthesisStep):        
    rdfs_isDefinedBy                    = OntoSyn
    isWashedWith                        : Optional[IsWashedWith[Material]]                              = set()
    hasWashingSolvent                   : Optional[HasWashingSolvent[ChemicalInput]]                    = set()
    isRepeated                          : Optional[IsRepeated[int]]                                     = set()
class Dry(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
class Stir(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasStirringRate                     : Optional[HasStirringRate[float]]                              = set()
class Sonication(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
class HeatChill(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
    hasTargetTemperature                : Optional[HasTargetTemperature[Temperature]]                   = set()
    hasTemperatureRate                  : Optional[HasTemperatureRate[TemperatureRate]]                 = set()
    hasHeatChillDevice                  : Optional[HasHeatChillDevice[HeatChillDevice]]                 = set()
    hasVacuum                           : Optional[HasVacuum[bool]]                                     = set()
    isSealed                            : Optional[IsSealed[bool]]                                      = set()
    hasStirringSpeed                    : Optional[HasStirringSpeed[float]]                             = set()
class ExecutionPoint(SynthesisStep):
    rdfs_isDefinedBy                    = OntoSyn
class LabEquipment(BaseClass):
    rdfs_isDefinedBy                    = OntoLab
class VesselEnvironment(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    label                               : Label[str]
class Vessel(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn
class Species(BaseClass):
    rdfs_isDefinedBy                    = OntoSpecies
    label                               : Optional[Label[str]]                                          = set()
    altLabel                            : Optional[AltLabel[str]]                                       = set()
class PhaseComponent(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    representsOccurenceOf               : Optional[RepresentsOccurenceOf[Species]]                      = set()
    hasProperty                         : Optional[HasProperty[PhaseComponentConcentration]]            = set()
class ScalarValue(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeSystem
    hasUnitOfMeasure                    : Optional[HasUnitOfMeasure[UnitOfMeasure]]                     = set()
    hasNumericalValue                   : Optional[HasNumericalValue[float]]                            = set()
class PhaseComponentConcentration(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    hasValue                            : Optional[HasValue[ScalarValue]]                               = set()
class Composition(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    comprisesDirectly                   : Optional[ComprisesDirectly[PhaseComponentConcentration]]      = set()
class SinglePhase(BaseClass):
    rdfs_isDefinedBy                    = OntoCapePhaseSystem
    isComposedOfSubsystem               : Optional[IsComposedOfSubsystem[PhaseComponent]]               = set()
    hasComposition                      : Optional[HasComposition[Composition]]                         = set()
class Material(BaseClass):
    rdfs_isDefinedBy                    = OntoCapeMaterial
    thermodynamicBehaviour              : Optional[ThermodynamicBehaviour[SinglePhase]]                 = set()
class ChemicalInput(BaseClass):
    rdfs_isDefinedBy                    = OntoSyn
    referencesMaterial                  : Optional[ReferencesMaterial[Material]]                        = set()
class MetalOrganicPolyhedron(BaseClass):
    rdfs_isDefinedBy                    = OntoMOPs
    hasCCDCNumber                       : Optional[HasCCDCNumber[str]]                                  = set()
    hasMOPFormula                       : Optional[HasMOPFormula[str]]                                  = set()
    mopAltLabel                         : Optional[MopAltLabel[str]]                                    = set()
class HeatChillDevice(LabEquipment):
    rdfs_isDefinedBy                    = OntoSyn
# ---- provenance classes: ----
class Document(BaseClass):
    rdfs_isDefinedBy                    = BIBO
    doi                                 : Optional[Doi[str]]                                            = set()
# ---- potentially useless classes: ----
class Mass(BaseClass):
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
# OntoSynthesis datatype properties
class HasVacuum(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsSealed(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasStirringSpeed(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasOrder(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasExecutionPoint(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasVesselEnvironment(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsDropwise(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsRepeated(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoSyn
# om2 datatype properties: 
class HasNumericalValue(DatatypeProperty):
    rdfs_isDefinedBy                    = OM2
# simple knowledge organization ontology:
class AltLabel(DatatypeProperty):
    rdfs_isDefinedBy                    = SKOS
class Label(DatatypeProperty):
    rdfs_isDefinedBy                    = RDFS
# OntoMOPs datatype properties:
class HasCCDCNumber(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class HasMOPFormula(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
class MopAltLabel(DatatypeProperty):
    rdfs_isDefinedBy                    = OntoMOPs
# bibliography datatype properties:
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
class HasVessel(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasProduct(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class IsRepresentedBy(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSynthesisYield(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasYieldMass(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasStepDuration(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class YieldLimitingSpecies(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasAddedChemicalInput(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasReactantMaterialAmount(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasSolventMaterialAmount(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasEquipment(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasHeatChillDevice(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasTargetTemperature(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasTemperatureRate(ObjectProperty):
    rdfs_isDefinedBy                    = OntoSyn
class HasStirringRate(ObjectProperty):
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
class HasUnit(ObjectProperty):
    rdfs_isDefinedBy                    = OM2
class HasValue(ObjectProperty):
    rdfs_isDefinedBy                    = OM2
class HasUnitOfMeasure(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem
class HasProperty(ObjectProperty):
    rdfs_isDefinedBy                    = OntoCapeSystem

def change_property(instance_var, property_var, value_var, client, push=False):
    # Check if the attribute exists  
    if hasattr(instance_var, property_var):
        setattr(instance_var, property_var, value_var)
    else:
        raise AttributeError(f"'{type(instance_var).__name__}' object has no attribute '{property_var}'")
    if push:
        instance_var.push_to_kg(client, recursive_depth=-1)

def upload_predefined(client):
    vessel_ss_teflon                    = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_eb0f5942-d36b-47b1-86f0-725c1549fa2e", rdfs_label="Teflon-lined stainless-steel vessel")
    glass_vial                          = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_90589d23-44e8-4698-acdf-bee3e44df96f", rdfs_label="glass vial")
    quartz_tube                         = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_06304c23-7926-45d2-841d-690b5de16ed0", rdfs_label="quartz tube")
    round_bottom_flask                  = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_5a7d7ec9-44d5-4280-8467-f9f624374a9d", rdfs_label="round bottom flask")
    glass_scintilation_vial             = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_b67ea47b-7849-4aac-b0fd-e2715a4ac034", rdfs_label="glass scintillation vial")
    pyrex_tube                          = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_080ad74b-950d-4651-a87c-5aa96d5ffb52", rdfs_label="pyrex tube")
    undefined_vessel                    = Vessel(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/Vessel_183ad74b-950d-4631-a47c-5aa91d5ffb12", rdfs_label="N/A")
      

    degree_celsius                      = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius", rdfs_label="degree Celsius")
    kelvin                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin", rdfs_label="kelvin")
    degree_celsius_hour                 = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerHour", rdfs_label="degree Celsius per hour")
    degree_celsius_min                  = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerMinute-Time", rdfs_label="degree Celsius per minute")
    duration_h                          = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/hour", rdfs_label="hour")
    duration_day                        = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/day", rdfs_label="day")
    duration_s                          = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/second-Time", rdfs_label="second")
    temperature_rate_degs               = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerSecond-Time", rdfs_label="degree Celsius per second")
    mole_per_litre                      = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre", rdfs_label="mole per litre")
    grams                               = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/gram", rdfs_label="gram")
    mole                                = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/mole", rdfs_label="mole")
    mmole                               = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/millimole", rdfs_label="mmole") 
    mlitre                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/millilitre", rdfs_label="mlitre")
    # from TWA branch of OM:
    revolutions_per_minute              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/revolutionPerMinute-Time", rdfs_label="revolution per minute", rdfs_comment="Revolution per minute is a unit of rotational speed (or rotational frequency) for rotating machines.")
    unknown_unit                        = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/unknown", rdfs_label="N/A")

    instances                           = [vessel_ss_teflon, glass_vial, quartz_tube, round_bottom_flask, glass_scintilation_vial, pyrex_tube, degree_celsius_hour, kelvin, degree_celsius, degree_celsius_min, duration_day, duration_h, duration_s, temperature_rate_degs, mole_per_litre, revolutions_per_minute, grams, mole, mmole, mlitre, undefined_vessel, unknown_unit]
    push_component_to_kg(instances, client)

def extract_numbers_and_units(text, pattern_type):
    """patterns:
    add:        r'(\d*\.?\d+)\s*([a-zA-Z]+)'
    temp:       r'(\d*\.?\d+)\s*([^\d\s]+)', also used for rate
    """
    match pattern_type:
        case "add":
            pattern              = r'(\d*\.?\d+)\s*([a-zA-Z]+)'
        case "temp":
            pattern              = r'(\d*\.?\d+)\s*([^\d\s]+)'

    # Find all matches in the text
    matches             = re.findall(pattern, text)

    # Separate numbers and units
    numbers             = [float(match[0]) for match in matches]
    units               = [match[1] for match in matches]
    if numbers == []:
        numbers         = [0] 
    if units == []:
        units         = ["N/A"] 
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
        product_entries     = []
        other_entries       = []

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
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    query = f"""
        PREFIX om:      <https://www.theworldavatar.com/kg/ontomops/>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        SELECT ?MOPIRI
        WHERE {{
        ?MOPIRI a <https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedron>                        .
        VALUES ?Text {{"{CCDC_number}" "{mop_formula}" "{mop_name}"}}
        ?MOPIRI (<https://www.theworldavatar.com/kg/ontomops/hasMOPFormula>|<https://www.theworldavatar.com/kg/ontomops/mopAltLabel>|<https://www.theworldavatar.com/kg/ontomops/hasCCDCNumber>) ?Text .  
        }}
        GROUP BY ?MOPIRI"""
    out                     = client.perform_query(query)
    print("used query: ", query)
    print("MOp query result returned: ", out)
    return out

def transformation_querying(client, mop_name):
    print("mop_name: ", mop_name)
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
    out             = client.perform_query(query)
    print("\n ----- \n", out)
    return out

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

# alternative approach to unit upload -> additional query but does not require to upload them again.
def match_vessel(vessel_name, client): 

    match vessel_name:
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
        case _:     
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/Vessel_183ad74b-950d-4631-a47c-5aa91d5ffb12"
    vessel                                      = Vessel.pull_from_kg(vessel_iri, client,recursive_depth=-1)[0]
    return vessel

def get_unit(unit_name, client):
    print("unit_name: ", unit_name)
    match unit_name:
        case "°C" | "C" | "degC":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsius", client, recursive_depth=-1)[0]
        case "K" | "Kelvin":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/kelvin", client, recursive_depth=-1)[0]
        case "°C/h" | "C/h" | "degC/h" | "°C/hour" | "C/hour" | "degC/hour":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerHour", client, recursive_depth=-1)[0]
        case "°C/min" | "C/min" | "degC/min" | "°C/minute" | "C/minute" | "degC/minute":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/degreeCelsiusPerMinute-Time", client, recursive_depth=-1)[0]    
        case "hour" | "hours" | "h" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/hour", client, recursive_depth=-1)[0]    
        case "day" | "days" | "d" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/day", client, recursive_depth=-1)[0]    
        case "seconds" | "second" | "s" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/second-Time", client, recursive_depth=-1)[0]                      
        case "g" | "gram" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/gram", client, recursive_depth=-1)[0]                      
        case "mol" | "mole" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/mole", client, recursive_depth=-1)[0]                                  
        case "mmol" | "milimole" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/millimole", client, recursive_depth=-1)[0]  
        case "mL" | "mililitre" | "mL" | "ml"  :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/millilitre", client, recursive_depth=-1)[0]                                  
        case _: 
            print(f"Unit was not recognized. Check the following unit: {unit_name} \n")            
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/unknown", client, recursive_depth=-1)[0]                                  
       

    """
    mole_per_litre                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre", rdfs_label="mole per litre")
    revolutions_per_minute                      = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/revolutionPerMinute-Time", rdfs_label="revolutions per minute")
    """
    return unit
def heatchill_upload(client, heatchill_step):
    temp, temp_unit                             = extract_numbers_and_units(heatchill_step["targetTemperature"],"temp")
    heat_time, time_unit                        = extract_numbers_and_units(heatchill_step["heatCoolingTime"], "add")
    heat_rate, rate_unit                        = extract_numbers_and_units(heatchill_step["heatingCoolingRate"], "temp")
    print("temperature: ", temp, temp_unit)
    print("heatingrate: ", heat_rate, rate_unit)
    print("duration: ", heat_time, time_unit)
    # temperature 
    id_hash_value                               = str(uuid.uuid4())
    temperature_unit                            = get_unit(temp_unit[0], client) 
    temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
    target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetTemperature_{id_hash_value}", hasValue=temperature_value)

    # heat rate
    rate_unit                                   = get_unit(rate_unit[0], client) 
    rate_value                                  = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureRate_{id_hash_value}", hasNumericalValue=heat_rate[0], hasUnit=rate_unit)
    temperature_rate                            = TemperatureRate(hasValue=rate_value) 

    # duration 
    duration_unit                               = get_unit(time_unit[0], client) 
    duration_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/StepDuration_{id_hash_value}", hasNumericalValue=heat_time[0], hasUnit=duration_unit)
    duration                                    = Duration(hasValue=duration_value)

    # Vessel:
    vessel                                      = match_vessel(heatchill_step['usedVessel'], client)
    sealed                                      = heatchill_step["sealedVessel"]
    vacuum                                      = heatchill_step["underVacuum"]
    heatchill_device                            = heatchill_step["usedDevice"]
    # put everything together
    heat_chill                                  = HeatChill(hasVessel=vessel, hasStepDuration=duration, hasTargetTemperature=target_temperature, hasTemperatureRate=temperature_rate, hasVacuum=vacuum, isSealed=sealed, hasOrder=heatchill_step['stepNumber'])
    components = [temperature_value, target_temperature, temperature_value, temperature_rate, duration_value, duration, heat_chill]
    push_component_to_kg(components, client)
    return  heat_chill

def add_upload(add_step, synthesis_client, species_client):
    print("add step inside: ", add_step)
    added_amount                                        = add_step["addedChemicalAmount"]
    add_value, add_unit                                 = extract_numbers_and_units(added_amount, "add")
    vessel                                              = match_vessel(add_step['usedVessel'], synthesis_client)
    chemical_name                                       = add_step['addedChemicalName']
    # Vessel:
    species_name                                        = chemical_name     
    species                                             = instantiate_input(species_name, species_name, client_species=species_client, client_synthesis=synthesis_client) 
    # Initialize an empty list to hold `ScalarValue` instances
    scalar_values = []
    # Iterate over each pair of unit and value
    for unit, value in zip(add_unit, add_value):
        # Get the unit using the `get_unit` function
        print("unit and value: ", unit, value)
        unit_instance                                   = get_unit(unit, synthesis_client)
        
        # Create a `ScalarValue` instance and add it to the list
        scalar_value_instance                           = ScalarValue(hasNumericalValue=value, hasUnitOfMeasure=unit_instance)
        scalar_values.append(scalar_value_instance)

    phase_component_concentration                       = PhaseComponentConcentration(hasValue=set(scalar_values))
    phase_component                                     = PhaseComponent(representsOccurenceOf=species, hasProperty=phase_component_concentration)
    composition                                         = Composition(comprisesDirectly=phase_component_concentration)
    single_phase                                        = SinglePhase(isComposedOfSubsystem=phase_component, hasComposition=composition)     
    material                                            = Material(thermodynamicBehaviour=single_phase)
    chemical_input                                      = ChemicalInput(referencesMaterial=material)
    add_class                                           = Add(hasOrder=add_step['stepNumber'], hasVessel=vessel, hasAddedChemicalInput=chemical_input)
    components = [phase_component_concentration, phase_component, composition, single_phase, material, chemical_input, add_class]
    push_component_to_kg(components, synthesis_client)
    return add_class

def filter_upload(filter_step, synthesis_client):
    print("filter step: ", filter_step)
    filter_class                                        = Filter(hasOrder=filter_step["stepNumber"], isRepeated=filter_step["repetitions"])                                         
    components = [filter_class]
    push_component_to_kg(components, synthesis_client)
    return filter_class

def remove_na(input_candidate):
    if input_candidate == "N/A":
        return ""
    return input_candidate

def instantiate_input_old(chemical_formula, species_name, client_species, client_synthesis):
    # search the ontospecies and ontosynthesis blazegraphs for existing instances
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
        print("species rdf type: ", species.rdf_type)
    return species

def instantiate_input(chemical_formula, species_name, client_species, client_synthesis):
    # search the ontospecies and ontosynthesis blazegraphs for existing instances
    species_iri                                             = str(uuid.uuid4())
    triples                                                 = species_querying(client_species, species_name, 0)
    print("OntoSpecies results: ", triples)
    if triples == None or triples == []:
        triples                                             = species_querying(client_synthesis, species_name, 0)
        if triples == None or triples == []:
            species                                         = Species(label=chemical_formula, altLabel=species_name)
            # Ontospecies uses different base IRIs for rdf type and the actual instance IRI.
            species.instance_iri                            = f"http://www.theworldavatar.com/kb/ontospecies/Species_{species_iri}"
        else:
            print("Success: ", triples[0]["Species"])
            species                                         = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
            species.altLabel.update(species_name)

    else:
        species                                             = Species(label=chemical_formula, altLabel=species_name)
        species.instance_iri                                = triples[0]["Species"]
        print("species rdf type: ", species.rdf_type)
    return species

def instantiate_output(ccdc_number, chemical_formula, mop_names, client_mop, client_synthesis):
    # query for existing mops either in the onto mops ontology or if not in the onto synthesis ontology
    mop_iri                             = mop_querying(client_mop, ccdc_number, chemical_formula, mop_names)
    if mop_iri == [] or mop_iri == None:
        mop_iri                         = mop_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
        # pull the mop instance with the iri and update the alt labels. If it fails isntantiate a new MOP instance.

            # ccdc_number is of type int and "N/A" a string -> causes an error
    if ccdc_number == "N/A":
                mop                 = MetalOrganicPolyhedron(hasMOPFormula=chemical_formula, mopAltLabel=mop_names)
    else:
                mop                 = MetalOrganicPolyhedron(hasCCDCNumber=ccdc_number, hasMOPFormula=chemical_formula, mopAltLabel=mop_names)
    if mop_iri != [] and mop_iri != None:
        mop.instance_iri            = mop_iri[0]["MOPIRI"] 

    chemical_output                 = ChemicalOutput(isRepresentedBy=mop)

    return mop, chemical_output

def chemicals_upload(input_path, output_path):
    client_synthesis                            = get_client("OntoSynthesisConnection")
    client_species                              = get_client("OntoSpeciesConnection") 
    client_mop                                  = get_client("OntoMOPConnection") 
    print("Input path: ", input_path)
    # read in CSV:
    # path: "../Data/first10_prompt2/10.1021_ic402428m.txt" 
    filename                                    = os.path.basename(input_path)
    # Split the filename into the two parts using '_'
    number1, number2_with_extension             = filename.split('_')
    # Remove the '.txt' extension from the second part
    number2                                     = os.path.splitext(number2_with_extension)[0]
    # Combine the parts in the desired format
    doi = f"{number1}/{number2}"
    print("doi: ", doi)
    document                                    = Document(doi=doi)
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
        chemical_synthesis                      = ChemicalSynthesis(retrievedFrom=document) 
        chemical_transformation                 = ChemicalTransformation(hasChemicalOutput=chemical_output, isDescribedBy=chemical_synthesis)
        components                              = [chemical_output, mop, chemical_transformation]
        print("mop: ", mop)
        print(chemical_output)
        push_component_to_kg(components, client_synthesis)

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
                chemical_transformation                     = ChemicalTransformation.pull_from_kg(syn_prod[0]["chemicalTrans"], sparql_client=client_synthesis, recursive_depth=-1) 
                print("chemical transformation: ", chemical_transformation[0])
                chemical_synthesis_set                      = chemical_transformation[0].isDescribedBy  
                print("chemical synthesis: ", chemical_synthesis_set)
                # get IRI from set
                for synthesis in chemical_synthesis_set:
                    instance_iri = synthesis.instance_iri
                chemical_synthesis                          = ChemicalSynthesis.pull_from_kg(instance_iri, sparql_client=client_synthesis, recursive_depth=-1) 
        print(chemical_transformation)
        chemical_synthesis[0].hasChemicalInput.add(chemical_input)
        components                                          = [phase_component, single_phase, material, chemical_input, species, chemical_transformation, chemical_synthesis]
        # Loop through each component and call the function
        print("phase component: ", phase_component)
        push_component_to_kg(components, client_synthesis)
        last_prod                                           = syn_prod
            
def push_component_to_kg(instances:list, client, recursive_depth=-1):
    for instance in instances:
        print("instance: ", instance)
        try:
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)
        except:
            instance                                                = instance[0]
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)

# uploaded: 10.1021_ja 0104352.txt

def main():
    sparql_client_synthesis                                 = get_client("OntoSynthesisConnection")
    sparql_client_species                                   = get_client("OntoSpeciesConnection") 
    sparql_client_mop                                       = get_client("OntoMOPConnection") 
    #upload_predefined(sparql_client_synthesis)
    # 10.1039_C6DT02764D
    # 10.1021_ja0104352.txt
    chemicals_upload("../Data/first10_prompt22/10.1021_ja0104352.txt", "")
    # read in JSON:
    synthesis_json                                           = read_json_file("../Data/first10_prompt53/10.1021_ja0104352.json")["Synthesis"]
    print("actual full data: ", synthesis_json)
    for entry in synthesis_json:
        print("entry: ", entry)
    data2                                                   = synthesis_json[0]
    print("json file: ", data2)
    mop_name                                                = data2["productName"]
    mop_ccdc                                                = data2["productCCDCNumber"]
    transformation_iri                                      = transformation_querying(sparql_client_synthesis, mop_name=mop_name)
    if transformation_iri == []:
        transformation_querying(sparql_client_synthesis, mop_name=mop_ccdc)
    print("transformation iri", transformation_iri)  
    print("IRI: ", transformation_iri[0]["chemicalTrans"])
    print("full data: ", data2)
    step_data                                   = data2["steps"]
    step_list                                   = []
    for step_dat in step_data:
        if 'Add' in step_dat:
            add_class                               = add_upload(add_step=step_dat["Add"], synthesis_client=sparql_client_synthesis, species_client=sparql_client_species)
            step_list.append(add_class)

        elif 'HeatChill' in step_dat:
            heatchill_step                          = step_dat["HeatChill"]
            print("heatchill step: ", heatchill_step)
            if heatchill_step["targetTemperature"] == "—" or heatchill_step["targetTemperature"] == "room temperature" or heatchill_step["targetTemperature"] == "N/A":
                continue
            heat_class                              = heatchill_upload(sparql_client_synthesis, heatchill_step)
            step_list.append(heat_class)

        elif 'Filter' in step_dat:
            filter_class                            = filter_upload(step_dat["Filter"], sparql_client_synthesis)
            step_list.append(filter_class)

    chemical_transformation                     = ChemicalTransformation.pull_from_kg(transformation_iri[0]["chemicalTrans"], sparql_client_synthesis, recursive_depth=-1)   
    chemical_synthesis                          = ChemicalSynthesis(hasSynthesisStep=step_list) 
    chemical_transformation[0].isDescribedBy.add(chemical_synthesis)
    components = [chemical_synthesis, chemical_transformation]
    push_component_to_kg(components, sparql_client_synthesis)


    #OntoSyn         = "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl"





if __name__ == "__main__":
    main()

