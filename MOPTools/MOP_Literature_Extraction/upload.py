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
from OntoSyn_ontology import *
import uuid
import re
import os
import sys

PROCESSING_DIR = os.path.abspath(os.path.join(os.path.dirname(__file__), os.pardir))
# Add the processing directory to the system path
sys.path.append(PROCESSING_DIR)
from io import StringIO
import upload_utils as uputil
import utils
import kg_queries as kgq
import predefine_iris as preiri

def upload_predefined():
    """
    Uploads predefined ontology instances to the knowledge graph.

    This function initializes predefined entities related to chemistry, materials, 
    units of measurement, vessel environments, separation methods, and unknown placeholders. 
    These instances are then pushed to the knowledge graph to ensure consistency and 
    standardization in chemical synthesis data.

    Parameters:
        None

    Returns:
        None
    """
    # Initialize SPARQL clients for ontology connections
    filename_noext, subdir, client, client_species, client_mop  = uputil.start_upload("")
    # Define characteristic peaks for IR and NMR spectroscopy
    Ir_NA                               = CharacteristicPeak(instance_iri=preiri.IR_NA, rdfs_label="N/A", rdfs_comment="")
    Nmr_NA                              = CharacteristicPeak(instance_iri=preiri.NMR_NA,rdfs_label="N/A", rdfs_comment="")
    # Define species used in IR spectroscopy
    KBr                                 = Species(instance_iri=preiri.KBR, rdfs_label="KBr") # Potassium bromide

    # Define commonly referenced chemical elements
    hydrogen                            = Element(instance_iri=preiri.HYDROGEN, rdfs_label="Hydrogen")
    carbon                              = Element(instance_iri=preiri.CARBON, rdfs_label="Carbon")
    oxygen                              = Element(instance_iri=preiri.OXYGEN, rdfs_label="Oxygen")
    nitrogen                            = Element(instance_iri=preiri.NITROGEN, rdfs_label="Nitrogen")
    unknown_element                     = Element(instance_iri=preiri.UNKNOWN_ELEMENT, rdfs_label="N/A")  # Placeholder for unknown elements

    # Define commonly used vessel types in chemical synthesis
    vessel_ss_teflon                    = VesselType(instance_iri=preiri.VESSEL_SS_TEFLON, rdfs_label="Teflon-lined stainless-steel vessel")
    glass_vial                          = VesselType(instance_iri=preiri.GLASS_VIAL, rdfs_label="glass vial")
    quartz_tube                         = VesselType(instance_iri=preiri.QUARTZ_TUBE, rdfs_label="quartz tube")
    round_bottom_flask                  = VesselType(instance_iri=preiri.ROUND_BOTTOM_FLASK, rdfs_label="round bottom flask")
    glass_scintilation_vial             = VesselType(instance_iri=preiri.GLASS_SCINTILATION_VIAL, rdfs_label="glass scintillation vial")
    pyrex_tube                          = VesselType(instance_iri=preiri.PYREX_TUBE, rdfs_label="pyrex tube")
    schlenk                             = VesselType(instance_iri=preiri.SCHLENK, rdfs_label="schlenk flask")
    undefined_vessel                    = VesselType(instance_iri=preiri.UNDEFINED_VESSEL, rdfs_label="N/A")
      
    # Define units of measurement commonly used in synthesis
    degree_celsius                      = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS, rdfs_label="degree Celsius")
    kelvin                              = UnitOfMeasure(instance_iri=preiri.KELVIN, rdfs_label="kelvin")
    degree_celsius_hour                 = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS_HOUR, rdfs_label="degree Celsius per hour")
    degree_celsius_min                  = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS_MIN, rdfs_label="degree Celsius per minute")
    duration_h                          = UnitOfMeasure(instance_iri=preiri.DURATION_H, rdfs_label="hour")
    duration_day                        = UnitOfMeasure(instance_iri=preiri.DURATION_DAY, rdfs_label="day")
    duration_s                          = UnitOfMeasure(instance_iri=preiri.DURATION_S, rdfs_label="second")
    duration_week                       = UnitOfMeasure(instance_iri=preiri.DURATION_WEEK, rdfs_label="week")
    duration_min                        = UnitOfMeasure(instance_iri=preiri.DURATION_MIN, rdfs_label="minute")
    
    # Define additional temperature rates and chemical measurement units
    temperature_rate_degs               = UnitOfMeasure(instance_iri=preiri.TEMPERATURE_RATE_DEGS, rdfs_label="degree Celsius per second")
    mole_per_litre                      = UnitOfMeasure(instance_iri=preiri.MOLE_PER_LITRE, rdfs_label="mole per litre")
    grams                               = UnitOfMeasure(instance_iri=preiri.GRAMS, rdfs_label="gram")
    miligrams                           = UnitOfMeasure(instance_iri=preiri.MILI_GRAMS, rdfs_label="miligram")
    mole                                = UnitOfMeasure(instance_iri=preiri.MOLE, rdfs_label="mole")
    mmole                               = UnitOfMeasure(instance_iri=preiri.MMOLE, rdfs_label="mmole") 
    mlitre                              = UnitOfMeasure(instance_iri=preiri.MLITRE, rdfs_label="mlitre")
    # Define additional units related to reaction rates and volumes
    revolutions_per_minute              = UnitOfMeasure(instance_iri=preiri.REVOLUTIONS_PER_MINUTE, rdfs_label="revolution per minute", rdfs_comment="Revolution per minute is a unit of rotational speed (or rotational frequency) for rotating machines.")
    drop                                = UnitOfMeasure(instance_iri=preiri.DROP, rdfs_label="drop")
    unknown_unit                        = UnitOfMeasure(instance_iri=preiri.UNKNOWN_UNIT, rdfs_label="N/A")
    # Define vessel environments (gas atmospheres used in reactions)
    N2Atmo                              = VesselEnvironment(instance_iri=preiri.N2_ATMO, rdfs_label="N2 atmosphere")
    ArAtmo                              = VesselEnvironment(instance_iri=preiri.AR_ATMO, rdfs_label="Ar atmosphere")
    AirAtmo                             = VesselEnvironment(instance_iri=preiri.AIR_ATMO, rdfs_label="Air atmosphere")
    unknown_Atmo                        = VesselEnvironment(instance_iri=preiri.UNKNOWN_ATMO, rdfs_label="N/A")
    # Define placeholders for yield values and chemical inputs
    yield_value                         = Measure(instance_iri=preiri.YIELD_VALUE, hasNumericalValue=-1, hasUnit=unknown_unit)
    yield_instance                      = AmountOfSubstanceFraction(instance_iri=preiri.YIELD_INSTANCE, hasValue=yield_value)
    chemical_input                      = ChemicalInput(instance_iri=preiri.CHEMICAL_INPUT, rdfs_label="N/A")
    unknown_mop                         = MetalOrganicPolyhedron(rdfs_label="N/A", instance_iri=preiri.UNKNWON_MOP)
    # Define common separation techniques
    centrifuge                          = SeparationType(instance_iri=preiri.CENTRIFUGE, rdfs_label="centrifuge")
    column                              = SeparationType(instance_iri=preiri.COLUMN, rdfs_label="column")
    wash                                = SeparationType(instance_iri=preiri.WASH, rdfs_label="washing")
    extraction                          = SeparationType(instance_iri=preiri.EXTRACTION, rdfs_label="extraction")
    unknown_sep                         = SeparationType(instance_iri=preiri.UNKNOWN_SEP, rdfs_label="N/A")
    # Placeholder for undefined synthesis steps
    unknown_step                        = SynthesisStep(instance_iri=preiri.UNKNOWN_STEP, rdfs_comment="N/A")
    # Define percentage as a unit of measure
    percentage                          = UnitOfMeasure(instance_iri=preiri.PERCENTAGE, rdfs_label="percent")
    # List of all predefined instances to be uploaded to the knowledge graph
    instances                           = [yield_value, chemical_input, yield_value, unknown_step, yield_instance,
                                            unknown_mop, N2Atmo, column, extraction, unknown_sep, wash, centrifuge,
                                            ArAtmo, AirAtmo , unknown_Atmo, vessel_ss_teflon, glass_vial, quartz_tube, 
                                            round_bottom_flask, glass_scintilation_vial, pyrex_tube, degree_celsius_hour, 
                                            kelvin, degree_celsius, degree_celsius_min, duration_min, duration_day, duration_h, 
                                            duration_s, duration_week, temperature_rate_degs, mole_per_litre, revolutions_per_minute, 
                                            grams, miligrams, mole, mmole, mlitre, undefined_vessel, unknown_unit, drop, nitrogen, 
                                            hydrogen, carbon, oxygen, unknown_element, percentage, KBr, Ir_NA, Nmr_NA, schlenk]
    # Push all predefined instances to the knowledge graph
    uputil.push_component_to_kg(instances, client)

# alternative approach to unit upload -> additional query but does not require to upload them again.
def upload_inputChem(chemicals, synthesis_client, species_client):
    """
    Uploads chemical input data to a knowledge graph (KG) by processing chemical components and their concentrations.

    This function processes a list of chemicals, where each chemical contains a name and an optional amount. 
    It then instantiates corresponding `ChemicalInput` objects and uploads them to the KG.

    Args:
        chemicals (list): A list of dictionaries where each dictionary represents a chemical, containing:
            - "chemicalName" (str): The name of the chemical species.
            - "chemicalAmount" (str, optional): The amount of the chemical, expressed in a value-unit pair.
        synthesis_client (Client): The client used to interact with the synthesis knowledge graph.
        species_client (Client): The client used to interact with the species knowledge graph.

    Returns:
        ChemicalInput: The instantiated `ChemicalInput` object that references the material containing the uploaded chemical data.

    Raises:
        ValueError: If required keys (e.g., "chemicalName") are missing from the input data.

    Notes:
        - If no `chemicalAmount` is provided, a default value of `-1` and unit `"N/A"` is used.
        - If no species name is found, `"N/A"` is used as a fallback.
    """
    # Initialize lists to hold phase components and their concentrations
    phase_components                                        = []
    phase_component_concentrations                          = [] 
    # Iterate through each chemical entry in the input list
    for chemical in chemicals:
        species_name                                            = chemical["chemicalName"]  # Retrieve the species name or default to an empty list

        # Extract the numerical value and unit from the chemical amount, if provided
        if "chemicalAmount" in chemical:
            add_value, add_unit                                 = uputil.extract_numbers_and_units(chemical["chemicalAmount"], "add")
        else: 
            add_value                                           = [-1]                      # Default value when no chemical amount is provided
            add_unit                                            = ["N/A"]                   # Default unit when no chemical amount is provided
        # Use "N/A" as a fallback if no species name is provided
        if species_name == []:
            species_name                                        = ["N/A"]
        # Instantiate the species object for the chemical input
        species                                                 = instantiate_input(species_name[0], species_name, client_species=species_client, client_synthesis=synthesis_client) 
        print("species name: ", species_name)
        # Initialize an empty list to store `ScalarValue` instances (amounts with units)
        scalar_values                                           = []
        # Iterate through extracted unit-value pairs and create ScalarValue instances
        for unit, value in zip(add_unit, add_value):
            print("unit and value: ", unit, value)
            # Retrieve or instantiate the unit object
            unit_instance                                       = uputil.get_unit(unit, synthesis_client)
            # Create a `ScalarValue` instance representing the quantity of the chemical
            scalar_value_instance                               = ScalarValue(hasNumericalValue=value, hasUnitOfMeasure=unit_instance)
            # Append the `ScalarValue` instance to the list
            scalar_values.append(scalar_value_instance)
            # Push the scalar value to the knowledge graph
            scalar_value_instance.push_to_kg(synthesis_client, -1)
        # Create a `PhaseComponentConcentration` instance to store the concentrations of the species
        phase_component_concentration                           = PhaseComponentConcentration(hasValue=set(scalar_values))
        phase_component_concentrations.append(phase_component_concentration)
        # Create a `PhaseComponent` instance linking the species and its concentration
        phase_component                                         = PhaseComponent(representsOccurenceOf=species, hasProperty=phase_component_concentration)
        phase_components.append(phase_component)
    # Define the overall composition of the chemical mixture
    composition                                                 = Composition(comprisesDirectly=phase_component_concentrations)
    # Create a `SinglePhase` instance
    single_phase                                                = SinglePhase(isComposedOfSubsystem=phase_components, hasComposition=composition)     
    # Define the material containing the phase data
    material                                                    = Material(thermodynamicBehaviour=single_phase)
    # Create a `ChemicalInput` instance to reference the material
    chemical_input                                              = ChemicalInput(referencesMaterial=material)
    print("species names: ", species.rdfs_label) 
    # Prepare a list of all created instances for bulk upload
    components = [phase_component_concentration, phase_component, composition, single_phase, material, chemical_input]
    # Upload the instantiated objects to the knowledge graph
    uputil.push_component_to_kg(components, synthesis_client)
    return chemical_input


def standard_step_upload(standard_input, vessel_list, chemicals_list, synthesis_client, species_client):
    """
    Uploads a standard synthesis step to the knowledge graph (KG) based on the given input.

    This function processes different types of synthesis steps such as Sonicate, Add, HeatChill, Filter, Stir, 
    Crystallization, Dry, Evaporate, Transfer, Dissolve, and Separate. Each step is instantiated as a corresponding 
    class and uploaded to the KG.

    Args:
        standard_input (dict): A dictionary containing synthesis step data.
        vessel_list (list): A list of vessels used in the synthesis.
        chemicals_list (list): A list of chemical inputs used in the synthesis.
        synthesis_client (Client): The client used to interact with the synthesis knowledge graph.
        species_client (Client): The client used to interact with the species knowledge graph.

    Returns:
        Tuple: (Instantiated step object, updated vessel_list, updated chemicals_list).
    """
    # Case: Sonicate step
    if "Sonicate" in standard_input:
        standard_step                                       = standard_input["Sonicate"]
        # Preprocess step to extract duration, vessel, and atmosphere
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step,
                                                                                                                synthesis_client, vessel_list)
        # Create Sonicate instance
        sonication                                          = Sonicate(hasStepDuration=duration,
                                                                        hasOrder=standard_step["stepNumber"],
                                                                        hasVessel=vessel, 
                                                                        hasVesselEnvironment=atmosphere)
        # Push the components to the KG
        components                                          = [duration_value, duration, vessel, sonication]
        uputil.push_component_to_kg(components, synthesis_client)
        return sonication, vessel_list, chemicals_list
    # Case: Add step
    elif "Add" in standard_input:
        standard_step                                           = standard_input["Add"]
        # Preprocess step to extract duration, vessel, and atmosphere
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step,
                                                                                                                   synthesis_client, vessel_list)
        # Upload added chemical input or retrieve a default chemical input
        if standard_step["addedChemical"] != []:
            chemical_input                                      = upload_inputChem(standard_step["addedChemical"], synthesis_client, species_client)
        else:
            chemical_input                                      = ChemicalInput.pull_from_kg(f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74",
                                                                                              synthesis_client, recursive_depth=-1)
        # Create Add instance
        add_class                                               = Add(hasOrder=standard_step['stepNumber'],
                                                                    hasStepDuration=duration,
                                                                    hasVessel=vessel, 
                                                                    hasAddedChemicalInput=chemical_input, 
                                                                    isStirred=standard_step['stir'], 
                                                                    hasTargetPh=float(standard_step['targetPH']), 
                                                                    isLayered=standard_step["isLayered"], 
                                                                    hasVesselEnvironment=atmosphere,
                                                                    rdfs_comment=standard_step['comment'])  
        # Push the components to the KG
        components = [add_class, vessel, duration, duration_value]
        uputil.push_component_to_kg(components, synthesis_client)
        # Update the chemicals list
        chemicals_list.append(chemical_input)
        return add_class, vessel_list, chemicals_list
    # Case: HeatChill step
    elif "HeatChill" in standard_input:
        standard_step                                           = standard_input["HeatChill"]
        # Preprocess step to extract duration, vessel, and atmosphere
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        # Extract target temperature
        if standard_step["targetTemperature"] == "room temperature":
            temp                                    = [25.0]                # Default room temperature
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["targetTemperature"],"temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        # Extract heating/cooling rate
        heat_rate, rate_unit                        = uputil.extract_numbers_and_units(standard_step["heatingCoolingRate"], "temp")
        rate_unit                                   = uputil.get_unit(rate_unit[0], synthesis_client) 
        device                                      = HeatChillDevice(rdfs_label=standard_step["usedDevice"])
        print("heatingrate: ", heat_rate, rate_unit)
        # temperature 
        id_hash_value                               = str(uuid.uuid4())
        # Instantiate temperature and rate measures
        temperature_value                           = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureValue_{id_hash_value}",
            hasNumericalValue=temp[0], 
            hasUnit=temperature_unit)
        target_temperature                          = Temperature(
            instance_iri=f"{preiri.ONTOSYN_BASE}TargetTemperature_{id_hash_value}", 
            hasValue=temperature_value)
        rate_value                                  = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureRate_{id_hash_value}", 
            hasNumericalValue=heat_rate[0], 
            hasUnit=rate_unit)
        

        temperature_rate                            = TemperatureRate(hasValue=rate_value) 
        # Create HeatChill instance
        heat_chill                                  = HeatChill(
            hasVessel=vessel, hasHeatChillDevice=device,hasVesselEnvironment=atmosphere, 
            hasStepDuration=duration, hasTargetTemperature=target_temperature, 
            hasTemperatureRate=temperature_rate, hasVacuum=standard_step["underVacuum"], 
            isSealed=standard_step["sealedVessel"], IsStirred=standard_step['stir'], 
            hasOrder=standard_step['stepNumber'], rdfs_comment=standard_step['comment'])
        components = [heat_chill]
        # Push the components to the KG
        uputil.push_component_to_kg(components, synthesis_client) 
        return heat_chill, vessel_list, chemicals_list
    
    # Case: Filter step
    elif "Filter" in standard_input:
        standard_step                                       = standard_input["Filter"] 
        # Find or instantiate vessel
        new_vessel                                          = True
        for ves in vessel_list:
            if ves.rdfs_label == standard_step["usedVesselName"]:
                    vessel                                  = ves
                    new_vessel                              = False
        # instantiate new vessel if none matches
        if new_vessel:
            vessel_type                                     = uputil.match_vessel(standard_step['usedVesselType'], synthesis_client)
            vessel                                          = Vessel(rdfs_label=standard_step["usedVesselName"], hasVesselType=vessel_type)
            vessel_list.append(vessel)
        # Match atmosphere
        atmosphere                                          = uputil.match_atmosphere(standard_step["atmosphere"], synthesis_client)
        # Upload washing solvent or retrieve a default chemical input
        if standard_step["washingSolvent"] != []:
            chemical_input                                  = upload_inputChem(standard_step["washingSolvent"], synthesis_client, species_client)
        else:
            chemical_input                                  = ChemicalInput.pull_from_kg(f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        # Create Filter instance
        filter_class                                        = Filter(hasOrder=standard_step["stepNumber"], hasVesselEnvironment=atmosphere, isRepeated=standard_step["numberOfFiltrations"], isVacuumFiltration=standard_step["vacuumFiltration"],hasWashingSolvent=chemical_input, rdfs_comment=standard_step["comment"], hasVessel=vessel)  
        # Push the components to the KG
        components = [filter_class]
        uputil.push_component_to_kg(components, synthesis_client)
        # Update the chemicals list
        chemicals_list.append(chemical_input)
        return filter_class, vessel_list, chemicals_list
    # Case: Stir step
    elif "Stir" in standard_input:
        standard_step                                       = standard_input["Stir"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        # Extract temperature
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]                # Default room temperature
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        # Instantiate temperature measure
        temperature_value                           = Measure(instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"{preiri.ONTOSYN_BASE}StirTemperature_{id_hash_value}", hasValue=temperature_value)
        # Create Stir instance
        stir                                        = Stir(hasStepDuration=duration, isWait=standard_step["wait"], hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasStirringTemperature=target_temperature)
        # Push the components to the KG
        components                                  = [stir]
        uputil.push_component_to_kg(components, synthesis_client)
        return stir, vessel_list, chemicals_list
    # Case: Crystallization step
    elif "Crystallization" in standard_input:
        standard_step                                       = standard_input["Crystallization"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(
            standard_step, 
            synthesis_client, 
            vessel_list)
        # Extract target temperature
        if standard_step["targetTemperature"] == "room temperature" or standard_step["targetTemperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["targetTemperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        # Instantiate temperature measure
        temperature_value                           = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureValue_{id_hash_value}",
            hasNumericalValue=temp[0], 
            hasUnit=temperature_unit)
        target_temperature                          = Temperature(
            instance_iri=f"{preiri.ONTOSYN_BASE}TargetTemperature_{id_hash_value}", 
            hasValue=temperature_value)
        # Create Crystallization instance
        crystallization                             = Crystallize(
            hasOrder=standard_step["stepNumber"], hasVesselEnvironment=atmosphere, hasVessel=vessel, 
            hasStepDuration=duration, rdfs_comment=standard_step["comment"], hasCrystallizationTargetTemperature=target_temperature)
        components                                  = [crystallization]
        uputil.push_component_to_kg(components, synthesis_client)
        return crystallization, vessel_list, chemicals_list
    # Case: Dry step
    elif "Dry" in standard_input:
        standard_step                                       = standard_input["Dry"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(
            standard_step, synthesis_client, vessel_list)
        # Extract drying temperature
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
        # Extract drying agent if available
        if standard_step["dryingAgent"] != []:
            chemical_input                          = upload_inputChem(standard_step["dryingAgent"], synthesis_client, species_client)
        else:
            chemical_input                          = ChemicalInput.pull_from_kg(
                f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", 
                synthesis_client, 
                recursive_depth=-1)[0]
        
        chemicals_list.append(chemical_input)
        # Instantiate drying temperature measure
        temperature_value                           = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureValue_{id_hash_value}",
            hasNumericalValue=temp[0], 
            hasUnit=temperature_unit, 
            rdfs_comment=standard_step["temperature"])
        # Instantiate drying temperature
        drying_temperature                          = Temperature(
            instance_iri=f"{preiri.ONTOSYN_BASE}DryingTemperature_{id_hash_value}", 
            hasValue=temperature_value)
        # Instantiate drying pressure
        drying_pressure                             = Pressure(
            instance_iri=f"{preiri.ONTOSYN_BASE}DryingPressure_{id_hash_value}", 
            rdfs_label=standard_step["pressure"])
        # Create Dry instance
        dry                                         = Dry(
            hasStepDuration=duration, hasVesselEnvironment=atmosphere, 
            hasOrder=standard_step["stepNumber"], hasVessel=vessel, 
            hasDryingPressure=drying_pressure,hasDryingTemperature=drying_temperature, 
            rdfs_comment=standard_step["comment"])
        components                                  = [dry]
        # Push the components to the KG
        uputil.push_component_to_kg(components, synthesis_client)
        return dry, vessel_list, chemicals_list
    # Case: Evaporate step
    elif "Evaporate" in standard_input:
        standard_step                                       = standard_input["Evaporate"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(
            standard_step, synthesis_client, vessel_list)
        # Extract evaporation temperature
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
        
        # Instantiate temperature measure
        temperature_value                           = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}TemperatureValue_{id_hash_value}",
            hasNumericalValue=temp[0], 
            hasUnit=temperature_unit, 
            rdfs_comment=standard_step["temperature"])
        evap_temperature                            = Temperature(
            instance_iri=f"{preiri.ONTOSYN_BASE}EvaporationTemperature_{id_hash_value}", 
            hasValue=temperature_value)
        # Instantiate pressure
        evap_pressure                               = Pressure(
            instance_iri=f"{preiri.ONTOSYN_BASE}EvaporationPressure_{id_hash_value}", 
            rdfs_label=standard_step["pressure"])
        
        if standard_step["removedSpecies"] != []:
            chemical_input                          = upload_inputChem(standard_step["removedSpecies"], synthesis_client, species_client)
        else:
            chemical_input                          = ChemicalInput.pull_from_kg(f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        
        # add chemical input so it can be properly connected once all inputs are established
        chemicals_list.append(chemical_input)
        # Instantiate volume 
        vol, vol_unit                               = uputil.extract_numbers_and_units(standard_step["targetVolume"], "temp")
        volume_unit                                 = uputil.get_unit(vol_unit[0], synthesis_client) 
        volume_value                                = Measure(instance_iri=f"{preiri.ONTOSYN_BASE}VolumeValue_{id_hash_value}",hasNumericalValue=vol[0], hasUnit=volume_unit, rdfs_comment=standard_step["targetVolume"])
        target_volume                               = Volume(instance_iri=f"{preiri.ONTOSYN_BASE}TargetVolume_{id_hash_value}", hasValue=volume_value)
        evaporate                                   = Evaporate(
            hasStepDuration=duration, isEvaporatedToVolume=target_volume, 
            removesSpecies=chemical_input, hasRotaryEvaporator=standard_step["rotaryEvaporator"],
            hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, 
            hasEvaporationTemperature=evap_temperature, hasEvaporationPressure=evap_pressure, rdfs_comment=standard_step["comment"])  
        components                                  = [evaporate]
        # Push the components to the KG
        uputil.push_component_to_kg(components, synthesis_client)
        return evaporate, vessel_list, chemicals_list
    # Case: Transfer step
    elif "Transfer" in standard_input:
        standard_step                               = standard_input["Transfer"]
        # target vessel
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        # Target vessel determination
        if vessel.rdfs_label != standard_step["targetVesselName"]:
            targetvessel_type                       = uputil.match_vessel(standard_step["targetVesselType"], synthesis_client)
            targetvessel                            = Vessel(rdfs_label=standard_step["targetVesselName"], hasVesselType=targetvessel_type)
        else: 
            targetvessel                            = vessel
        # Extract transferred amount
        vol, vol_unit                               = uputil.extract_numbers_and_units(standard_step["transferedAmount"], "temp")
        volume_unit                                 = uputil.get_unit(vol_unit[0], synthesis_client) 
        # Instantiate volume measure
        volume_value                                = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}VolumeValue_{id_hash_value}",
            hasNumericalValue=vol[0], 
            hasUnit=volume_unit)
        target_volume                               = Volume(
            instance_iri=f"{preiri.ONTOSYN_BASE}TargetVolume_{id_hash_value}", 
            hasValue=volume_value)
        # Create Transfer instance
        transfer                                    = Transfer(
            hasStepDuration=duration, 
            hasTransferedAmount=target_volume, 
            isLayeredTransfer=standard_step["isLayered"], 
            hasVesselEnvironment=atmosphere, 
            hasOrder=standard_step["stepNumber"], 
            hasVessel=vessel, 
            isTransferedTo=targetvessel, 
            rdfs_comment=standard_step["comment"])
        components                                  = [transfer]
        uputil.push_component_to_kg(components, synthesis_client)
        return transfer, vessel_list, chemicals_list
    # Case: Dissolve step
    elif "Dissolve" in standard_input:
        standard_step                                                   = standard_input["Dissolve"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        # Extract solvent if available
        if standard_step["solvent"] != []:
            chemical_input                                              = upload_inputChem(standard_step["solvent"], synthesis_client, species_client)
        else:
            chemical_input                                              = ChemicalInput.pull_from_kg(f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        # Create Dissolve instance
        dissolve                                                        = Dissolve(hasStepDuration=duration, hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasSolventDissolve=chemical_input, rdfs_comment=standard_step["comment"])
        # Push the components to the KG
        components                                                      = [dissolve]
        chemicals_list.append(chemical_input)
        uputil.push_component_to_kg(components, synthesis_client)
        return dissolve, vessel_list, chemicals_list
    # Case: Separate step
    elif "Separate" in standard_input:
        standard_step                           = standard_input["Separate"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(
            standard_step,
            synthesis_client, 
            vessel_list)
        # Extract separation solvent if available
        if standard_step["solvent"] != []:
            chemical_input                                              = upload_inputChem(
                standard_step["solvent"], 
                synthesis_client, 
                species_client)
        else:
            chemical_input                                              = ChemicalInput.pull_from_kg(
                f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", 
                synthesis_client, 
                recursive_depth=-1)[0]
        # Extract separation type
        separation_type                                                 = uputil.match_separation(
            standard_step["separationType"], 
            synthesis_client)
        # Create Separate instance
        separate                                                        = Separate(
            hasStepDuration=duration, 
            isSeparationType=separation_type, 
            hasVesselEnvironment=atmosphere, 
            hasOrder=standard_step["stepNumber"], 
            hasVessel=vessel, 
            hasSeparationSolvent=chemical_input ,
            rdfs_comment=standard_step["comment"])
        # Push the components to the KG
        components                                                      = [separate]
        chemicals_list.append(chemical_input)
        uputil.push_component_to_kg(components, synthesis_client)
        return separate, vessel_list, chemicals_list
    # Default case: If no valid step type is found
    else:
        standard_class                                                  = SynthesisStep.pull_from_kg(
            f"{preiri.ONTOSYN_BASE}SynthesisStep_ddb7ceda-13d2-461a-a63e-9e7df3116882", 
            synthesis_client, 
            recursive_depth=-1)[0]
        return standard_class, vessel_list, chemicals_list

def instantiate_input(chemical_formula, species_name, client_species, client_synthesis):
    """
    Instantiates or retrieves a chemical species from the knowledge graph (KG).

    This function checks both the OntoSpecies and OntoSynthesis knowledge graphs for an existing species instance.
    If found, it retrieves and updates the species. If not, a new species instance is created.

    Args:
        chemical_formula (str): The molecular formula of the chemical species.
        species_name (list): A list of alternative names or labels for the species.
        client_species (Client): The client used to query the OntoSpecies knowledge graph.
        client_synthesis (Client): The client used to query the OntoSynthesis knowledge graph.

    Returns:
        Species: An instance of the Species class, either retrieved from the KG or newly instantiated.

    Notes:
        - The function first attempts to find the species in OntoSynthesis.
        - If not found, it queries OntoSpecies.
        - If the species does not exist in either KG, a new instance is created with a unique IRI.
        - If an existing species is found but lacks some labels, they are updated in the KG.
    """
    # Generate a unique identifier for the species instance
    species_iri                                             = str(uuid.uuid4())
    # Ensure the chemical formula and species names have proper formatting
    species_name                                            = uputil.replace_character(species_names=species_name)
    chemical_formula                                        = uputil.replace_character(species_names=[chemical_formula])[0]
    # Query OntoSynthesis KG to check if the species already exists
    triples                                                 = kgq.species_querying_ontosyn(client_synthesis, species_name)

    print("OntoSpecies results: ", triples)
    # If no results are found in OntoSynthesis, query OntoSpecies KG
    if triples == None or triples == []:
        triples                                             = kgq.species_querying(client_species, species_name)
        # If the species is still not found, create a new instance
        if triples == None or triples == []:
            species                                         = Species(label=chemical_formula, altLabel=species_name)
            # Generate an instance IRI following the OntoSpecies format
            species.instance_iri                            = f"{preiri.SPECIES_NEW_BASE}Species_{species_iri}"
        else:
            try:
                # Retrieve species information from OntoSpecies KG
                species                                     = Species(
                    instance_iri=triples[0]["Species"] ,
                    label=chemical_formula, 
                    altLabel=species_name)
            except:
                # If an existing species IRI is found but has different labels, update it in OntoSynthesis KG
                species                                     = Species.pull_from_kg(
                    triples[0]["Species"], 
                    client_synthesis, 
                    recursive_depth=-1)[0]
                # Ensure the alternative labels are updated in the KG to avoid duplicate entries
                species                                     = uputil.update_alt_label(species, species_name=species_name)
            

    else:
        # Species found in OntoSynthesis KG, retrieve and update it
        print("Success: ", triples[0]["Species"])
        # Pull the species from the KG
        species                                             = Species.pull_from_kg(
            triples[0]["Species"], 
            client_synthesis, 
            recursive_depth=-1)[0]
        # Update alternative labels if they are missing to prevent duplicate species
        species                                             = uputil.update_alt_label(species, species_name=species_name)
    return species

def instantiate_output(ccdc_number, chemical_formula, mop_names, yield_str, client_mop, client_synthesis):
    """
    Instantiates or retrieves a Metal-Organic Polyhedron (MOP) and its corresponding yield from the knowledge graph (KG).

    This function searches for an existing MOP in both OntoMOPs and OntoSynthesis KGs using the provided CCDC number, 
    chemical formula, and alternative MOP names. If a match is found, it updates the existing entry; otherwise, 
    a new entry is created or linked to an "unknown" MOP entry if no match is available.

    Args:
        ccdc_number (str): The CCDC (Cambridge Crystallographic Data Centre) number of the MOP.
        chemical_formula (str): The molecular formula of the MOP.
        mop_names (list): A list of alternative names for the MOP.
        yield_str (str): The yield percentage of the synthesis step.
        client_mop (Client): The client used to query the OntoMOPs knowledge graph.
        client_synthesis (Client): The client used to query the OntoSynthesis knowledge graph.

    Returns:
        tuple: A tuple containing:
            - ChemicalOutput: The instantiated or retrieved chemical output object.
            - AmountOfSubstanceFraction: The instantiated or retrieved yield instance.

    Notes:
        - The function first searches for an existing MOP in OntoSynthesis.
        - If not found, it queries OntoMOPs.
        - If the MOP is still not found, it is linked to a predefined "unknown" MOP entry.
        - The yield value is extracted, converted to a float, and linked to the chemical output.
    """
    # Replace any special characters in MOP names and chemical formula for consistency
    mop_names                               = uputil.replace_character(mop_names)
    chemical_formula                        = uputil.replace_character([chemical_formula])[0]
    # Query OntoSynthesis for an existing MOP using the CCDC number, formula, and alternative names 
    mop_iri                                 = kgq.mop_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
    # If no MOP is found in OntoSynthesis, search OntoMOPs
    if mop_iri == []:
        mop_iri                             = kgq.mop_querying(client_mop, ccdc_number, chemical_formula, mop_names)
        # If the MOP is still not found, link to a predefined "unknown" MOP entry
        if mop_iri == []:
            # couldn't link with a mop, instantiate unknown mop to indicate failed linkage:
            mop                             = MetalOrganicPolyhedron.pull_from_kg(
                f"{preiri.MOPS_BASE}MetalOrganicPolyhedra_1d981ba2-4072-47ef-9ecd-b9f5cc06a50a", 
                client_synthesis, 
                recursive_depth=-1)[0]
        else:
            # Attempt to retrieve the MOP from KG, handling possible failures in pulling data
            try:
                mop                             = MetalOrganicPolyhedron(
                    instance_iri=mop_iri[0]["MOPIRI"], 
                    hasCCDCNumber=ccdc_number, 
                    hasMOPFormula=chemical_formula, 
                    altLabel=mop_names)
            except:
                # If retrieval fails, pull the MOP again and update alternative labels if necessary
                mop                             = MetalOrganicPolyhedron.pull_from_kg(
                    mop_iri[0]["MOPIRI"], 
                    client_synthesis,
                    recursive_depth=-1)[0]
                mop                             = uputil.update_alt_label(mop, mop_names)
    else:
        # If MOP exists in OntoSynthesis, retrieve and update labels if necessary
        mop                                     = MetalOrganicPolyhedron.pull_from_kg(
            mop_iri[0]["MOPIRI"], 
            client_synthesis,
            recursive_depth=-1)[0]
        mop                                     = uputil.update_alt_label(mop, mop_names)
    # Instantiate or retrieve yield value
    uuid_id                                     = str(uuid.uuid4())
    # Retrieve the unit of measurement (percent) from KG
    unit                                        = UnitOfMeasure.pull_from_kg(
        preiri.PERCENTAGE, 
        client_synthesis, 
        recursive_depth=-1)[0]
    print("yield number", yield_str)
    # Remove percentage sign if present
    yield_str                                   = yield_str.replace('%', '')
    # Handle missing yield values by linking to a predefined "unknown" yield instance
    if yield_str == "-1" or yield_str == "N/A":
        yield_instance                          = AmountOfSubstanceFraction.pull_from_kg(
            f"{preiri.ONTOSYN_BASE}Yield_3ed5e18b-5206-405d-ada0-382071f73f74", 
            client_synthesis, 
            recursive_depth=-1)[0]
        print("unknown yield: ", yield_instance)
    else:
        # Attempt to convert the yield value to a float and create a new instance
        try:
            yield_val                               = float(yield_str)
            yield_value                             = Measure(instance_iri=f"{preiri.ONTOSYN_BASE}YieldValue_{uuid_id}", hasNumericalValue=yield_val, hasUnit=unit)
            yield_instance                          = AmountOfSubstanceFraction(
                instance_iri=f"{preiri.ONTOSYN_BASE}Yield_{uuid_id}", 
                hasValue=yield_value)
        except:
            # If conversion fails, link to the predefined "unknown" yield instance
            yield_instance                          = AmountOfSubstanceFraction.pull_from_kg(
                f"{preiri.ONTOSYN_BASE}Yield_3ed5e18b-5206-405d-ada0-382071f73f74", 
                client_synthesis, 
                recursive_depth=-1)[0]
        
    # output chemical
    output_names                                = []
    print("yield: ", yield_instance)
    # Construct a list of alternative names for the chemical output
    output_names.append(chemical_formula)
    output_names.append(ccdc_number)
    for mop_name in mop_names:
        output_names.append(mop_name)
    # Query OntoSynthesis for an existing chemical output entry    
    output_iri                                  = kgq.chemicalOutput_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
    if output_iri == []:
        print("output names3: ", output_names)
        chemical_output                         = ChemicalOutput(isRepresentedBy=mop, altLabel=output_names)
    else: 
        # Retrieve and update chemical output entry in KG
        chemical_output                         = ChemicalOutput.pull_from_kg(
            output_iri[0]["chemicalOutput"], 
            client_synthesis, 
            recursive_depth=-1)[0]
        uputil.update_alt_label(chemical_output, output_names)
        chemical_output.isRepresentedBy.add(mop)
    return chemical_output, yield_instance 

def chemicals_upload(input_path, output_path):
    """
    Uploads chemical synthesis data from a JSON file to a knowledge graph (KG).

    This function processes input chemicals, output chemicals, and synthesis-related metadata, 
    linking them to existing knowledge graph entities or creating new ones as needed.

    Args:
        input_path (str): The path to the JSON file containing synthesis procedures.
        output_path (str): The output path (not used in this function, but kept for potential future use).

    Returns:
        None: The function uploads data to the knowledge graph but does not return a value.

    Notes:
        - Reads synthesis procedures from a JSON file.
        - Extracts general information such as DOI and retrieves or creates the corresponding document entity.
        - Processes input chemicals and output chemicals, linking them to the synthesis.
        - Checks for existing chemical transformations and updates them if they exist.
    """
    # Print the input path for debugging purposes
    print("input path: ", input_path)
    # Initialize the SPARQL clients for different ontologies (OntoSynthesis, OntoSpecies, OntoMOPs)
    filename_noext, subdir, client_synthesis, client_species, client_mop  = uputil.start_upload(input_path)
    # Read the synthesis procedures from the input JSON file
    chemicals_json                              = utils.read_json_file(input_path)['synthesisProcedures']
    # Extract the DOI from the file path
    doi                                         = utils.doi_from_path(input_path)
    # Query the knowledge graph for an existing document entry using the DOI
    document_iri                                = kgq.doi_querying(client_synthesis, doi)
    # If no document entry exists, create a new one; otherwise, retrieve it from the KG
    if document_iri == []:
        document                                    = Document(doi=doi)
    else: 
        document                                    = Document.pull_from_kg(document_iri[0]["doc"], 
                                                                            sparql_client=client_synthesis, 
                                                                            recursive_depth=-1)
    # Iterate over synthesis procedures described in the JSON file 
    for synthesis in chemicals_json:
        print("synthesis: ", synthesis)
        # Extract input chemicals
        chemicals                               = synthesis['steps'][0]
        print("chemcials: ", chemicals)
        # Extract output chemical details (assumes only one output chemical is listed)
        output_chemical                         = chemicals['outputChemical'][0]
        print("output chemical: ", output_chemical)
        # Instantiate or retrieve the output chemical and its yield
        chemical_output, yield_instance                     = instantiate_output(
            output_chemical["CCDCNumber"], output_chemical["chemicalFormula"], output_chemical["names"], 
            output_chemical["yield"], client_mop, client_synthesis)
        # Process input chemicals
        chemical_list                                       = []
        for chemical in chemicals['inputChemicals']:
            if chemical["chemical"] != []:
                # Upload input chemical data and retrieve its corresponding instance
                chemical_input                              = upload_inputChem(chemical["chemical"], client_synthesis, client_species)
            else:
                # Use a predefined "unknown" chemical input instance if no chemical data is provided
                chemical_input                                              = ChemicalInput.pull_from_kg(
                    f"{preiri.ONTOSYN_BASE}ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", 
                    client_synthesis, 
                    recursive_depth=-1)[0]
            # If supplier information and purity are provided, create supplier and associate purity level
            if "supplierName" in chemical and "purity" in chemical:
                supplier                                                    = Supplier(rdfs_label=chemical["supplierName"])
                chemical_input.hasPurity.add(chemical["purity"])                                              
                chemical_input.isSuppliedBy.add(supplier)
            # Add processed chemical input to the list
            chemical_list.append(chemical_input)
        # Construct a list of names to query existing chemical transformations in the KG
        mop_names                                           = output_chemical["names"]
        mop_names.append(output_chemical["chemicalFormula"])
        mop_names.append(output_chemical["CCDCNumber"])
        # Query the knowledge graph for an existing transformation process using the output chemical names
        syn_prod                                            = kgq.transformation_querying(client_synthesis, mop_names) 
        print("transformation: ",yield_instance) 
        # Instantiate the chemical synthesis process, linking it to the document and input chemicals
        chemical_synthesis                                  = ChemicalSynthesis(
            retrievedFrom=document, 
            hasChemicalInput=chemical_list, 
            hasYield=yield_instance) 
        # If no transformation entry exists, create a new one 
        if syn_prod == []:
            chemical_transformation                         = ChemicalTransformation(
                hasChemicalOutput=chemical_output, 
                isDescribedBy=chemical_synthesis)
        # otherwise use existing one
        else: 
            # If no transformation entry exists, create a new one
            print("transformation iri: ", syn_prod[0]["chemicalTrans"])
            chemical_transformation                         = ChemicalTransformation.pull_from_kg(
                syn_prod[0]["chemicalTrans"], 
                sparql_client=client_synthesis, 
                recursive_depth=-1)[0]
            # connect to transformation
            chemical_transformation.isDescribedBy.add(chemical_synthesis)
        # Push the newly created or updated transformation and synthesis data to the KG
        components_output                                   = [chemical_transformation, chemical_synthesis]
        uputil.push_component_to_kg(components_output, client_synthesis)

def elemental_analysis_upload(elemental_analysis, syn_client, chemical_output, molecular_formula, device):
    """
    Uploads elemental analysis data to the knowledge graph (KG) by processing the provided elemental composition.

    This function processes elemental analysis data and classifies it as either an 
    experimental analysis (if a device is specified) or an analytical analysis 
    (if based on a molecular formula).

    Args:
        elemental_analysis (str): A string containing elemental composition percentages.
        syn_client (SPARQLClient): The client used to interact with the synthesis knowledge graph.
        chemical_output (ChemicalOutput): The chemical compound associated with this elemental analysis.
        molecular_formula (str): The molecular formula of the compound.
        device (str): The instrument used for elemental analysis. If empty, the analysis is considered analytical.

    Returns:
        None: The function uploads the data to the KG but does not return a value.

    Notes:
        - Calls `parse_element_string` to extract elemental weight percentages.
        - If a device is provided, the analysis is classified as `ExperimentalElementalAnalysis`.
        - If no device is provided, the analysis is classified as `AnalyticalElementalAnalysis` and linked to its molecular formula.
    """
    # Parse the elemental composition string to extract weight percentages for elements
    percentages                                            = parse_element_string(elemental_analysis, syn_client)
    # If a specific device was used, classify as ExperimentalElementalAnalysis
    if device != "":
        analysis_class                                      = ExperimentalElementalAnalysis(
            hasElementWeightPercentage=percentages, 
            hasElementalDevice=device)
    else:
        # If no device is specified, classify as AnalyticalElementalAnalysis based on molecular formula
        elem                                                = MolecularFormula(value=molecular_formula)
        # Upload the molecular formula to the knowledge graph
        elem.push_to_kg(syn_client, recursive_depth=-1)
        # Create an AnalyticalElementalAnalysis instance and link it to the molecular formula
        analysis_class                                      = AnalyticalElementalAnalysis(hasElementWeightPercentage=percentages, isBasedOnMolecularFormula=elem)
    return analysis_class, chemical_output

def characterisation_upload(input_path, output_path):
    """
    Processes and uploads characterization data from a JSON file to the knowledge graph (KG).

    This function extracts characterization data such as elemental analysis, 
    NMR spectra, and infrared spectroscopy from a given JSON file, links them 
    to the corresponding chemical transformation, and uploads the processed data 
    to the knowledge graph.

    Args:
        input_path (str): Path to the JSON file containing characterization data.
        output_path (str): Path where the processed output will be stored (currently unused).

    Returns:
        None: The function uploads data to the KG but does not return any values.

    Notes:
        - Extracts and processes device names used for characterization.
        - Queries the KG to find the corresponding chemical transformation.
        - Associates extracted data (e.g., elemental analysis, IR, and NMR spectra) with the chemical output.
        - Uploads the processed characterization data to the KG.
    """
    # Initialize SPARQL clients for different knowledge graphs
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    # Read the characterization JSON file
    characterisation_json                                       = utils.read_json_file(input_path)["Devices"][0]
    # Extract device names used for elemental analysis, HNMR, and infrared spectroscopy
    elemental_device_name                                       = characterisation_json["ElementalAnalysisDevice"]["deviceName"]
    # general information for all procedures of the paper
    elemental_device                                            = InstrumentType(rdfs_label=elemental_device_name)
    hnmr_device_name                                            = characterisation_json["HNMRDevice"]["deviceName"]
    nmr_device                                                  = InstrumentType(rdfs_label=hnmr_device_name)
    hnmr_frequency                                              = characterisation_json["HNMRDevice"]["frequency"]
    ir_device_name                                              = characterisation_json["InfraredSpectroscopyDevice"]["deviceName"]
    ir_device                                                   = InstrumentType(rdfs_label=ir_device_name)
    # Iterate over the list of characterization data entries
    for entry in characterisation_json["Characterisation"]:
        print("entry: ", entry)
        # Extract product names and CCDC number for linking
        mop_name                                                = entry["productNames"]
        mop_ccdc                                                = entry["productCCDCNumber"]
        print("mop_name1: ", mop_name)
        mop_name.append(mop_ccdc)                           # Append CCDC number to names for better linkage
        print("mop names: ", mop_name, "ccdc:", mop_ccdc, "mop_name: ", mop_name)
        # Query the knowledge graph for the corresponding chemical transformation
        transformation_iri                                      = kgq.transformation_querying(syn_client, mop_name=mop_name)
        if transformation_iri == []:
            # Skip this entry if no linkage to a chemical transformation is found
            continue
        try:
            # Retrieve the existing chemical transformation from the KG
            chemical_transformation                                 = ChemicalTransformation.pull_from_kg(
                transformation_iri[0]["chemicalTrans"], 
                sparql_client=syn_client, 
                recursive_depth=1) 
        except:
            print("failed to pull IRI for: ", mop_name)
            continue
        if type(chemical_transformation) == str:
            print("pulled iri only, continue")
            continue
        # Extract chemical output from the transformation
        chemical_output                                         = chemical_transformation[0].hasChemicalOutput
        # --- Processing NMR Data ---
        nmr                                                     = entry['HNMR']
        solvent                                                 = Solvent(rdfs_label=nmr["solvent"])
        # Extract NMR chemical shifts and assign to peaks
        nmr_shifts                                              = uputil.extract_numbers_and_brackets(nmr["shifts"])
        nmr_peaks                                               = []
        for shift in nmr_shifts:
            # Use predefined "N/A" characteristic peak from KG
            if type(shift) != tuple:
                # Use predefined "N/A" characteristic peak from KG
                nmr_peak                                        = CharacteristicPeak(hasX1=shift, rdfs_comment="")
            elif shift == "N/A" or shift == " N/A":
                # Create a peak with shift and associated comment
                nmr_peak                                        = CharacteristicPeak.pull_from_kg(preiri.NMR_NA, syn_client, -1)
            else:
                # Create a peak with only shift value
                nmr_peak                                        = CharacteristicPeak(hasX1=shift[0], rdfs_comment=shift[1])
            nmr_peaks.append(nmr_peak)
        # Create an NMR spectra graph with peaks
        nmr_spectra_graph                                       = SpectraGraph(hasX1Axis="ppm", hasPeak=nmr_peaks)
        hnmr                                                    = HNMRSpectra(hasSolvent=solvent, hasInstrumentType=nmr_device, hasSpectraGraph=nmr_spectra_graph)
        # --- Processing Elemental Analysis ---
        elemental_analysis                                      = entry["ElementalAnalysis"]
        # Process calculated elemental analysis (without device)
        calc_analysis_class, chemical_output                    = elemental_analysis_upload(
            elemental_analysis["weightPercentageCalculated"], 
            syn_client, chemical_output, 
            elemental_analysis["chemicalFormula"], 
            "")
        # Process experimental elemental analysis (with device)
        exp_analysis_class, chemical_output                     = elemental_analysis_upload(
            elemental_analysis["weightPercentageExperimental"], 
            syn_client, 
            chemical_output, 
            elemental_analysis["chemicalFormula"], 
            elemental_device)
        # --- Processing IR Spectroscopy ---
        ir                                                      = entry['InfraredSpectroscopy']
        # Extract IR band peaks
        bands                                                   = uputil.extract_numbers_and_brackets(ir["bands"])
        peaks                                                   = []
        for band in bands:
            if type(band) != tuple:
                # Create a peak with only wavenumber
                peak                                            = CharacteristicPeak(hasX1=band, rdfs_comment="")
                peak.push_to_kg(syn_client, recursive_depth=-1)
            elif band == "N/A":
                # Use predefined "N/A" IR characteristic peak
                peak                                            = CharacteristicPeak.pull_from_kg(preiri.IR_NA, syn_client, -1)
            else:
                # Create a peak with wavenumber and comment
                peak                                            = CharacteristicPeak(hasX1=band[0], rdfs_comment=band[1])
                peak.push_to_kg(syn_client, recursive_depth=-1)
            peaks.append(peak)
        # Create an IR spectra graph and Fourier Transform spectrum
        spectra_graph                                           = SpectraGraph(hasX1Axis="cm-1", hasPeak=peaks)
        ft_spectra                                              = FourierTransformSpectrum(
            hasSpectraGraph=spectra_graph, 
            hasInstrumentType=ir_device)
        # --- Associating Characterization Data with Chemical Output ---s
        for chem_out in chemical_output:
            chem_out.hasFourierTransformSpectrum.add(ft_spectra)
            chem_out.hasElementalAnalysis.add(calc_analysis_class)
            chem_out.hasElementalAnalysis.add(exp_analysis_class)
            chem_out.has1H1HNMR.add(hnmr)
        # Upload updated chemical output data
        uputil.push_component_to_kg(chemical_output, syn_client)
        # Upload all components (devices, analyses, spectra)
        components                                              = [elemental_device, ir_device, calc_analysis_class, exp_analysis_class, spectra_graph, ft_spectra, hnmr, nmr_spectra_graph, solvent, chemical_transformation, chem_out]
        uputil.push_component_to_kg(components, syn_client)
    return

def instantiate_cbu(cbu_formula, syn_client, mop_client, species):
    """
    Instantiates or retrieves a Chemical Building Unit (CBU) from the knowledge graph (KG).

    This function queries the synthesis and MOP (Metal-Organic Polyhedra) knowledge graphs
    for an existing Chemical Building Unit (CBU) with the given formula. If no existing 
    CBU is found, a new instance is created and linked to the given species.

    Args:
        cbu_formula (str): The chemical formula of the CBU.
        syn_client (Client): The SPARQL client for the synthesis knowledge graph.
        mop_client (Client): The SPARQL client for the MOP knowledge graph.
        species (Species): The species that is used as the chemical component of the CBU.

    Returns:
        ChemicalBuildingUnit: An instance of the Chemical Building Unit, either retrieved 
        from the KG or newly created.

    Notes:
        - First, it searches for an existing CBU in the synthesis KG.
        - If not found, it searches the MOP KG.
        - If the CBU is still not found, a new instance is created and linked to the species.
        - If an existing CBU is found, it is pulled from the KG.
    """
    # Query the synthesis knowledge graph for an existing CBU with the given formula
    cbu_iri                                 = kgq.CBU_querying(syn_client, cbu_formula)
    if cbu_iri==[]:                                 # If no CBU found in the synthesis KG, search in the MOP KG
        cbu_iri                             = kgq.CBU_querying(mop_client, cbu_formula)
        print("cbu iri: ", cbu_iri[0]["CBUIRI"])
        if cbu_iri==[]:                             # If no CBU found in either KG, create a new instance
            cbu                             = ChemicalBuildingUnit(hasCBUFormula=cbu_formula, isUsedAsChemical=species)
        else:
            try:
                # If CBU found in the MOP KG, instantiate it with the retrieved IRI
                cbu                         = ChemicalBuildingUnit(
                    instance_iri=cbu_iri[0]["CBUIRI"],
                    hasCBUFormula=cbu_formula, 
                    isUsedAsChemical=species)
            except:
                # If there's an issue with direct instantiation, pull the existing CBU from the KG
                cbu                         = ChemicalBuildingUnit.pull_from_kg(
                    cbu_iri[0]["CBUIRI"], 
                    syn_client, 
                    recursive_depth=-1)[0]
    else:
        # If CBU found in the synthesis KG, retrieve it from the KG
        cbu                                 = ChemicalBuildingUnit.pull_from_kg(
            cbu_iri[0]["CBUIRI"], 
            syn_client, 
            recursive_depth=-1)[0]
    return cbu

def link_cbu(input_path, output_path):    
    """
    Links Chemical Building Units (CBUs) to their corresponding Metal-Organic Polyhedron (MOP) 
    in the knowledge graph (KG) based on synthesis procedure data.

    This function reads a JSON file containing synthesis procedures, queries for species in the KG, 
    and associates CBUs with their corresponding MOP instances.

    Args:
        input_path (str): Path to the input JSON file containing synthesis data.
        output_path (str): (Unused in the function) Path for output results.

    Returns:
        None: The function updates the KG by linking CBUs to MOPs.

    Notes:
        - If any species is marked as "N/A" or not found in the KG, the function skips that entry.
        - Uses `instantiate_cbu` to create or retrieve CBUs from the KG.
        - Adds the retrieved or instantiated CBUs to their corresponding MOP entity in the KG.
    """
    # Initialize the upload process and retrieve required SPARQL clients
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    # Read synthesis procedures from the input JSON file
    cbu_json                                                        = utils.read_json_file(input_path)["synthesisProcedures"]
    print("json: ", cbu_json)
    # Iterate over each product in the synthesis data
    for product in cbu_json:
        # Extract the CCDC number for identifying the MOP
        CCDC_num                                                    = product["mopCCDCNumber"]
        # Query the KG for species IRIs based on provided species names
        species_iri_1                                               = kgq.species_querying(syn_client, product["cbuSpeciesNames1"])
        species_iri_2                                               = kgq.species_querying(syn_client, product["cbuSpeciesNames2"])
        # Check if species information is missing, marked as "N/A", or not found in the KG
        if product["cbuSpeciesNames1"] == "N/A" or product["cbuSpeciesNames2"] == "N/A" or product["cbuSpeciesNames1"] == ["N/A"] or product["cbuSpeciesNames2"] == ["N/A"] or species_iri_1 == [] or species_iri_2 == [] :
            print("species not found, abort mission")
            continue
        print("species iri: ", species_iri_1[0]["Species"])
        # Retrieve species instances from the KG
        species1                                                    = Species.pull_from_kg(species_iri_1[0]["Species"] ,syn_client,1)
        species2                                                    = Species.pull_from_kg(species_iri_2[0]["Species"] ,syn_client,1)
        # Instantiate or retrieve CBUs for the corresponding species
        cbu1                                                        = instantiate_cbu(product["cbuFormula1"], syn_client, sparql_client_mop, species1)
        cbu2                                                        = instantiate_cbu(product["cbuFormula2"], syn_client, sparql_client_mop, species2)
        # Query the KG for an existing MOP instance using the CCDC number
        mop_iri                                                     = kgq.mop_querying(syn_client, CCDC_num, "", "")
        # Retrieve the corresponding MOP instance from the KG
        mop_instance                                                = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"] ,syn_client,1)[0]
        print("mop instance:", mop_instance)
        # Link the retrieved CBUs to the MOP instance
        mop_instance.hasChemicalBuildingUnit.add(cbu1)
        mop_instance.hasChemicalBuildingUnit.add(cbu2)
        print("mop instance", mop_instance)
        # Prepare components to push to the KG
        components                                                  = [cbu1, cbu2, mop_instance]
        uputil.push_component_to_kg(components, syn_client)
    return

def parse_element_string(element_string, syn_client):
    """
    Parses a string containing element weight percentages and converts them into structured 
    data objects for storage in a knowledge graph.

    Args:
        element_string (str): A string containing element names and their corresponding percentages, 
                              typically formatted as "Element1 X%, Element2 Y%".
        syn_client (SPARQLClient): The client used to interact with the synthesis knowledge graph.

    Returns:
        list: A list of `ElementWeightPercentage` objects representing the parsed element weight percentages.

    Notes:
        - If the input string contains semicolons (";"), they are replaced with commas for consistency.
        - The function handles cases where values and element names might be swapped.
        - Extracted data is structured into `Measure`, `MassFraction`, and `ElementWeightPercentage` objects.
        - Each structured component is pushed to the knowledge graph.
    """
    # Standardize delimiter: Replace semicolons with commas and remove other unnecessary characters
    if ";" in element_string:
        element_string                                          = element_string.replace(",", "")
        element_string                                          = element_string.replace(";", ",")
    # Remove percentage signs and split the string by commas into individual element-value pairs
    element_list                                                = element_string.replace('%', '').split(', ')
    # Initialize an empty list to store parsed element weight percentages
    element_percentages                                         = []
    # Iterate through each element-value pair in the list
    for element_pair in element_list:
        try:
            # Split the element name and its corresponding numerical value
            element, value                                      = element_pair.split()
        except:
            # If splitting fails, assign a default value (indicating missing data)
            element                                             = "N/A"
            value                                               = 0
        # Print extracted values for debugging purposes
        print("element: ", element, "value: ", value)
        try:
            # Convert extracted value to a floating-point number
            value                                                   = float(value)
        except:
            # If conversion fails, attempt to swap the positions of element and value
            ele                                                 = value
            value                                               = element
            element                                             = ele
        # Generate a unique identifier for the instance
        id_hash_value                                           = str(uuid.uuid4())
        # Retrieve the unit of measurement (percentage) from the knowledge graph
        unit                                                    = UnitOfMeasure.pull_from_kg(preiri.PERCENTAGE, syn_client, recursive_depth=-1)[0]
        # Create a `Measure` instance to store the numerical value and its unit
        measure                                                 = Measure(
            instance_iri=f"{preiri.ONTOSYN_BASE}MassFractionValue_{id_hash_value}", 
            hasNumericalValue=value, 
            hasUnit=unit)
        # Create a `MassFraction` instance that references the `Measure`
        mass_fraction                                           = MassFraction(hasValue=measure)
        # Match the element name to an existing entity in the knowledge graph
        element_inst                                            = uputil.match_element(element, client=syn_client)
        # Create an `ElementWeightPercentage` instance linking the mass fraction to the element
        element_weight_precentage                               = ElementWeightPercentage(
            hasMassFraction=mass_fraction, 
            isReferingToElement=element_inst)
        # Append the structured data to the list
        element_percentages.append(element_weight_precentage)
        # Push all related components to the knowledge graph
        components                                              = [measure, mass_fraction, element_weight_precentage]
        uputil.push_component_to_kg(components, syn_client)
    # Return the list of parsed `ElementWeightPercentage` objects
    return element_percentages


def upload_steps(input_path, output_path):
    """
    Uploads synthesis steps from a JSON file to a knowledge graph.

    This function processes synthesis data from a given JSON input file, linking it to 
    corresponding chemical transformations, synthesis steps, vessels, and chemical inputs.

    Args:
        input_path (str): Path to the input JSON file containing synthesis steps.
        output_path (str): Path where output files will be stored (currently unused).

    Returns:
        None: The function processes and uploads data but does not return a value.

    Notes:
        - The function extracts synthesis steps from the JSON file.
        - It links the synthesis to a DOI document and identifies or creates a chemical transformation.
        - It iterates through each synthesis step, creating instances for steps, vessels, and chemicals.
        - The processed data is uploaded to the knowledge graph.
    """
    # Initialize clients for synthesis, species, and MOP (Metal-Organic Polyhedra) knowledge graphs
    filename_noext, subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    # Read synthesis data from the JSON file
    print("input path: ", input_path)
    synthesis_json                                              = utils.read_json_file(input_path)["Synthesis"]
    print("actual full data: ", synthesis_json)
    # Extract DOI from the input file path
    doi                                                         = utils.doi_from_path(input_path)
    # Query the knowledge graph to check if the document already exists
    document_iri                                                = kgq.doi_querying(sparql_client_synthesis, doi)
    if document_iri == []:
        # Create a new document entry if it does not exist
        document                                                = Document(doi=doi)
    else: 
        # Retrieve the existing document from the knowledge graph
        document                                                = Document.pull_from_kg(document_iri[0]["doc"], sparql_client=sparql_client_synthesis, recursive_depth=-1)
    # Iterate through each synthesis procedure in the JSON file
    for entry in synthesis_json:
        # Extract product names and CCDC numbers
        mop_name                                                = entry["productNames"]
        mop_name.append(entry["productCCDCNumber"])
        print("mop_name: ", mop_name)
        # Query the knowledge graph for an existing chemical transformation linked to the product
        transformation_iri                                      = kgq.transformation_querying(
            sparql_client_synthesis, 
            mop_name=mop_name)
        # Default to unknown yield
        yield_instance                                          = AmountOfSubstanceFraction.pull_from_kg(
            f"{preiri.ONTOSYN_BASE}Yield_3ed5e18b-5206-405d-ada0-382071f73f74", 
            sparql_client_synthesis, 
            recursive_depth=-1)[0]
        print("transformation iri: ", transformation_iri)
        if transformation_iri == []:
            # If no transformation is found, create a new one
            print(f"generating new Transformation! MOP values: CCDC={entry["productCCDCNumber"]} and productName= {entry["productNames"]}. ")
            # Instantiate a new chemical output with an unknown yield
            chemical_output, yield_instance                     = instantiate_output(entry["productCCDCNumber"], "N/A", entry["productNames"], "-1", sparql_client_mop, sparql_client_synthesis)
            # Create a new chemical transformation instance
            chemical_transformation                             = ChemicalTransformation(hasChemicalOutput=chemical_output)
            # Store components to be pushed to the knowledge graph
            components                                          = [chemical_output, chemical_transformation]
            # Save transformation IRI for later reference
            transformation_iri                                  = [{'chemicalTrans': chemical_transformation.instance_iri}]
            print(chemical_output)
            # Upload new transformation and output to the knowledge graph
            uputil.push_component_to_kg(components, sparql_client_synthesis)
        # List to store synthesis steps and chemical inputs  
        step_list                                               = []
        chemicals_list                                          = []
        # Initialize an empty vessel for the first iteration
        vessel                                                  = Vessel()
        vessel_list                                             = []
        vessel_list.append(vessel)
        # Iterate through each synthesis step
        for step_dat in entry["steps"]:
            print("step data: ", step_dat)
            # Process and instantiate the step, updating vessels and chemical inputs
            step_class, vessel_list, chemicals_list             = standard_step_upload(step_dat, vessel_list, chemicals_list, sparql_client_synthesis, sparql_client_species)
            # Append step to the list
            step_list.append(step_class)
            
        print("finished steps!")
        # Attempt to create a ChemicalSynthesis instance linking steps and inputs
        try:
            chemical_synthesis                                  = ChemicalSynthesis(
                hasSynthesisStep=step_list, 
                retrievedFrom=document, 
                hasChemicalInput=chemicals_list, 
                hasYield=yield_instance) 
        except: 
            # If the chemical list is too long, exclude it and process separately
            chemical_synthesis                                  = ChemicalSynthesis(
                hasSynthesisStep=step_list, 
                retrievedFrom=document, 
                hasYield=yield_instance) 
        # Retrieve the chemical transformation from the knowledge graph
        chemical_transformation                                 = ChemicalTransformation.pull_from_kg(
            transformation_iri[0]["chemicalTrans"], 
            sparql_client_synthesis, 
            recursive_depth=1)   
        print("pulled transformation IRI: ", chemical_transformation)
        # Link the synthesis process to the transformation
        chemical_transformation[0].isDescribedBy.add(chemical_synthesis)
        # Store components for upload
        components                                              = [chemical_synthesis, chemical_transformation]
        print("Started pushing synthesis and transformation")
        # Upload the synthesis steps and transformation to the knowledge graph
        uputil.push_component_to_kg(components, sparql_client_synthesis, 2)
        print("Ended pushing synthesis and transformation")

def main():
    upload_predefined()
  


if __name__ == "__main__":
    main()