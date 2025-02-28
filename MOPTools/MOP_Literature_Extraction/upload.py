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

import json
import csv
from io import StringIO
import upload_utils as uputil
import utils
import kg_queries as kgq
import predefine_iris as preiri

def upload_predefined():
    # put this in different file
    filename_noext, subdir, client, client_species, client_mop  = uputil.start_upload("")

    Ir_NA                               = CharacteristicPeak(instance_iri=preiri.IR_NA, rdfs_label="N/A", rdfs_comment="")
    Nmr_NA                              = CharacteristicPeak(instance_iri=preiri.NMR_NA,rdfs_label="N/A", rdfs_comment="")
    KBr                                 = Species(instance_iri=preiri.KBR, rdfs_label="KBr")

    hydrogen                            = Element(instance_iri=preiri.HYDROGEN, rdfs_label="Hydrogen")
    carbon                              = Element(instance_iri=preiri.CARBON, rdfs_label="Carbon")
    oxygen                              = Element(instance_iri=preiri.OXYGEN, rdfs_label="Oxygen")
    nitrogen                            = Element(instance_iri=preiri.NITROGEN, rdfs_label="Nitrogen")
    unknown_element                     = Element(instance_iri=preiri.UNKNOWN_ELEMENT, rdfs_label="N/A")

    vessel_ss_teflon                    = VesselType(instance_iri=preiri.VESSEL_SS_TEFLON, rdfs_label="Teflon-lined stainless-steel vessel")
    glass_vial                          = VesselType(instance_iri=preiri.GLASS_VIAL, rdfs_label="glass vial")
    quartz_tube                         = VesselType(instance_iri=preiri.QUARTZ_TUBE, rdfs_label="quartz tube")
    round_bottom_flask                  = VesselType(instance_iri=preiri.ROUND_BOTTOM_FLASK, rdfs_label="round bottom flask")
    glass_scintilation_vial             = VesselType(instance_iri=preiri.GLASS_SCINTILATION_VIAL, rdfs_label="glass scintillation vial")
    pyrex_tube                          = VesselType(instance_iri=preiri.PYREX_TUBE, rdfs_label="pyrex tube")
    schlenk                             = VesselType(instance_iri=preiri.SCHLENK, rdfs_label="schlenk flask")
    undefined_vessel                    = VesselType(instance_iri=preiri.UNDEFINED_VESSEL, rdfs_label="N/A")
      
    degree_celsius                      = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS, rdfs_label="degree Celsius")
    kelvin                              = UnitOfMeasure(instance_iri=preiri.KELVIN, rdfs_label="kelvin")
    degree_celsius_hour                 = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS_HOUR, rdfs_label="degree Celsius per hour")
    degree_celsius_min                  = UnitOfMeasure(instance_iri=preiri.DEGREE_CELSIUS_MIN, rdfs_label="degree Celsius per minute")
    duration_h                          = UnitOfMeasure(instance_iri=preiri.DURATION_H, rdfs_label="hour")
    duration_day                        = UnitOfMeasure(instance_iri=preiri.DURATION_DAY, rdfs_label="day")
    duration_s                          = UnitOfMeasure(instance_iri=preiri.DURATION_S, rdfs_label="second")
    duration_week                       = UnitOfMeasure(instance_iri=preiri.DURATION_WEEK, rdfs_label="week")
    duration_min                        = UnitOfMeasure(instance_iri=preiri.DURATION_MIN, rdfs_label="minute")
    
    temperature_rate_degs               = UnitOfMeasure(instance_iri=preiri.TEMPERATURE_RATE_DEGS, rdfs_label="degree Celsius per second")
    mole_per_litre                      = UnitOfMeasure(instance_iri=preiri.MOLE_PER_LITRE, rdfs_label="mole per litre")
    grams                               = UnitOfMeasure(instance_iri=preiri.GRAMS, rdfs_label="gram")
    miligrams                           = UnitOfMeasure(instance_iri=preiri.MILI_GRAMS, rdfs_label="miligram")
    mole                                = UnitOfMeasure(instance_iri=preiri.MOLE, rdfs_label="mole")
    mmole                               = UnitOfMeasure(instance_iri=preiri.MMOLE, rdfs_label="mmole") 
    mlitre                              = UnitOfMeasure(instance_iri=preiri.MLITRE, rdfs_label="mlitre")
    # from TWA branch of OM:
    revolutions_per_minute              = UnitOfMeasure(instance_iri=preiri.REVOLUTIONS_PER_MINUTE, rdfs_label="revolution per minute", rdfs_comment="Revolution per minute is a unit of rotational speed (or rotational frequency) for rotating machines.")
    drop                                = UnitOfMeasure(instance_iri=preiri.DROP, rdfs_label="drop")
    unknown_unit                        = UnitOfMeasure(instance_iri=preiri.UNKNOWN_UNIT, rdfs_label="N/A")

    N2Atmo                              = VesselEnvironment(instance_iri=preiri.N2_ATMO, rdfs_label="N2 atmosphere")
    ArAtmo                              = VesselEnvironment(instance_iri=preiri.AR_ATMO, rdfs_label="Ar atmosphere")
    AirAtmo                             = VesselEnvironment(instance_iri=preiri.AIR_ATMO, rdfs_label="Air atmosphere")
    unknown_Atmo                        = VesselEnvironment(instance_iri=preiri.UNKNOWN_ATMO, rdfs_label="N/A")
    # unknowns
    yield_value                         = Measure(instance_iri=preiri.YIELD_VALUE, hasNumericalValue=-1, hasUnit=unknown_unit)
    yield_instance                      = AmountOfSubstanceFraction(instance_iri=preiri.YIELD_INSTANCE, hasValue=yield_value)
    chemical_input                      = ChemicalInput(instance_iri=preiri.CHEMICAL_INPUT, rdfs_label="N/A")
    unknown_mop                         = MetalOrganicPolyhedron(rdfs_label="N/A", instance_iri=preiri.UNKNWON_MOP)
    # separation types
    centrifuge                          = SeparationType(instance_iri=preiri.CENTRIFUGE, rdfs_label="centrifuge")
    column                              = SeparationType(instance_iri=preiri.COLUMN, rdfs_label="column")
    wash                                = SeparationType(instance_iri=preiri.WASH, rdfs_label="washing")
    extraction                          = SeparationType(instance_iri=preiri.EXTRACTION, rdfs_label="extraction")
    unknown_sep                         = SeparationType(instance_iri=preiri.UNKNOWN_SEP, rdfs_label="N/A")
    unknown_step                        = SynthesisStep(instance_iri=preiri.UNKNOWN_STEP, rdfs_comment="N/A")
    percentage                          = UnitOfMeasure(instance_iri=preiri.PERCENTAGE, rdfs_label="percent")
    instances                           = [yield_value, chemical_input, yield_value, unknown_step, yield_instance, unknown_mop, N2Atmo, column, extraction, unknown_sep, wash, centrifuge, ArAtmo, AirAtmo , unknown_Atmo, vessel_ss_teflon, glass_vial, quartz_tube, round_bottom_flask, glass_scintilation_vial, pyrex_tube, degree_celsius_hour, kelvin, degree_celsius, degree_celsius_min, duration_min, duration_day, duration_h, duration_s, duration_week, temperature_rate_degs, mole_per_litre, revolutions_per_minute, grams, miligrams, mole, mmole, mlitre, undefined_vessel, unknown_unit, drop, nitrogen, hydrogen, carbon, oxygen, unknown_element, percentage, KBr, Ir_NA, Nmr_NA, schlenk]
    uputil.push_component_to_kg(instances, client)

# alternative approach to unit upload -> additional query but does not require to upload them again.
def upload_inputChem(chemicals, synthesis_client, species_client):
    """
    Uploads chemical input data to a knowledge graph (KG) by processing chemical components and their concentrations.

    This function processes a list of chemicals, where each chemical contains a name and an optional amount. 
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
    phase_components                                        = []
    phase_component_concentrations                          = [] 
    for chemical in chemicals:
        species_name                                            = chemical["chemicalName"]
        # check if key is present
        if "chemicalAmount" in chemical:
            add_value, add_unit                                 = uputil.extract_numbers_and_units(chemical["chemicalAmount"], "add")
        else: 
            add_value                                           = [-1]
            add_unit                                            = ["N/A"]
        if species_name == []:
            species_name                                        = ["N/A"]
        species                                                 = instantiate_input(species_name[0], species_name, client_species=species_client, client_synthesis=synthesis_client) 
        print("species name: ", species_name)
        # Initialize an empty list to hold `ScalarValue` instances
        scalar_values                                           = []
        # Iterate over each pair of unit and value
        for unit, value in zip(add_unit, add_value):
            # Get the unit using the `get_unit` function
            print("unit and value: ", unit, value)
            unit_instance                                       = uputil.get_unit(unit, synthesis_client)
            # Create a `ScalarValue` instance and add it to the list
            scalar_value_instance                               = ScalarValue(hasNumericalValue=value, hasUnitOfMeasure=unit_instance)
            # make sure to instantiate new phase component for each species in the mixture
            scalar_values.append(scalar_value_instance)
            scalar_value_instance.push_to_kg(synthesis_client, -1)
        phase_component_concentration                           = PhaseComponentConcentration(hasValue=set(scalar_values))
        phase_component_concentrations.append(phase_component_concentration)
        phase_component                                         = PhaseComponent(representsOccurenceOf=species, hasProperty=phase_component_concentration)
        phase_components.append(phase_component)
    composition                                                 = Composition(comprisesDirectly=phase_component_concentrations)
    single_phase                                                = SinglePhase(isComposedOfSubsystem=phase_components, hasComposition=composition)     
    material                                                    = Material(thermodynamicBehaviour=single_phase)
    chemical_input                                              = ChemicalInput(referencesMaterial=material)
    print("species names: ", species.rdfs_label) 
    components = [phase_component_concentration, phase_component, composition, single_phase, material, chemical_input]
    uputil.push_component_to_kg(components, synthesis_client)
    return chemical_input


def standard_step_upload(standard_input, vessel_list, chemicals_list, synthesis_client, species_client):

    if "Sonicate" in standard_input:
        standard_step                                       = standard_input["Sonicate"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        sonication                                          = Sonicate(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasVesselEnvironment=atmosphere)
        components                                          = [duration_value, duration, vessel, sonication]
        uputil.push_component_to_kg(components, synthesis_client)
        return sonication, vessel_list, chemicals_list
    
    elif "Add" in standard_input:
        standard_step                                           = standard_input["Add"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["addedChemical"] != []:
            chemical_input                                      = upload_inputChem(standard_step["addedChemical"], synthesis_client, species_client)
        else:
            chemical_input                                      = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)
        add_class                                               = Add(hasOrder=standard_step['stepNumber'], hasStepDuration=duration,hasVessel=vessel, hasAddedChemicalInput=chemical_input, isStirred=standard_step['stir'], hasTargetPh=float(standard_step['targetPH']), isLayered=standard_step["isLayered"], hasVesselEnvironment=atmosphere,rdfs_comment=standard_step['comment'])  
        components = [add_class, vessel, duration, duration_value]
        uputil.push_component_to_kg(components, synthesis_client)
        chemicals_list.append(chemical_input)
        return add_class, vessel_list, chemicals_list
    
    elif "HeatChill" in standard_input:
        standard_step                                           = standard_input["HeatChill"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value         = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["targetTemperature"] == "room temperature":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["targetTemperature"],"temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        heat_time, time_unit                        = uputil.extract_numbers_and_units(standard_step["duration"], "add")
        heat_rate, rate_unit                        = uputil.extract_numbers_and_units(standard_step["heatingCoolingRate"], "temp")
        device                                      = HeatChillDevice(rdfs_label=standard_step["usedDevice"])
        print("heatingrate: ", heat_rate, rate_unit)
        print("duration: ", heat_time, time_unit)
        # temperature 
        id_hash_value                               = str(uuid.uuid4())
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetTemperature_{id_hash_value}", hasValue=temperature_value)

        # heat rate
        rate_unit                                   = uputil.get_unit(rate_unit[0], synthesis_client) 
        rate_value                                  = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureRate_{id_hash_value}", hasNumericalValue=heat_rate[0], hasUnit=rate_unit)
        temperature_rate                            = TemperatureRate(hasValue=rate_value) 
        # put everything together
        # stirring is missing!
        heat_chill                                  = HeatChill(hasVessel=vessel, hasHeatChillDevice=device,hasVesselEnvironment=atmosphere, hasStepDuration=duration, hasTargetTemperature=target_temperature, hasTemperatureRate=temperature_rate, hasVacuum=standard_step["underVacuum"], isSealed=standard_step["sealedVessel"], IsStirred=standard_step['stir'], hasOrder=standard_step['stepNumber'], rdfs_comment=standard_step['comment'])
        components = [heat_chill]
        uputil.push_component_to_kg(components, synthesis_client) 
        return heat_chill, vessel_list, chemicals_list
    
    elif "Filter" in standard_input:
        standard_step                                       = standard_input["Filter"] 
        # vessel
        # Vessel:
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
        # atmosphere    
        atmosphere                                          = uputil.match_atmosphere(standard_step["atmosphere"], synthesis_client)
        if standard_step["washingSolvent"] != []:
            chemical_input                                  = upload_inputChem(standard_step["washingSolvent"], synthesis_client, species_client)
        else:
            chemical_input                                  = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        filter_class                                        = Filter(hasOrder=standard_step["stepNumber"], hasVesselEnvironment=atmosphere, isRepeated=standard_step["numberOfFiltrations"], isVacuumFiltration=standard_step["vacuumFiltration"],hasWashingSolvent=chemical_input, rdfs_comment=standard_step["comment"], hasVessel=vessel)  
        components = [filter_class]
        uputil.push_component_to_kg(components, synthesis_client)
        chemicals_list.append(chemical_input)
        return filter_class, vessel_list, chemicals_list
    
    elif "Stir" in standard_input:
        standard_step                                       = standard_input["Stir"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/StirTemperature_{id_hash_value}", hasValue=temperature_value)
        
        stir                                        = Stir(hasStepDuration=duration, isWait=standard_step["wait"], hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasStirringTemperature=target_temperature)
        components                                  = [stir]
        uputil.push_component_to_kg(components, synthesis_client)
        return stir, vessel_list, chemicals_list
    
    elif "Crystallization" in standard_input:
        standard_step                                       = standard_input["Crystallization"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["targetTemperature"] == "room temperature" or standard_step["targetTemperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["targetTemperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)

        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetTemperature_{id_hash_value}", hasValue=temperature_value)
        crystallization                             = Crystallize(hasOrder=standard_step["stepNumber"], hasVesselEnvironment=atmosphere, hasVessel=vessel, hasStepDuration=duration, rdfs_comment=standard_step["comment"], hasCrystallizationTargetTemperature=target_temperature)
        components                                  = [crystallization]
        uputil.push_component_to_kg(components, synthesis_client)
        return crystallization, vessel_list, chemicals_list
    
    elif "Dry" in standard_input:
        standard_step                                       = standard_input["Dry"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
        if standard_step["dryingAgent"] != []:
            chemical_input                          = upload_inputChem(standard_step["dryingAgent"], synthesis_client, species_client)
        else:
            chemical_input                          = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        
        chemicals_list.append(chemical_input)
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit, rdfs_comment=standard_step["temperature"])
        #pressure_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/PressureValue_{id_hash_value}",hasNumericalValue=pres[0], hasUnit=pressure_unit, rdfs_comment=standard_step["pressure"])
        drying_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/DryingTemperature_{id_hash_value}", hasValue=temperature_value)
        drying_pressure                             = Pressure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/DryingPressure_{id_hash_value}", rdfs_label=standard_step["pressure"])
        dry                                         = Dry(hasStepDuration=duration, hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasDryingPressure=drying_pressure,hasDryingTemperature=drying_temperature, rdfs_comment=standard_step["comment"])
        components                                  = [dry]
        uputil.push_component_to_kg(components, synthesis_client)
        return dry, vessel_list, chemicals_list
    
    elif "Evaporate" in standard_input:
        standard_step                                       = standard_input["Evaporate"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = uputil.get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = uputil.extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = uputil.get_unit(temp_unit[0], synthesis_client) 
        vol, vol_unit                               = uputil.extract_numbers_and_units(standard_step["targetVolume"], "temp")
        volume_unit                                 = uputil.get_unit(vol_unit[0], synthesis_client) 
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit, rdfs_comment=standard_step["temperature"])
        #pressure_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/PressureValue_{id_hash_value}",hasNumericalValue=pres[0], hasUnit=pressure_unit, rdfs_comment=standard_step["pressure"])
        evap_temperature                            = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/EvaporationTemperature_{id_hash_value}", hasValue=temperature_value)
        evap_pressure                               = Pressure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/EvaporationPressure_{id_hash_value}", rdfs_label=standard_step["pressure"])
        if standard_step["removedSpecies"] != []:
            chemical_input                          = upload_inputChem(standard_step["removedSpecies"], synthesis_client, species_client)
        else:
            chemical_input                          = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        chemicals_list.append(chemical_input)
        volume_value                                = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/VolumeValue_{id_hash_value}",hasNumericalValue=vol[0], hasUnit=volume_unit, rdfs_comment=standard_step["targetVolume"])
        target_volume                               = Volume(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetVolume_{id_hash_value}", hasValue=volume_value)
        evaporate                                   = Evaporate(hasStepDuration=duration, isEvaporatedToVolume=target_volume, removesSpecies=chemical_input, hasRotaryEvaporator=standard_step["rotaryEvaporator"],hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasEvaporationTemperature=evap_temperature, hasEvaporationPressure=evap_pressure, rdfs_comment=standard_step["comment"])  
        components                                  = [evaporate]
        uputil.push_component_to_kg(components, synthesis_client)
        return evaporate, vessel_list, chemicals_list

    elif "Transfer" in standard_input:
        standard_step                               = standard_input["Transfer"]
        # target vessel
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if vessel.rdfs_label != standard_step["targetVesselName"]:
            targetvessel_type                       = uputil.match_vessel(standard_step["targetVesselType"], synthesis_client)
            targetvessel                            = Vessel(rdfs_label=standard_step["targetVesselName"], hasVesselType=targetvessel_type)
        else: 
            targetvessel                            = vessel
        # volume
        vol, vol_unit                               = uputil.extract_numbers_and_units(standard_step["transferedAmount"], "temp")
        volume_unit                                 = uputil.get_unit(vol_unit[0], synthesis_client) 
        volume_value                                = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/VolumeValue_{id_hash_value}",hasNumericalValue=vol[0], hasUnit=volume_unit)
        target_volume                               = Volume(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetVolume_{id_hash_value}", hasValue=volume_value)
        transfer                                    = Transfer(hasStepDuration=duration, hasTransferedAmount=target_volume, isLayeredTransfer=standard_step["isLayered"], hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, isTransferedTo=targetvessel, rdfs_comment=standard_step["comment"])
        components                                  = [transfer]
        uputil.push_component_to_kg(components, synthesis_client)
        return transfer, vessel_list, chemicals_list
    
    elif "Dissolve" in standard_input:
        standard_step                                                   = standard_input["Dissolve"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["solvent"] != []:
            chemical_input                                              = upload_inputChem(standard_step["solvent"], synthesis_client, species_client)
        else:
            chemical_input                                              = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        dissolve                                                        = Dissolve(hasStepDuration=duration, hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasSolventDissolve=chemical_input, rdfs_comment=standard_step["comment"])
        components                                                      = [dissolve]
        chemicals_list.append(chemical_input)
        uputil.push_component_to_kg(components, synthesis_client)
        return dissolve, vessel_list, chemicals_list
    
    elif "Separate" in standard_input:
        standard_step                           = standard_input["Separate"]
        vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value     = uputil.steps_preupload(standard_step, synthesis_client, vessel_list)
        if standard_step["solvent"] != []:
            chemical_input                                              = upload_inputChem(standard_step["solvent"], synthesis_client, species_client)
        else:
            chemical_input                                              = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", synthesis_client, recursive_depth=-1)[0]
        separation_type                                                 = uputil.match_separation(standard_step["separationType"], synthesis_client)
        separate                                                        = Separate(hasStepDuration=duration, isSeparationType=separation_type, hasVesselEnvironment=atmosphere, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasSeparationSolvent=chemical_input , rdfs_comment=standard_step["comment"])
        components                                                      = [separate]
        chemicals_list.append(chemical_input)
        uputil.push_component_to_kg(components, synthesis_client)
        return separate, vessel_list, chemicals_list
    else:
        standard_class                                                  = SynthesisStep.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/SynthesisStep_ddb7ceda-13d2-461a-a63e-9e7df3116882", synthesis_client, recursive_depth=-1)[0]
        return standard_class, vessel_list, chemicals_list

def instantiate_input(chemical_formula, species_name, client_species, client_synthesis):
    # search the ontospecies and ontosynthesis blazegraphs for existing instances
    species_iri                                             = str(uuid.uuid4())
    species_name                                            = uputil.replace_character(species_names=species_name)
    chemical_formula                                        = uputil.replace_character(species_names=[chemical_formula])[0]
    triples                                                 = kgq.species_querying_ontosyn(client_synthesis, species_name)

    print("OntoSpecies results: ", triples)
    if triples == None or triples == []:
        triples                                             = kgq.species_querying(client_species, species_name)
        if triples == None or triples == []:
            species                                         = Species(label=chemical_formula, altLabel=species_name)
            # Ontospecies uses different base IRIs for rdf type and the actual instance IRI.
            species.instance_iri                            = f"http://www.theworldavatar.com/kb/ontospecies/Species_{species_iri}"
        else:
            try:
                species                                     = Species(instance_iri=triples[0]["Species"] ,label=chemical_formula, altLabel=species_name)
            except:
                # there already exists a species with the IRI but with different labels than before -> query syn kg and add label
                species                                     = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
                # update if not already saved (avoids 1000s of duplicates)
                species                                     = uputil.update_alt_label(species, species_name=species_name)
            #species                                         = update_alt_label(species, species_name=species_name)
            # upload here as well
            

    else:
        print("Success: ", triples[0]["Species"])
        # species                                             = Species(instance_iri=triples[0]["Species"] ,label=chemical_formula, altLabel=species_name)
        species                                             = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
        # update if not already saved (avoids 1000s of duplicates)
        species                                             = uputil.update_alt_label(species, species_name=species_name)
        
    return species

def instantiate_output(ccdc_number, chemical_formula, mop_names, yield_str, client_mop, client_synthesis):
    mop_names                               = uputil.replace_character(mop_names)
    chemical_formula                        = uputil.replace_character([chemical_formula])[0]
    # query for existing mops either in the OntoMOPs ontology 
    mop_iri                                 = kgq.mop_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
    # if no mop fits instantiate otherwise empty entry
    if mop_iri == []:
        mop_iri                             = kgq.mop_querying(client_mop, ccdc_number, chemical_formula, mop_names)
        if mop_iri == []:
            # couldn't link with a mop, instantiate unknown mop to indicate failed linkage:
            mop                             = MetalOrganicPolyhedron.pull_from_kg("https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedra_1d981ba2-4072-47ef-9ecd-b9f5cc06a50a", client_synthesis, recursive_depth=-1)[0]
        else:
            # for unknown reason sometimes it fails to pull and returns iri, check if not list otherwise query again!
            try:
                mop                             = MetalOrganicPolyhedron(instance_iri=mop_iri[0]["MOPIRI"], hasCCDCNumber=ccdc_number, hasMOPFormula=chemical_formula, altLabel=mop_names)
            except:
                mop                             = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"], client_synthesis,recursive_depth=-1)[0]
                mop                             = uputil.update_alt_label(mop, mop_names)
    else:
        mop                                     = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"], client_synthesis,recursive_depth=-1)[0]
        mop                                     = uputil.update_alt_label(mop, mop_names)
    # Yield
    uuid_id                                     = str(uuid.uuid4())
    unit                                        = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/percent", client_synthesis, recursive_depth=-1)[0]
    print("yield number", yield_str)
    yield_str                                   = yield_str.replace('%', '')
    if yield_str == "-1" or yield_str == "N/A":
        yield_instance                          = AmountOfSubstanceFraction.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/Yield_3ed5e18b-5206-405d-ada0-382071f73f74", client_synthesis, recursive_depth=-1)[0]
        print("unknown yield: ", yield_instance)
    else:
        try:
            yield_val                               = float(yield_str)
            yield_value                             = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/YieldValue_{uuid_id}", hasNumericalValue=yield_val, hasUnit=unit)
            yield_instance                          = AmountOfSubstanceFraction(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/Yield_{uuid_id}", hasValue=yield_value)
        except:
            yield_instance                          = AmountOfSubstanceFraction.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/Yield_3ed5e18b-5206-405d-ada0-382071f73f74", client_synthesis, recursive_depth=-1)[0]
        
 # output chemical
    output_names                                = []
    print("yield: ", yield_instance)

    output_names.append(chemical_formula)
    output_names.append(ccdc_number)
    for mop_name in mop_names:
        output_names.append(mop_name)
    output_iri                                  = kgq.chemicalOutput_querying(client_synthesis, ccdc_number, chemical_formula, mop_names)
    if output_iri == []:
        print("output names3: ", output_names)
        chemical_output                         = ChemicalOutput(isRepresentedBy=mop, altLabel=output_names)
    else: 
        chemical_output                         = ChemicalOutput.pull_from_kg(output_iri[0]["chemicalOutput"], client_synthesis, recursive_depth=-1)[0]
        uputil.update_alt_label(chemical_output, output_names)
        chemical_output.isRepresentedBy.add(mop)
    return chemical_output, yield_instance 

def chemicals_upload(input_path, output_path):
    print("input path: ", input_path)
    filename_noext, subdir, client_synthesis, client_species, client_mop  = uputil.start_upload(input_path)
    # go through json file:
    chemicals_json                              = utils.read_json_file(input_path)['synthesisProcedures']
    # general information
    doi                                         = utils.doi_from_path(input_path)
    document_iri                                = kgq.doi_querying(client_synthesis, doi)
    if document_iri == []:
        document                                    = Document(doi=doi)
    else: 
        document                                    = Document.pull_from_kg(document_iri[0]["doc"], sparql_client=client_synthesis, recursive_depth=-1)
    # go trough the different procedures described in the paper: 
    for synthesis in chemicals_json:
        print("synthesis: ", synthesis)
        chemicals                               = synthesis['steps'][0]
        print("chemcials: ", chemicals)
        # output chemical(currently only one otherwise one would need the same for loop as input):
        output_chemical                         = chemicals['outputChemical'][0]
        print("output chemical: ", output_chemical)
        # add yield to chemical output / make subclass of product!
        chemical_output, yield_instance                     = instantiate_output(output_chemical["CCDCNumber"], output_chemical["chemicalFormula"], output_chemical["names"], output_chemical["yield"], client_mop, client_synthesis)
        # input chemicals:
        chemical_list                                       = []
        for chemical in chemicals['inputChemicals']:
            if chemical["chemical"] != []:
                chemical_input                              = upload_inputChem(chemical["chemical"], client_synthesis, client_species)
            else:
                chemical_input                                              = ChemicalInput.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/ChemicalInput_3ed5e18b-5206-405d-ada0-382071f73f74", client_synthesis, recursive_depth=-1)[0]
            if "supplierName" in chemical and "purity" in chemical:
                supplier                                                    = Supplier(rdfs_label=chemical["supplierName"])
                chemical_input.hasPurity.add(chemical["purity"])                                              
                chemical_input.isSuppliedBy.add(supplier)
            chemical_list.append(chemical_input)
        
        mop_names                                           = output_chemical["names"]
        mop_names.append(output_chemical["chemicalFormula"])
        mop_names.append(output_chemical["CCDCNumber"])
        syn_prod                                            = kgq.transformation_querying(client_synthesis, mop_names) 
        print("transformation: ",yield_instance) 
        chemical_synthesis                                  = ChemicalSynthesis(retrievedFrom=document, hasChemicalInput=chemical_list, hasYield=yield_instance) 
        # no entry => make a new one
        if syn_prod == []:
            chemical_transformation                         = ChemicalTransformation(hasChemicalOutput=chemical_output, isDescribedBy=chemical_synthesis)
        # otherwise use existing one
        else: 
            print("transformation iri: ", syn_prod[0]["chemicalTrans"])
            chemical_transformation                         = ChemicalTransformation.pull_from_kg(syn_prod[0]["chemicalTrans"], sparql_client=client_synthesis, recursive_depth=-1)[0]
            chemical_transformation.isDescribedBy.add(chemical_synthesis)
        components_output                                   = [chemical_transformation, chemical_synthesis]
        uputil.push_component_to_kg(components_output, client_synthesis)

def elemental_analysis_upload(elemental_analysis, syn_client, chemical_output, molecular_formula, device):
    percentages                                            = parse_element_string(elemental_analysis, syn_client)
    if device != "":
        analysis_class                                      = ExperimentalElementalAnalysis(hasElementWeightPercentage=percentages, hasElementalDevice=device)
    else:
        elem                                                = MolecularFormula(value=molecular_formula)
        elem.push_to_kg(syn_client, recursive_depth=-1)
        analysis_class                                      = AnalyticalElementalAnalysis(hasElementWeightPercentage=percentages, isBasedOnMolecularFormula=elem)
    # analysis_class.push_to_kg(syn_client, recursive_depth=-1)
        
    return analysis_class, chemical_output

def characterisation_upload(input_path, output_path):
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    characterisation_json                                       = utils.read_json_file(input_path)["Devices"][0]
    elemental_device_name                                       = characterisation_json["ElementalAnalysisDevice"]["deviceName"]
    # general information for all procedures of the paper
    elemental_device                                            = InstrumentType(rdfs_label=elemental_device_name)
    hnmr_device_name                                            = characterisation_json["HNMRDevice"]["deviceName"]
    nmr_device                                                  = InstrumentType(rdfs_label=hnmr_device_name)
    hnmr_frequency                                              = characterisation_json["HNMRDevice"]["frequency"]
    ir_device_name                                              = characterisation_json["InfraredSpectroscopyDevice"]["deviceName"]
    ir_device                                                   = InstrumentType(rdfs_label=ir_device_name)
    # synthesis specific information
    # http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_f6cce625-9d69-4491-bd9d-b096114db7af
    for entry in characterisation_json["Characterisation"]:
        print("entry: ", entry)
        mop_name                                                = entry["productNames"]
        mop_ccdc                                                = entry["productCCDCNumber"]
        print("mop_name1: ", mop_name)
        mop_name.append(mop_ccdc)
        print("mop names: ", mop_name, "ccdc:", mop_ccdc, "mop_name: ", mop_name)
        transformation_iri                                      = kgq.transformation_querying(syn_client, mop_name=mop_name)
        if transformation_iri == []:
            # stop if no linkage with product is possible
            continue
        try:
            chemical_transformation                                 = ChemicalTransformation.pull_from_kg(transformation_iri[0]["chemicalTrans"], sparql_client=syn_client, recursive_depth=1) 
        except:
            print("failed to pull IRI for: ", mop_name)
            continue
        if type(chemical_transformation) == str:
            print("pulled iri only, continue")
            continue
        chemical_output                                         = chemical_transformation[0].hasChemicalOutput
        # NMR
        nmr                                                     = entry['HNMR']
        solvent                                                 = Solvent(rdfs_label=nmr["solvent"])
        nmr_shifts                                              = uputil.extract_numbers_and_brackets(nmr["shifts"])
        nmr_peaks                                               = []
        for shift in nmr_shifts:
            if type(shift) != tuple:
                nmr_peak                                        = CharacteristicPeak(hasX1=shift, rdfs_comment="")
            elif shift == "N/A" or shift == " N/A":
                nmr_peak                                        = CharacteristicPeak.pull_from_kg(preiri.NMR_NA, syn_client, -1)
            else:
                nmr_peak                                        = CharacteristicPeak(hasX1=shift[0], rdfs_comment=shift[1])
            nmr_peaks.append(nmr_peak)
            
        nmr_spectra_graph                                       = SpectraGraph(hasX1Axis="ppm", hasPeak=nmr_peaks)
        hnmr                                                    = HNMRSpectra(hasSolvent=solvent, hasInstrumentType=nmr_device, hasSpectraGraph=nmr_spectra_graph)
        # elemental analysis
        elemental_analysis                                      = entry["ElementalAnalysis"]
        calc_analysis_class, chemical_output                    = elemental_analysis_upload(elemental_analysis["weightPercentageCalculated"], syn_client, chemical_output, elemental_analysis["chemicalFormula"], "")
        exp_analysis_class, chemical_output                     = elemental_analysis_upload(elemental_analysis["weightPercentageExperimental"], syn_client, chemical_output, elemental_analysis["chemicalFormula"], elemental_device)
        # IR
        ir                                                      = entry['InfraredSpectroscopy']
        bands                                                   = uputil.extract_numbers_and_brackets(ir["bands"])
        peaks                                                   = []
        for band in bands:
            if type(band) != tuple:
                peak                                            = CharacteristicPeak(hasX1=band, rdfs_comment="")
                peak.push_to_kg(syn_client, recursive_depth=-1)
            elif band == "N/A":
                peak                                            = CharacteristicPeak.pull_from_kg(preiri.IR_NA, syn_client, -1)
            else:
                peak                                            = CharacteristicPeak(hasX1=band[0], rdfs_comment=band[1])
                peak.push_to_kg(syn_client, recursive_depth=-1)
            peaks.append(peak)
        spectra_graph                                           = SpectraGraph(hasX1Axis="cm-1", hasPeak=peaks)
        ft_spectra                                              = FourierTransformSpectrum(hasSpectraGraph=spectra_graph, hasInstrumentType=ir_device)

        for chem_out in chemical_output:
            chem_out.hasFourierTransformSpectrum.add(ft_spectra)
            chem_out.hasElementalAnalysis.add(calc_analysis_class)
            chem_out.hasElementalAnalysis.add(exp_analysis_class)
            chem_out.has1H1HNMR.add(hnmr)
        uputil.push_component_to_kg(chemical_output, syn_client)
        # upload all
        components                                              = [elemental_device, ir_device, calc_analysis_class, exp_analysis_class, spectra_graph, ft_spectra, hnmr, nmr_spectra_graph, solvent, chemical_transformation, chem_out]
        uputil.push_component_to_kg(components, syn_client)
    return

def instantiate_cbu(cbu_formula, syn_client, mop_client, species):
    cbu_iri                                 = kgq.CBU_querying(syn_client, cbu_formula)
    if cbu_iri==[]:
        cbu_iri                             = kgq.CBU_querying(mop_client, cbu_formula)
        print("cbu iri: ", cbu_iri[0]["CBUIRI"])
        if cbu_iri==[]:
            cbu                             = ChemicalBuildingUnit(hasCBUFormula=cbu_formula, isUsedAsChemical=species)
        else:
            try:
                cbu                         = ChemicalBuildingUnit(instance_iri=cbu_iri[0]["CBUIRI"],hasCBUFormula=cbu_formula, isUsedAsChemical=species)
            except:
                # there already exists a species with the IRI but with different labels than before -> query syn kg and add label
                cbu                         = ChemicalBuildingUnit.pull_from_kg(cbu_iri[0]["CBUIRI"], syn_client, recursive_depth=-1)[0]
    else:
        cbu                                 = ChemicalBuildingUnit.pull_from_kg(cbu_iri[0]["CBUIRI"], syn_client, recursive_depth=-1)[0]
    return cbu

def link_cbu(input_path, output_path):    
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    cbu_json                                                        = utils.read_json_file(input_path)["synthesisProcedures"]
    print("json: ", cbu_json)
    for product in cbu_json:
        CCDC_num                                                    = product["mopCCDCNumber"]
        species_iri_1                                               = kgq.species_querying(syn_client, product["cbuSpeciesNames1"])
        species_iri_2                                               = kgq.species_querying(syn_client, product["cbuSpeciesNames2"])
        if product["cbuSpeciesNames1"] == "N/A" or product["cbuSpeciesNames2"] == "N/A" or product["cbuSpeciesNames1"] == ["N/A"] or product["cbuSpeciesNames2"] == ["N/A"] or species_iri_1 == [] or species_iri_2 == [] :
            print("species not found, abort mission")
            continue
        print("species iri: ", species_iri_1[0]["Species"])
        species1                                                    = Species.pull_from_kg(species_iri_1[0]["Species"] ,syn_client,1)
        species2                                                    = Species.pull_from_kg(species_iri_2[0]["Species"] ,syn_client,1)
        cbu1                                                        = instantiate_cbu(product["cbuFormula1"], syn_client, sparql_client_mop, species1)
        cbu2                                                        = instantiate_cbu(product["cbuFormula2"], syn_client, sparql_client_mop, species2)
        mop_iri                                                     = kgq.mop_querying(syn_client, CCDC_num, "", "")

        mop_instance                                                = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"] ,syn_client,1)[0]
        print("mop instance:", mop_instance)
        mop_instance.hasChemicalBuildingUnit.add(cbu1)
        mop_instance.hasChemicalBuildingUnit.add(cbu2)
        print("mop instance", mop_instance)
        components                                                  = [cbu1, cbu2, mop_instance]
        uputil.push_component_to_kg(components, syn_client)

    return

def parse_element_string(element_string, syn_client):
    # Remove any percentage signs and commas and split the string by commas
    if ";" in element_string:
        element_string                                          = element_string.replace(",", "")
        element_string                                          = element_string.replace(";", ",")
    element_list                                                = element_string.replace('%', '').split(', ')
    # Initialize an empty dictionary
    element_percentages                                         = []
    # Iterate through each element-value pair in the list
    for element_pair in element_list:
        # Split the element and its corresponding value
        try:
            element, value                                      = element_pair.split()
        except:
            element                                             = "N/A"
            value                                               = 0
        # Add the element and its float value to the dictionary
        print("element: ", element, "value: ", value)
        try:
            value                                                   = float(value)
        except:
            # swap element and value to test if the other way around works
            ele                                                 = value
            value                                               = element
            element                                             = ele
        id_hash_value                                           = str(uuid.uuid4())
        unit                                                    = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/percent", syn_client, recursive_depth=-1)[0]
        measure                                                 = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/MassFractionValue_{id_hash_value}", hasNumericalValue=value, hasUnit=unit)
        mass_fraction                                           = MassFraction(hasValue=measure)
        element_inst                                            = uputil.match_element(element, client=syn_client)
        element_weight_precentage                               = ElementWeightPercentage(hasMassFraction=mass_fraction, isReferingToElement=element_inst)
        element_percentages.append(element_weight_precentage)
        components                                              = [measure, mass_fraction, element_weight_precentage]
        uputil.push_component_to_kg(components, syn_client)

    return element_percentages


def upload_steps(input_path, output_path):
    filename_noext, subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  = uputil.start_upload(input_path)
    # go through json file:
    print("input path: ", input_path)
    synthesis_json                                              = utils.read_json_file(input_path)["Synthesis"]
    print("actual full data: ", synthesis_json)
    # get doi
    doi                                                         = utils.doi_from_path(input_path)
    document_iri                                                = kgq.doi_querying(sparql_client_synthesis, doi)
    if document_iri == []:
        document                                                = Document(doi=doi)
    else: 
        document                                                = Document.pull_from_kg(document_iri[0]["doc"], sparql_client=sparql_client_synthesis, recursive_depth=-1)
            
    for entry in synthesis_json:
        mop_name                                                = entry["productNames"]
        mop_name.append(entry["productCCDCNumber"])
        print("mop_name: ", mop_name)
        transformation_iri                                      = kgq.transformation_querying(sparql_client_synthesis, mop_name=mop_name)
        # set yield to unknown yield as default
        yield_instance                                          = AmountOfSubstanceFraction.pull_from_kg("https://www.theworldavatar.com/kg/OntoSyn/Yield_3ed5e18b-5206-405d-ada0-382071f73f74", sparql_client_synthesis, recursive_depth=-1)[0]
        print("transformation iri: ", transformation_iri)
        if transformation_iri == []:
            print(f"generating new Transformation! MOP values: CCDC={entry["productCCDCNumber"]} and productName= {entry["productNames"]}. ")
            chemical_output, yield_instance                     = instantiate_output(entry["productCCDCNumber"], "N/A", entry["productNames"], "-1", sparql_client_mop, sparql_client_synthesis)
            chemical_transformation                             = ChemicalTransformation(hasChemicalOutput=chemical_output)
            components                                          = [chemical_output, chemical_transformation]
            transformation_iri                                  = [{'chemicalTrans': chemical_transformation.instance_iri}]
            print(chemical_output)
            uputil.push_component_to_kg(components, sparql_client_synthesis)
        # gather step and input_chemical class instances at every step and link all of them in the end    
        step_list                                               = []
        chemicals_list                                          = []
        # instantiate empty vessel for the first iteration
        vessel                                                  = Vessel()
        vessel_list                                             = []
        vessel_list.append(vessel)
        for step_dat in entry["steps"]:
            print("step data: ", step_dat)
            step_class, vessel_list, chemicals_list             = standard_step_upload(step_dat, vessel_list, chemicals_list, sparql_client_synthesis, sparql_client_species)
            step_list.append(step_class)
            
        print("finished steps!")
        # failed to upload for 30+ steps
        try:
            chemical_synthesis                                  = ChemicalSynthesis(hasSynthesisStep=step_list, retrievedFrom=document, hasChemicalInput=chemicals_list, hasYield=yield_instance) 
        except: 
            # chemical list too long? do it in postprocessing
            chemical_synthesis                                  = ChemicalSynthesis(hasSynthesisStep=step_list, retrievedFrom=document, hasYield=yield_instance) 
        chemical_transformation                                 = ChemicalTransformation.pull_from_kg(transformation_iri[0]["chemicalTrans"], sparql_client_synthesis, recursive_depth=1)   
        print("pulled transformation IRI: ", chemical_transformation)
        chemical_transformation[0].isDescribedBy.add(chemical_synthesis)
        components                                              = [chemical_synthesis, chemical_transformation]
        print("Started pushing synthesis and transformation")
        uputil.push_component_to_kg(components, sparql_client_synthesis, 2)
        print("Ended pushing synthesis and transformation")

def main():
    #input_path                                                  = "../Data/first10_prompt54/10.1002_anie.201900519.json"
    #upload(input_path, "")
    #OntoSyn         = "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl"
    upload_predefined()
  
    #chemicals_upload_json(input_path=input_path, output_path="")
    #upload(input_path, "")
    #upload_cbu(input_path)
    #upload_steps(input_path=input_path, output_path="")
    #characterisation_upload(input_path, "")


if __name__ == "__main__":
    main()