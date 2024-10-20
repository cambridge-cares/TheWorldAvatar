import re
import os
from OntoSyn_ontology import *
import utils

import uuid

def match_element(element_name, client):
    match element_name:
        case 'C':
            element_iri                          = "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_f1a5025b-7e20-4a6a-821e-a7b6c0473b8c"
        case 'O':  
            element_iri                          = "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_6a6be1ce-2021-4634-aed4-6a77488765df"
        case 'H': 
            element_iri                          = "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_43cfac3b-81db-4338-bfb1-b0b3386f7473"
        case 'N':  
            element_iri                          = "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_a9a7806c-f077-4eb2-b5b0-099d51033b7b"
        case _:     
            element_iri                          = "http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_e253b6ca-c169-4e60-b6be-46b95e045a85"
    element                                      = Element.pull_from_kg(element_iri, client,recursive_depth=-1)[0]
    return element

def match_separation(separation_name, client):
    match separation_name:
        case 'centrifuge':
            separation_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/SeparationType_5aa57330-613e-437d-a22b-dc10833a50b8"
        case 'column':  
            separation_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/SeparationType_aea2a49f-067f-4818-8abc-544dd8696ba8"
        case 'washing': 
            separation_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/SeparationType_61233c76-a0e5-4cb0-8c5c-ab8347955ea6"
        case 'extraction':  
            separation_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/SeparationType_c6d7ff74-4bdb-47b8-bcd8-fc40d9fbfb87"
        case _:     
            separation_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/SeparationType_9ff2a8f7-3c9c-4419-9f3a-76b34d8629c0"
    separation                                      = SeparationType.pull_from_kg(separation_iri, client,recursive_depth=-1)[0]
    return separation

def match_atmosphere(atmosphere, client):
    match atmosphere:
        case 'N2':
            atmosphere_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselEnvironment_434aa6e1-3ac6-4a08-a208-fbc23e78a758"
        case 'Ar':  
            atmosphere_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselEnvironment_65b5af5d-349d-467c-bd14-b239d4e94376"
        case 'Air': 
            atmosphere_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselEnvironment_bd2ef29a-1c5c-40eb-a9b2-84f1a3fda734"
        case _: 
            atmosphere_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselEnvironment_1f70dc2c-5a37-491a-89ec-0897f9dcb7b8"
    atmosphere                                      = VesselEnvironment.pull_from_kg(atmosphere_iri, client, recursive_depth=-1)[0]
    return atmosphere

def match_vessel(vessel_name, client): 
    match vessel_name:
        case 'Teflon-lined stainless-steel vessel':
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_eb0f5942-d36b-47b1-86f0-725c1549fa2e"
        case 'glass vial':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_90589d23-44e8-4698-acdf-bee3e44df96f"
        case 'quartz tube': 
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_06304c23-7926-45d2-841d-690b5de16ed0"
        case 'round bottom flask':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_5a7d7ec9-44d5-4280-8467-f9f624374a9d"
        case 'glass scintillation vial':    
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_b67ea47b-7849-4aac-b0fd-e2715a4ac034"
        case 'pyrex tube':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_080ad74b-950d-4651-a87c-5aa96d5ffb52"
        case 'schlenk flask':  
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_080zd54b-230c-4341-e87g-5ta46d2fgh91"
        case _:     
            vessel_iri                          = "https://www.theworldavatar.com/kg/OntoSyn/VesselType_183ad74b-950d-4631-a47c-5aa91d5ffb12"
    vessel                                      = VesselType.pull_from_kg(vessel_iri, client,recursive_depth=-1)[0]
    return vessel

def get_unit(unit_name, client):
    print("unit_name: ", unit_name)
    unit_name                   = unit_name.replace(" ", "")
    match unit_name:
        case "°C" | "C" | "degC" | "ºC" :
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
        case "week" | "weeks":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/week", client, recursive_depth=-1)[0]    
        case "seconds" | "second" | "s" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/second-Time", client, recursive_depth=-1)[0]                      
        case "min" | "minute" | "minutes" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/minute-Time", client, recursive_depth=-1)[0]                      
        case "g" | "gram" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/gram", client, recursive_depth=-1)[0]                      
        case "mg" | "miligram" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/milligram", client, recursive_depth=-1)[0]                      
        case "mol" | "mole" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/mole", client, recursive_depth=-1)[0]                                  
        case "mmol" | "milimole" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/millimole", client, recursive_depth=-1)[0]  
        case "mL" | "mililitre" | "mL" | "ml"  :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/millilitre", client, recursive_depth=-1)[0]                                  
        case "drop" | "drops" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/drop", client, recursive_depth=-1)[0]                                  
        case "M" | "mol/L" | "mol/l":
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre",  client, recursive_depth=-1)[0]  
        case _: 
            print(f"Unit was not recognized. Check the following unit: {unit_name} \n")            
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/unknown", client, recursive_depth=-1)[0]                                  
       

    """
    mole_per_litre                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre", rdfs_label="mole per litre")
    revolutions_per_minute                      = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/revolutionPerMinute-Time", rdfs_label="revolutions per minute")
    """
    return unit
def extract_numbers_and_units(text, pattern_type, multiplier_flag=2):
    """
    Extracts numbers and units based on pattern type ("add" or "temp").
    If "add" is matched, it also processes the range pattern (number1:number2).
    If "temp" is matched, range processing is skipped.

    Parameters:
        - text: The input text containing numbers and units.
        - pattern_type: "add" for regular units, "temp" for temperature or rates.
        - multiplier_flag: 0 to multiply by number1 from range, 1 to multiply by number2.
    """
    
    # Define patterns
    patterns = {
        'add': r'(\d*\.?\d+)\s*([a-zA-Z]+)',   # Pattern for numbers with units
        'temp': r'(\d*\.?\d+)\s*([^\d\s]+)',   # Pattern for temperature or rates
        'range': r'\((\d+):(\d+)\)'            # Pattern for (number1:number2)
    }

    # Determine the pattern based on pattern_type
    if pattern_type == "add":
        pattern = patterns['add']
    elif pattern_type == "temp":
        pattern = patterns['temp']
    else:
        return [0], ["N/A"], [1]  # Return default values if no valid pattern_type

    # Find all matches based on the selected pattern
    matches = re.findall(pattern, text)
    
    # Separate numbers and units
    numbers = [float(match[0]) for match in matches]
    units = [match[1] for match in matches]
    
    if not numbers:
        numbers = [0] 
    if not units:
        units = ["N/A"] 
    
    # If the pattern is "add", we process the range pattern
    if pattern_type == "add" and matches:
        # Find 'range' pattern matches (number1:number2)
        range_matches = re.findall(patterns['range'], text)
        
        if range_matches:
            print("range matched: ", range_matches)
            # If 'range' pattern is found, extract number1 and number2
            number1_list = [int(match[0]) for match in range_matches]
            number2_list = [int(match[1]) for match in range_matches]
            
            # Calculate the sum of number1 and number2 for each match
            divisors = [n1 + n2 for n1, n2 in zip(number1_list, number2_list)]
        else:
            # Default divisor if no 'range' is found but 'add' is present
            divisors = [1] * len(numbers)
            number1_list, number2_list = [1] * len(numbers), [1] * len(numbers)
    
        # Now divide each number by the corresponding sum of range values
        divisor = divisors[0]  # Using the first divisor for all numbers for simplicity
        numbers_divided = [num / divisor for num in numbers]
    
        # Multiply the result of division by number1 or number2 depending on multiplier_flag
        if multiplier_flag == 0:
            # Multiply by number1
            numbers_final = [num_div * number1_list[0] for num_div in numbers_divided]
        else:
            # Multiply by number2
            numbers_final = [num_div * number2_list[0] for num_div in numbers_divided]
    
    # If the pattern is "temp", skip range processing
    elif pattern_type == "temp":
        # Return numbers directly without dividing or multiplying by range values
        numbers_final = numbers
        divisors = [1] * len(numbers)
    else: 
        numbers_final = numbers
    
    return numbers_final, units

def extract_numbers_and_brackets(input_string):
    # Regular expression to match numbers and content in brackets
    pattern = r'(\d+\.?\d*)\s*\(([^)]+)\)'
    
    # Find all matches in the input string
    matches = re.findall(pattern, input_string)
    
    if matches:
        # Convert the results into a list of tuples
        extracted_data = [(num, details) for num, details in matches]
        return extracted_data
    else:
        separated_strings = [s.strip() for s in input_string.split(",")]
        return separated_strings
        
def remove_na(input_candidate):
    if input_candidate == "N/A":
        return ""
    return input_candidate

def update_alt_label(species, species_name):
    print("speecies name and type: ", type(species_name), species_name)
    for name in species_name:
        if name not in species.altLabel:
            species.altLabel.add(name)
    return species

def replace_character(species_names):
    names                                                   = []
    for name in species_names:
        name                = name.replace("·", "x")
        name                = name.replace("’", "'")
        name                = name.replace("”", '"')
        name                = name.replace("–", '-')
        name                = name.replace("·", 'x')
        name                = name.replace("•", 'x')
        names.append(name)
    return names

def push_component_to_kg(instances:list, client, recursive_depth=-1):
    
    for instance in instances:
        try:
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)
        except:
            instance                                                = instance[0]
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)


def start_upload(input_path):
    sparql_client_synthesis                                 = utils.get_client("OntoSynthesisConnection")
    sparql_client_species                                   = utils.get_client("OntoSpeciesConnection") 
    sparql_client_mop                                       = utils.get_client("OntoMOPConnection") 
    filename                                                = os.path.basename(input_path)
    filename_noext                                          = os.path.splitext(filename)[0]
    secondlast_subdir,last_subdir                           = os.path.split(input_path)
    return filename_noext, secondlast_subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  
def steps_preupload(standard_step, synthesis_client, vessel_list):
    """Each step has a duration, atmosphere, and vessel except for filter that has no duration. -> The funciton computes the common instances."""
    print("standard step: ", standard_step)
    if "duration" in standard_step:
        step_time, time_unit                    = extract_numbers_and_units(standard_step["duration"], "add")
    else:
        step_time                               = [-1]
        time_unit                               = "N/A"
    id_hash_value                               = str(uuid.uuid4())
    # duration 
    duration_unit                               = get_unit(time_unit[0], synthesis_client) 
    duration_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/StepDuration_{id_hash_value}", hasNumericalValue=step_time[0], hasUnit=duration_unit)
    duration                                    = Duration(hasValue=duration_value)
    # atmosphere    
    atmosphere                                  = match_atmosphere(standard_step["atmosphere"], synthesis_client)
    # Vessel:
    for ves in vessel_list:
        if ves.rdfs_label == standard_step["usedVesselName"]:
                vessel                                      = ves
                return vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value
    vessel_type                                 = match_vessel(standard_step['usedVesselType'], synthesis_client)
    vessel                                      = Vessel(rdfs_label=standard_step["usedVesselName"], hasVesselType=vessel_type)
    vessel_list.append(vessel)
    return vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value 