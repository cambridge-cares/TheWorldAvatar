import re
import os
from OntoSyn_ontology import *
import utils
import predefine_iris as piris

import uuid

def match_element(element_name, client):
    """
    Matches a given element name (symbol) to its corresponding IRI in the periodic table ontology 
    and retrieves its details from the knowledge graph.

    Parameters:
    element_name (str): The chemical element symbol (e.g., 'C' for Carbon, 'O' for Oxygen).
    client: A knowledge graph client instance used to fetch element details.

    Returns:
    Element: An instance of the Element class retrieved from the knowledge graph.
    """
    # Match the element symbol to its corresponding IRI from the ontology
    match element_name:
        case 'C':                       # Carbon
            element_iri                          = piris.CARBON
        case 'O':                       # Oxygen
            element_iri                          = piris.OXYGEN
        case 'H':                       # Hydrogen
            element_iri                          = piris.HYDROGEN
        case 'N':                       # Nitrogen
            element_iri                          = piris.NITROGEN
        case _:                         # Default case for unknown elements
            element_iri                          = piris.UNKNOWN_ELEMENT
    # Retrieve the element from the knowledge graph using the specified IRI
    element                                      = Element.pull_from_kg(element_iri, client,recursive_depth=-1)[0]
    return element

def match_separation(separation_name, client):
    """
    Matches a given separation technique name to its corresponding IRI in the ontology 
    and retrieves its details from the knowledge graph.

    Parameters:
    separation_name (str): The name of the separation technique (e.g., 'centrifuge', 'column').
    client: A knowledge graph client instance used to fetch separation details.

    Returns:
    SeparationType: An instance of the SeparationType class retrieved from the knowledge graph.
    """
    # Match the separation technique name to its corresponding IRI in the ontology
    match separation_name:
        case 'centrifuge':              # Centrifuge-based separation
            separation_iri                          = piris.CENTRIFUGE
        case 'column':                  # Column chromatography
            separation_iri                          = piris.COLUMN
        case 'washing':                 # Washing with solvent
            separation_iri                          = piris.WASH
        case 'extraction':              # Liquid-liquid or solid-liquid extraction
            separation_iri                          = piris.EXTRACTION
        case _:                         # Default case for unknown separation techniques
            separation_iri                          = piris.UNKNOWN_SEP
     # Retrieve the separation technique details from the knowledge graph using the specified IRI
    separation                                      = SeparationType.pull_from_kg(separation_iri, client,recursive_depth=-1)[0]
    return separation

def match_atmosphere(atmosphere, client):
    """
    Matches a given atmosphere type to its corresponding IRI in the ontology 
    and retrieves its details from the knowledge graph.

    Parameters:
    atmosphere (str): The type of atmospheric condition (e.g., 'N2' for nitrogen, 'Ar' for argon, 'Air' for ambient air).
    client: A knowledge graph client instance used to fetch atmospheric environment details.

    Returns:
    VesselEnvironment: An instance of the VesselEnvironment class retrieved from the knowledge graph.
    """
    # Match the atmosphere type to its corresponding IRI in the ontology
    match atmosphere:
        case 'N2':                      # Nitrogen atmosphere
            atmosphere_iri                          = piris.N2_ATMO
        case 'Ar':                      # Argon atmosphere
            atmosphere_iri                          = piris.AR_ATMO
        case 'Air':                     # Ambient air
            atmosphere_iri                          = piris.AIR_ATMO
        case _:                         # Default case for unknown or unspecified atmospheres
            atmosphere_iri                          = piris.UNKNOWN_ATMO
    # Retrieve the atmospheric environment details from the knowledge graph using the specified IRI
    atmosphere                                      = VesselEnvironment.pull_from_kg(atmosphere_iri, client, recursive_depth=-1)[0]
    return atmosphere

def match_vessel(vessel_name, client): 
    """
    Matches a given vessel type name to its corresponding IRI in the ontology 
    and retrieves its details from the knowledge graph.

    Parameters:
    vessel_name (str): The name of the laboratory vessel (e.g., 'Teflon-lined stainless-steel vessel', 'glass vial').
    client: A knowledge graph client instance used to fetch vessel details.

    Returns:
    VesselType: An instance of the VesselType class retrieved from the knowledge graph.
    """
    # Match the vessel type name to its corresponding IRI in the ontology
    match vessel_name:
        case 'Teflon-lined stainless-steel vessel':     # High-temperature, high-pressure reaction vessel
            vessel_iri                          = piris.VESSEL_SS_TEFLON
        case 'glass vial':                              # Small-scale reaction or storage container
            vessel_iri                          = piris.GLASS_VIAL
        case 'quartz tube':                             # High-temperature reaction vessel, often for thermal decomposition
            vessel_iri                          = piris.QUARTZ_TUBE
        case 'round bottom flask':                      # Common vessel for reflux and heating reactions
            vessel_iri                          = piris.ROUND_BOTTOM_FLASK
        case 'glass scintillation vial':                # Typically used for sample storage or small-scale reactions
            vessel_iri                          = piris.GLASS_SCINTILATION_VIAL
        case 'pyrex tube':                              # Resistant to thermal shock, used for heating or storing solutions
            vessel_iri                          = piris.PYREX_TUBE
        case 'schlenk flask':                           # Designed for air-sensitive reactions under inert atmosphere
            vessel_iri                          = piris.SCHLENK
        case _:                                         # Default case for unknown or unspecified vessels
            vessel_iri                          = piris.UNDEFINED_VESSEL
    # Retrieve the vessel details from the knowledge graph using the specified IRI
    vessel                                      = VesselType.pull_from_kg(vessel_iri, client,recursive_depth=-1)[0]
    return vessel

def get_unit(unit_name, client):
    """
    Matches a given unit name to its corresponding IRI in the ontology 
    and retrieves its details from the knowledge graph.

    Parameters:
    unit_name (str): The name of the unit (e.g., '°C', 'mol', 'g', 'hour').
    client: A knowledge graph client instance used to fetch unit details.

    Returns:
    UnitOfMeasure: An instance of the UnitOfMeasure class retrieved from the knowledge graph.
    """
    # Print the provided unit name for debugging purposes
    print("unit_name: ", unit_name)
    # Remove spaces from the unit name to standardize matching
    unit_name                   = unit_name.replace(" ", "")
    # Match the unit name to its corresponding IRI in the ontology
    match unit_name:
        # Temperature units
        case "°C" | "C" | "degC" | "ºC" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DEGREE_CELSIUS, client, recursive_depth=-1)[0]
        case "K" | "Kelvin":
            unit                = UnitOfMeasure.pull_from_kg(piris.KELVIN, client, recursive_depth=-1)[0]
        
        # Temperature change rate units
        case "°C/h" | "C/h" | "degC/h" | "°C/hour" | "C/hour" | "degC/hour":
            unit                = UnitOfMeasure.pull_from_kg(piris.DEGREE_CELSIUS_HOUR, client, recursive_depth=-1)[0]
        case "°C/min" | "C/min" | "degC/min" | "°C/minute" | "C/minute" | "degC/minute":
            unit                = UnitOfMeasure.pull_from_kg(piris.DEGREE_CELSIUS_MIN, client, recursive_depth=-1)[0]    
        
        # Time units
        case "hour" | "hours" | "h" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DURATION_H, client, recursive_depth=-1)[0]    
        case "day" | "days" | "d" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DURATION_DAY, client, recursive_depth=-1)[0]    
        case "week" | "weeks":
            unit                = UnitOfMeasure.pull_from_kg(piris.DURATION_WEEK, client, recursive_depth=-1)[0]    
        case "seconds" | "second" | "s" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DURATION_S, client, recursive_depth=-1)[0]                      
        case "min" | "minute" | "minutes" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DURATION_MIN, client, recursive_depth=-1)[0]                      
        
        # Mass units
        case "g" | "gram" :
            unit                = UnitOfMeasure.pull_from_kg(piris.GRAMS, client, recursive_depth=-1)[0]                      
        case "mg" | "miligram" :
            unit                = UnitOfMeasure.pull_from_kg(piris.MILI_GRAMS, client, recursive_depth=-1)[0]                      
        
        # Molar units
        case "mol" | "mole" :
            unit                = UnitOfMeasure.pull_from_kg(piris.MOLE, client, recursive_depth=-1)[0]                                  
        case "mmol" | "milimole" :
            unit                = UnitOfMeasure.pull_from_kg(piris.MMOLE, client, recursive_depth=-1)[0]  
        
        # Volume units
        case "mL" | "mililitre" | "mL" | "ml"  :
            unit                = UnitOfMeasure.pull_from_kg(piris.MLITRE, client, recursive_depth=-1)[0]                                  
        
        # Miscellaneous units
        case "drop" | "drops" :
            unit                = UnitOfMeasure.pull_from_kg(piris.DROP, client, recursive_depth=-1)[0]                                  
        case "M" | "mol/L" | "mol/l":
            unit                = UnitOfMeasure.pull_from_kg(piris.MOLE_PER_LITRE,  client, recursive_depth=-1)[0]  
        
        # Default case for unknown units
        case _: 
            print(f"Unit was not recognized. Check the following unit: {unit_name} \n")            
            unit                = UnitOfMeasure.pull_from_kg(piris.UNKNOWN_UNIT, client, recursive_depth=-1)[0]                                  
       
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
    """
    Extracts numerical values and their corresponding units from a given text string based on the specified pattern type.

    Parameters:
        text (str): The input text containing numbers and units.
        pattern_type (str): Determines the type of pattern to use for extraction.
            - "add": Extracts numbers with units and processes range patterns (number1:number2).
            - "temp": Extracts temperature or rate values, ignoring range patterns.
        multiplier_flag (int): Determines which value from a range (number1:number2) to multiply with.
            - 0: Multiply by number1.
            - 1: Multiply by number2.
            - Default is 2 (no multiplication applied).

    Returns:
        tuple: (numbers_final, units)
            - numbers_final (list): List of extracted or processed numerical values.
            - units (list): List of corresponding units.
    """
    # Define regex patterns for extracting different types of numerical data
    patterns = {
        'add': r'(\d*\.?\d+)\s*([a-zA-Z]+)',   # Extracts numbers followed by a unit
        'temp': r'(\d*\.?\d+)\s*([^\d\s]+)',   # Extracts numbers followed by any non-digit, non-space characters (for temperature/rates)
        'range': r'\((\d+):(\d+)\)'            # Extracts numerical ranges in format (number1:number2)
    }

    # Select the appropriate pattern based on pattern_type
    if pattern_type == "add":
        pattern = patterns['add']
    elif pattern_type == "temp":
        pattern = patterns['temp']
    else:
        # If the pattern_type is invalid, return default values
        return [0], ["N/A"], [1]  # Return default values if no valid pattern_type

    # Find all matches in the text using the selected pattern
    matches = re.findall(pattern, text)
    
    # Extract numerical values and their respective units from matches
    numbers = [float(match[0]) for match in matches]
    units = [match[1] for match in matches]
    
    if not numbers:
        numbers = [0] 
    if not units:
        units = ["N/A"] 
    
    # If the pattern is "add", check for range patterns (number1:number2)
    if pattern_type == "add" and matches:
        # Search for range pattern matches in the text
        range_matches = re.findall(patterns['range'], text)
        
        if range_matches:
            print("range matched: ", range_matches)
            # Extract number1 and number2 from the range pattern
            number1_list = [int(match[0]) for match in range_matches]
            number2_list = [int(match[1]) for match in range_matches]
            
            # Compute divisor as the sum of number1 and number2
            divisors = [n1 + n2 for n1, n2 in zip(number1_list, number2_list)]
        else:
            # Default divisor if no 'range' is found but 'add' is present
            divisors = [1] * len(numbers)
            number1_list, number2_list = [1] * len(numbers), [1] * len(numbers)
    
        # Normalize each number by dividing by the first divisor found
        divisor = divisors[0]  # Using the first divisor for all numbers for simplicity
        numbers_divided = [num / divisor for num in numbers]
    
        # Apply multiplication based on multiplier_flag
        if multiplier_flag == 0:
            # Multiply by number1
            numbers_final = [num_div * number1_list[0] for num_div in numbers_divided]
        else:
            # No multiplication
            numbers_final = [num_div * number2_list[0] for num_div in numbers_divided]
    
    # If the pattern is "temp", return numbers directly without any range processing
    elif pattern_type == "temp":
        # Return numbers directly without dividing or multiplying by range values
        numbers_final = numbers
        divisors = [1] * len(numbers)
    else: 
        numbers_final = numbers
    
    return numbers_final, units

def extract_numbers_and_brackets(input_string):
    """
    Extracts numerical values along with the content inside parentheses from a given input string.
    If no matches are found, the function returns a list of comma-separated values from the string.

    Parameters:
        input_string (str): The input string containing numbers and/or parenthetical content.

    Returns:
        list: 
            - If matches are found, returns a list of tuples where each tuple contains:
                (number as a string, content inside parentheses).
            - If no matches are found, returns a list of strings split by commas.
    """
    # Regular expression pattern to match:
    # - A number (integer or decimal) followed by optional whitespace
    # - An opening parenthesis '('
    # - Any content inside the parentheses until a closing parenthesis ')'
    pattern = r'(\d+\.?\d*)\s*\(([^)]+)\)'
    
    # Find all matches in the input string
    matches = re.findall(pattern, input_string)
    
    if matches:
        # Convert matches into a list of tuples (number, content inside parentheses)
        extracted_data = [(num, details) for num, details in matches]
        return extracted_data
    else:
        # If no matches are found, split the string by commas and remove leading/trailing spaces
        separated_strings = [s.strip() for s in input_string.split(",")]
        return separated_strings
        
def remove_na(input_candidate):
    """
    Replaces "N/A" values with an empty string.

    This function checks if the given input is "N/A" and replaces it with an empty string.
    Otherwise, it returns the original input.

    Parameters:
        input_candidate (str): The input string to check.

    Returns:
        str: An empty string if the input is "N/A", otherwise the original input.
    """
    if input_candidate == "N/A":
        return ""           # Return an empty string if the input is "N/A"
    return input_candidate  # Return the original input if it's not "N/A"

def update_alt_label(species, species_name):
    """
    Updates the alternative labels (altLabel) for a given species.

    This function iterates through a list of species names and adds each name 
    to the species' altLabel attribute if it is not already present.

    Parameters:
        species (Species): The species object whose alternative labels need to be updated.
        species_name (list): A list of alternative names for the species.

    Returns:
        Species: The updated species object with the new altLabels added.
    """
    # Debugging: Print the type and content of species_name
    print("speecies name and type: ", type(species_name), species_name)
    # Iterate over the list of species names
    for name in species_name:
        # Add the name to altLabel if it is not already present
        if name not in species.altLabel:
            species.altLabel.add(name)
    # Return the updated species object
    return species

def replace_character(species_names):
    """
    Replaces specific special characters in species names with standard equivalents.

    This function iterates through a list of species names and replaces non-standard characters
    with their appropriate ASCII representations for consistency.

    Parameters:
        species_names (list): A list of species names (strings) that may contain special characters.

    Returns:
        list: A new list of species names with replaced characters.
    """
    # Initialize an empty list to store the cleaned names
    names                                                   = []
    # Iterate through each species name in the input list
    for name in species_names:
        # Replace special characters with standardized equivalents
        name                = name.replace("·", "x")            # Replaces middle dot with 'x'
        name                = name.replace("’", "'")            # Replaces curly apostrophe with standard apostrophe
        name                = name.replace("”", '"')            # Replaces curly double quotes with standard double quotes
        name                = name.replace("–", '-')            # Replaces en dash with standard hyphen
        name                = name.replace("·", 'x')            # Replaces bullet point with 'x'
        name                = name.replace("•", 'x')            # Replaces bullet point with 'x'
        # Append the cleaned name to the list
        names.append(name)
    # Return the list of cleaned names
    return names

def push_component_to_kg(instances:list, client, recursive_depth=-1):
    """
    Pushes a list of component instances to a knowledge graph (KG).

    This function iterates through a list of instances and attempts to push each to the KG. 
    If an error occurs due to an instance being wrapped in a list, it extracts the first 
    element and retries the operation.

    Parameters:
        instances (list): A list of component instances to be pushed to the KG.
        client: The KG client responsible for handling the push operation.
        recursive_depth (int, optional): Specifies the recursive depth for pushing nested instances 
                                         (default is -1, meaning unlimited depth).

    Returns:
        None: The function updates the KG but does not return any values.
    """
    # Iterate over each instance in the provided list
    for instance in instances:
        try:
            # Attempt to push the instance to the KG and retrieve update graphs
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)
        except:
            # If an exception occurs, assume instance is wrapped in a list and extract the first element
            instance                                                = instance[0]
            # Retry pushing the extracted instance to the KG
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)


def start_upload(input_path):
    """
    Initializes the upload process by retrieving SPARQL clients for different knowledge graph (KG) 
    connections and extracting relevant file path components.

    This function:
    1. Retrieves SPARQL clients for OntoSynthesis, OntoSpecies, and OntoMOP KGs.
    2. Extracts the filename (without extension) from the input file path.
    3. Determines the parent directory and the second-to-last directory in the path.

    Parameters:
        input_path (str): The full path of the file to be uploaded.

    Returns:
        tuple: A tuple containing:
            - filename_noext (str): The name of the file without its extension.
            - secondlast_subdir (str): The parent directory of the file.
            - sparql_client_synthesis: SPARQL client for the OntoSynthesis KG.
            - sparql_client_species: SPARQL client for the OntoSpecies KG.
            - sparql_client_mop: SPARQL client for the OntoMOP KG.
    """
    # Retrieve SPARQL clients for different ontology connections
    sparql_client_synthesis                                 = utils.get_client("OntoSynthesisConnection")
    sparql_client_species                                   = utils.get_client("OntoSpeciesConnection") 
    sparql_client_mop                                       = utils.get_client("OntoMOPConnection") 
    # Extract the filename from the input path (e.g., 'file.txt' -> 'file')
    filename                                                = os.path.basename(input_path)
    filename_noext                                          = os.path.splitext(filename)[0]     # Remove file extension
    # Extract parent and second-to-last directory names from the input path
    secondlast_subdir,last_subdir                           = os.path.split(input_path)
    # Return extracted filename, directory names, and SPARQL clients
    return filename_noext, secondlast_subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  

def steps_preupload(standard_step, synthesis_client, vessel_list):
    """
    Prepares and standardizes step-related data before uploading it to the knowledge graph.

    Each synthesis step has attributes such as duration, atmosphere, and vessel.
    However, filter steps do not have a duration. This function extracts and processes these
    common instances for upload.

    Parameters:
        standard_step (dict): A dictionary containing step-related data including duration, atmosphere, and vessel.
        synthesis_client (object): The SPARQL client used to interact with the OntoSynthesis knowledge graph.
        vessel_list (list): A list of existing Vessel objects to check for duplicates before creating new ones.

    Returns:
        tuple: A tuple containing:
            - vessel (Vessel): The Vessel instance associated with this step.
            - vessel_list (list): The updated list of vessels, including newly created ones.
            - duration (Duration): The Duration instance representing the step duration.
            - duration_value (Measure): The numerical value and unit of the duration.
            - atmosphere (VesselEnvironment): The atmosphere associated with the step.
            - id_hash_value (str): A unique identifier for the step duration instance.
    """
    # Print the input step for debugging purposes
    print("standard step: ", standard_step)
    # Extract the step duration if available; otherwise, assign default values
    if "duration" in standard_step:
        step_time, time_unit                    = extract_numbers_and_units(standard_step["duration"], "add")
    else:
        step_time                               = [-1]      # Default value for missing duration
        time_unit                               = "N/A"     # Default unit placeholder
    # Generate a unique identifier for the duration instance
    id_hash_value                               = str(uuid.uuid4())
    # Convert the extracted duration unit into a structured format
    duration_unit                               = get_unit(time_unit[0], synthesis_client)      # Retrieve the appropriate unit from KG
    duration_value                              = Measure(instance_iri=f"{piris.ONTOSYN_BASE}StepDuration_{id_hash_value}", hasNumericalValue=step_time[0], hasUnit=duration_unit)
    duration                                    = Duration(hasValue=duration_value)
    # Retrieve the atmosphere type for the step from the knowledge graph
    atmosphere                                  = match_atmosphere(standard_step["atmosphere"], synthesis_client)
    # Check if the vessel already exists in the list to avoid duplicates
    for ves in vessel_list:
        if ves.rdfs_label == standard_step["usedVesselName"]:
                vessel                                      = ves       # Reuse the existing vessel instance
                return vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value
    # If vessel not found, create a new vessel instance
    vessel_type                                 = match_vessel(standard_step['usedVesselType'], synthesis_client)
    vessel                                      = Vessel(rdfs_label=standard_step["usedVesselName"], hasVesselType=vessel_type)
    # Add the newly created vessel to the vessel list for future reference
    vessel_list.append(vessel)
    # Return processed values including vessel, duration, and atmosphere details
    return vessel, vessel_list, duration, duration_value, atmosphere, id_hash_value 


