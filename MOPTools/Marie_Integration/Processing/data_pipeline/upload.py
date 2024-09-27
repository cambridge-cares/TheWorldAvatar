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
from export_twa import *
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

def change_property(instance_var, property_var, value_var, client, push=False):
    # Check if the attribute exists  
    if hasattr(instance_var, property_var):
        setattr(instance_var, property_var, value_var)
    else:
        raise AttributeError(f"'{type(instance_var).__name__}' object has no attribute '{property_var}'")
    if push:
        instance_var.push_to_kg(client, recursive_depth=-1)

def upload_predefined():
    filename_noext, subdir, client, client_species, client_mop  = start_upload("")

    Ir_NA                               = CharacteristicPeak(instance_iri="http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_f6cce625-9d69-4491-bd9d-b096114db7af",rdfs_label="N/A", rdfs_comment="")
    Nmr_NA                              = CharacteristicPeak(instance_iri="http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_920795e4-f412-4ad6-807b-8da69519a332",rdfs_label="N/A", rdfs_comment="")
    KBr                                 = Species(instance_iri="http://www.theworldavatar.com/kb/ontospecies/Species_3b6489f0-a36c-4734-962b-521c8fab639b", rdfs_label="KBr")

    hydrogen                            = Element(instance_iri="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_43cfac3b-81db-4338-bfb1-b0b3386f7473", rdfs_label="Hydrogen")
    carbon                              = Element(instance_iri="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_f1a5025b-7e20-4a6a-821e-a7b6c0473b8c", rdfs_label="Carbon")
    oxygen                              = Element(instance_iri="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_6a6be1ce-2021-4634-aed4-6a77488765df", rdfs_label="Oxygen")
    nitrogen                            = Element(instance_iri="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_a9a7806c-f077-4eb2-b5b0-099d51033b7b", rdfs_label="Nitrogen")
    unknown_element                     = Element(instance_iri="http://www.daml.org/2003/01/periodictable/PeriodicTable#Element_e253b6ca-c169-4e60-b6be-46b95e045a85", rdfs_label="N/A")

    vessel_ss_teflon                    = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_eb0f5942-d36b-47b1-86f0-725c1549fa2e", rdfs_label="Teflon-lined stainless-steel vessel")
    glass_vial                          = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_90589d23-44e8-4698-acdf-bee3e44df96f", rdfs_label="glass vial")
    quartz_tube                         = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_06304c23-7926-45d2-841d-690b5de16ed0", rdfs_label="quartz tube")
    round_bottom_flask                  = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_5a7d7ec9-44d5-4280-8467-f9f624374a9d", rdfs_label="round bottom flask")
    glass_scintilation_vial             = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_b67ea47b-7849-4aac-b0fd-e2715a4ac034", rdfs_label="glass scintillation vial")
    pyrex_tube                          = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_080ad74b-950d-4651-a87c-5aa96d5ffb52", rdfs_label="pyrex tube")
    schlenk                             = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_080zd54b-230c-4341-e87g-5ta46d2fgh91", rdfs_label="schlenk flask")
    undefined_vessel                    = VesselType(instance_iri="https://www.theworldavatar.com/kg/OntoSyn/VesselType_183ad74b-950d-4631-a47c-5aa91d5ffb12", rdfs_label="N/A")
      
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
    miligrams                           = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/milligram", rdfs_label="miligram")
    mole                                = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/mole", rdfs_label="mole")
    mmole                               = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/millimole", rdfs_label="mmole") 
    mlitre                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/millilitre", rdfs_label="mlitre")
    # from TWA branch of OM:
    revolutions_per_minute              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/revolutionPerMinute-Time", rdfs_label="revolution per minute", rdfs_comment="Revolution per minute is a unit of rotational speed (or rotational frequency) for rotating machines.")
    drop                                = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/drop", rdfs_label="drop")
    unknown_unit                        = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/unknown", rdfs_label="N/A")

    percentage                          = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/percent", rdfs_label="percent")
    instances                           = [vessel_ss_teflon, glass_vial, quartz_tube, round_bottom_flask, glass_scintilation_vial, pyrex_tube, degree_celsius_hour, kelvin, degree_celsius, degree_celsius_min, duration_day, duration_h, duration_s, temperature_rate_degs, mole_per_litre, revolutions_per_minute, grams, miligrams, mole, mmole, mlitre, undefined_vessel, unknown_unit, drop, nitrogen, hydrogen, carbon, oxygen, unknown_element, percentage, KBr, Ir_NA, Nmr_NA, schlenk]
    push_component_to_kg(instances, client)

def find_patterns_and_divide_multiply(text, multiplier_flag):
    """
    multiplier_flag:
        - 0: Multiply the result by `number1`
        - 1: Multiply the result by `number2`
    """
    
    # Define patterns
    patterns = {
        'add': r'(\d*\.?\d+)\s*([a-zA-Z]+)',  # Pattern for numbers with units
        'range': r'\((\d+):(\d+)\)'  # Pattern for (number1:number2)
    }
    
    # Find 'add' pattern matches (numbers with units)
    add_matches = re.findall(patterns['add'], text)

    # If 'add' pattern is found, extract the numbers and units
    numbers = [float(match[0]) for match in add_matches]
    units = [match[1] for match in add_matches]
    
    # Now that 'add' matches exist, check for 'range' pattern
    range_matches = re.findall(patterns['range'], text)
    
    if range_matches:
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
    
    # Return the modified numbers, units, and range information
    return numbers_final, units, divisors

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
    

def species_querying(client, species_label):
    # avoid linking all to N/A instance:
    species_label               = [item for item in species_label if item != 'N/A']
    insert_string               = ""
    # Loop through each element in the list
    for label in species_label:
        # Append each formatted element to the result string
        insert_string += f""" "{label}" """
    
    query = f"""
        PREFIX skos:    <http://www.w3.org/2004/02/skos/core#>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf:     <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?Species WHERE {{
        ?Species a os:Species .
        VALUES ?Text {{{insert_string}}}
        ?Species (((os:hasIUPACName|os:hasMolecularFormula|os:hasSMILES)/os:value)|rdfs:label|rdf:label|skos:altLabel|<http://www.w3.org/2000/01/rdf-schema/label>) ?Text . 
        }}"""
    print("species query: ", query)
    query_result                    = client.perform_query(query)
    print("query result: ", query_result)
    return query_result

def mop_querying(client, CCDC_number, mop_formula, mop_name):
    CCDC_number             = remove_na(CCDC_number)
    mop_formula             = remove_na(mop_formula)
    mop_name                = remove_na(mop_name)
    print("querying for mop: ", CCDC_number, mop_formula, mop_name)
    insert_string               = ""
    # break down mop list of strings in a way to insert in a value sparql statement
    for label in mop_name:
        # Append each formatted element to the result string
        insert_string += f""" "{label}" """
    print("mop querying: ", insert_string)
    # somehow the python derivation agent query fails with both numbers and strings in value so it is split for ccdc and not
    query = f"""
        PREFIX om:      <https://www.theworldavatar.com/kg/ontomops/>
        PREFIX os:      <http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#>
        PREFIX rdfs:    <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX xsd: 	<http://www.w3.org/2001/XMLSchema#>
        SELECT ?MOPIRI
        WHERE {{
        ?MOPIRI a <https://www.theworldavatar.com/kg/ontomops/MetalOrganicPolyhedron>                        .
        VALUES ?Text {{"{CCDC_number}" "{mop_formula}" {insert_string}}}
        ?MOPIRI (<https://www.theworldavatar.com/kg/ontomops/hasMOPFormula>|<https://www.theworldavatar.com/kg/ontomops/mopAltLabel>|<https://www.theworldavatar.com/kg/ontomops/hasCCDCNumber>) ?Text .  
        }}
        GROUP BY ?MOPIRI"""
    out                     = client.perform_query(query)
    print("used query: ", query)
    print("MOp query result returned: ", out)
    return out

def transformation_querying(client, mop_name):
    print("mop name: ", mop_name)
    insert_string               = ""
    for label in mop_name:
        # Append each formatted element to the result string
        if label != "N/A":
            insert_string += f""" "{label}" """
    print("mop querying: ", insert_string)
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
        VALUES ?Text {{{insert_string}}}
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
        case "seconds" | "second" | "s" :
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/second-Time", client, recursive_depth=-1)[0]                      
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
        
        case _: 
            print(f"Unit was not recognized. Check the following unit: {unit_name} \n")            
            unit                = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/unknown", client, recursive_depth=-1)[0]                                  
       

    """
    mole_per_litre                              = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/molePerLitre", rdfs_label="mole per litre")
    revolutions_per_minute                      = UnitOfMeasure(instance_iri="http://www.ontology-of-units-of-measure.org/resource/om-2/revolutionPerMinute-Time", rdfs_label="revolutions per minute")
    """
    return unit

def heatchill_upload(client, vessel, heatchill_step):
    if heatchill_step["targetTemperature"] == "room temperature":
        temp                                    = [25.0]
        temperature_unit                        = get_unit("C", client) 
    else:
        temp, temp_unit                         = extract_numbers_and_units(heatchill_step["targetTemperature"],"temp")
        temperature_unit                        = get_unit(temp_unit[0], client) 
        print("temperature: ", temp, temp_unit)
    heat_time, time_unit                        = extract_numbers_and_units(heatchill_step["duration"], "add")
    heat_rate, rate_unit                        = extract_numbers_and_units(heatchill_step["heatingCoolingRate"], "temp")
    
    print("heatingrate: ", heat_rate, rate_unit)
    print("duration: ", heat_time, time_unit)
    # temperature 
    id_hash_value                               = str(uuid.uuid4())
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
    vessel_type                                 = match_vessel(heatchill_step['usedVesselType'], client)
    if vessel.rdfs_label != heatchill_step["usedVesselName"]:
        print("new vessel")
        vessel                                  = Vessel(rdfs_label=heatchill_step["usedVesselName"],hasVesselType=vessel_type)
    heatchill_device                            = heatchill_step["usedDevice"]
    # put everything together
    heat_chill                                  = HeatChill(hasVessel=vessel, hasStepDuration=duration, hasTargetTemperature=target_temperature, hasTemperatureRate=temperature_rate, hasVacuum=heatchill_step["underVacuum"], isSealed=heatchill_step["sealedVessel"], IsStirred=heatchill_step['stir'], hasOrder=heatchill_step['stepNumber'], rdfs_comment=heatchill_step['comment'])
    components = [temperature_value, target_temperature, temperature_value, temperature_rate, duration_value, duration, heat_chill, vessel]
    push_component_to_kg(components, client)
    return  heat_chill, vessel

def upload_inputChem(species_name, amounts, synthesis_client, species_client):
    phase_components                                        = []
    phase_component_concentrations                          = [] 
    add_value, add_unit                                     = extract_numbers_and_units(amounts, "add")
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
        unit_instance                                       = get_unit(unit, synthesis_client)
        # Create a `ScalarValue` instance and add it to the list
        scalar_value_instance                               = ScalarValue(hasNumericalValue=value, hasUnitOfMeasure=unit_instance)
        # make sure to instantiate new phase component for each species in the mixture
        scalar_values.append(scalar_value_instance)
        scalar_value_instance.push_to_kg(synthesis_client, -1)
    phase_component_concentration                           = PhaseComponentConcentration(hasValue=set(scalar_values))
    phase_component_concentrations.append(phase_component_concentration)
    phase_component                                         = PhaseComponent(representsOccurenceOf=species, hasProperty=phase_component_concentration)
    phase_components.append(phase_component)
    composition                                             = Composition(comprisesDirectly=phase_component_concentrations)
    single_phase                                            = SinglePhase(isComposedOfSubsystem=phase_components, hasComposition=composition)     
    material                                                = Material(thermodynamicBehaviour=single_phase)
    chemical_input                                          = ChemicalInput(referencesMaterial=material)
    components = [phase_component_concentration, phase_component, composition, single_phase, material, chemical_input]
    push_component_to_kg(components, synthesis_client)
    return chemical_input


def add_upload_new(add_step, vessel, synthesis_client, species_client):
    print("add step inside: ", add_step)
    vessel_type                                             = match_vessel(add_step['usedVesselType'], synthesis_client)
    if vessel.rdfs_label != add_step["usedVesselName"]:
        print("new vessel")
        vessel                                              = Vessel(rdfs_label=add_step["usedVesselName"], hasVesselType=vessel_type)
    
    chemical_input                                          = upload_inputChem(add_step["addedChemicalName"], add_step["addedChemicalAmount"], synthesis_client, species_client)
    add_class                                               = Add(hasOrder=add_step['stepNumber'], hasVessel=vessel, hasAddedChemicalInput=chemical_input, isStirred=add_step['stir'], hasTargetPh=float(add_step['targetPH']), rdfs_comment=add_step['comment'])  
    components = [add_class, vessel]
    push_component_to_kg(components, synthesis_client)
    return add_class, chemical_input, vessel

def filter_upload(filter_step, vessel, synthesis_client, species_client):
    print("filter step: ", filter_step)
    washing_amount                                      = filter_step['washingSolventAmount']
    filter_value, filter_unit                           = extract_numbers_and_units(washing_amount, "add")
    vessel_type                                         = match_vessel(filter_step['usedVesselType'], synthesis_client)
    if vessel.rdfs_label != filter_step["usedVesselName"]:
        print("new vessel")
        vessel                                          = Vessel(rdfs_label=filter_step["usedVesselName"], hasVesselType=vessel_type)

    chemical_input                                      = upload_inputChem(filter_step["washingSolventName"], filter_step["washingSolventAmount"], synthesis_client, species_client)
    filter_class                                        = Filter(hasOrder=filter_step["stepNumber"], isRepeated=filter_step["numberOfFiltrations"], isVacuumFiltration=filter_step["vacuumFiltration"],hasWashingSolvent=chemical_input, rdfs_comment=filter_step["comment"], hasVessel=vessel)  
    components = [filter_class, vessel]
    push_component_to_kg(components, synthesis_client)
    return filter_class, chemical_input, vessel



def standard_step_upload(standard_input, vessel, step_type, chemicals_list, synthesis_client, species_client):

    if "Sonicate" in standard_input:
        standard_step                           = standard_input["Sonicate"]
        vessel, duration, duration_value        = steps_preupload(standard_step, synthesis_client)
        sonication                              = Sonication(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel)
        components                              = [duration_value, duration, vessel, sonication]
        push_component_to_kg(components, synthesis_client)
        return sonication, vessel, chemicals_list
    
    elif "Add" in standard_input:
        standard_step                                           = standard_input["Add"]
        vessel, duration, duration_value                        = steps_preupload(standard_step, synthesis_client)
        chemical_input                                          = upload_inputChem(standard_step["addedChemicalName"], standard_step["addedChemicalAmount"], synthesis_client, species_client)
        add_class                                               = Add(hasOrder=standard_step['stepNumber'], hasVessel=vessel, hasAddedChemicalInput=chemical_input, isStirred=standard_step['stir'], hasTargetPh=float(standard_step['targetPH']), isDropwise=standard_step["addedDropwise"],rdfs_comment=standard_step['comment'])  
        components = [add_class, vessel, duration, duration_value]
        push_component_to_kg(components, synthesis_client)
        chemicals_list.append(chemical_input)
        return add_class, vessel, chemicals_list
    
    elif "HeatChill" in standard_input:
        standard_step                                           = standard_input["HeatChill"]
        vessel, duration, duration_value                        = steps_preupload(standard_step, synthesis_client)
        if standard_step["targetTemperature"] == "room temperature":
            temp                                    = [25.0]
            temperature_unit                        = get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = extract_numbers_and_units(standard_step["targetTemperature"],"temp")
            temperature_unit                        = get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        heat_time, time_unit                        = extract_numbers_and_units(standard_step["duration"], "add")
        heat_rate, rate_unit                        = extract_numbers_and_units(standard_step["heatingCoolingRate"], "temp")
        
        print("heatingrate: ", heat_rate, rate_unit)
        print("duration: ", heat_time, time_unit)
        # temperature 
        id_hash_value                               = str(uuid.uuid4())
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetTemperature_{id_hash_value}", hasValue=temperature_value)

        # heat rate
        rate_unit                                   = get_unit(rate_unit[0], synthesis_client) 
        rate_value                                  = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureRate_{id_hash_value}", hasNumericalValue=heat_rate[0], hasUnit=rate_unit)
        temperature_rate                            = TemperatureRate(hasValue=rate_value) 
        # put everything together
        heat_chill                                  = HeatChill(hasVessel=vessel, hasStepDuration=duration, hasTargetTemperature=target_temperature, hasTemperatureRate=temperature_rate, hasVacuum=standard_step["underVacuum"], isSealed=standard_step["sealedVessel"], IsStirred=standard_step['stir'], hasOrder=standard_step['stepNumber'], rdfs_comment=standard_step['comment'])
        components = [temperature_value, target_temperature, temperature_value, temperature_rate, duration_value, duration, heat_chill, vessel]
        push_component_to_kg(components, synthesis_client) 
        return heat_chill, vessel, chemicals_list
    
    elif "Filter" in standard_input:
        standard_step                                       = standard_input["Filter"]
        vessel, duration, duration_value                    = steps_preupload(standard_step, synthesis_client)
        chemical_input                                      = upload_inputChem(standard_step["washingSolventName"], standard_step["washingSolventAmount"], synthesis_client, species_client)
        filter_class                                        = Filter(hasOrder=standard_step["stepNumber"], isRepeated=standard_step["numberOfFiltrations"], isVacuumFiltration=standard_step["vacuumFiltration"],hasWashingSolvent=chemical_input, rdfs_comment=standard_step["comment"], hasVessel=vessel)  
        components = [filter_class, vessel, duration, duration_value ]
        push_component_to_kg(components, synthesis_client)
        chemicals_list.append(chemical_input)
        return filter_class, vessel, chemicals_list
    
    elif "Stir" in standard_input:
        standard_step                                       = standard_input["Stir"]
        vessel, duration, duration_value                    = steps_preupload(standard_step, synthesis_client)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)
        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/StirTemperature_{id_hash_value}", hasValue=temperature_value)
        
        stir                                        = Stir(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasStirringTemperature=target_temperature)
        components                                  = [duration_value, duration, stir, vessel]
        push_component_to_kg(components, synthesis_client)
        return stir, vessel, chemicals_list
    
    elif "Crystallization" in standard_input:
        standard_step                                       = standard_input["Crystallization"]
        vessel, duration, duration_value                    = steps_preupload(standard_step, synthesis_client)
        if standard_step["targetTemperature"] == "room temperature" or standard_step["targetTemperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = extract_numbers_and_units(standard_step["targetTemperature"], "temp")
            temperature_unit                        = get_unit(temp_unit[0], synthesis_client) 
            print("temperature: ", temp, temp_unit)

        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit)
        target_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TargetTemperature_{id_hash_value}", hasValue=temperature_value)
        crystallization                             = Crystallization(hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasStepDuration=duration, rdfs_comment=standard_step["comment"], hasCrystallizationTargetTemperature=target_temperature)
        components                                  = [duration_value, duration, temperature_value, target_temperature, crystallization, vessel]
        push_component_to_kg(components, synthesis_client)
        return crystallization, vessel, chemicals_list
    

    elif "Dry" in standard_input:
        standard_step                                       = standard_input["Dry"]
        vessel, duration, duration_value                    = steps_preupload(standard_step, synthesis_client)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = get_unit(temp_unit[0], synthesis_client) 
        pres, pres_unit                             = extract_numbers_and_units(standard_step["pressure"], "temp")
        pressure_unit                               = get_unit(pres_unit[0], synthesis_client) 

        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit, rdfs_comment=standard_step["temperature"])
        pressure_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/PressureValue_{id_hash_value}",hasNumericalValue=pres[0], hasUnit=pressure_unit, rdfs_comment=standard_step["pressure"])
        drying_temperature                          = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/DryingTemperature_{id_hash_value}", hasValue=temperature_value)
        drying_pressure                             = Pressure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/DryingPressure_{id_hash_value}", hasValue=pressure_value)
        dry                                         = Dry(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasDryingTemperature=drying_temperature, hasDryingPressure=drying_pressure, rdfs_comment=standard_step["comment"])
        components                                  = [duration, duration_value, temperature_value, drying_temperature, drying_pressure, dry, pressure_value, vessel]
        push_component_to_kg(components, synthesis_client)
        return dry, vessel, chemicals_list
    
    elif "Evaporate" in standard_input:
        standard_step                                       = standard_input["Evaporate"]
        vessel, duration, duration_value                    = steps_preupload(standard_step, synthesis_client)
        if standard_step["temperature"] == "room temperature" or standard_step["temperature"] == "RT":
            temp                                    = [25.0]
            temperature_unit                        = get_unit("C", synthesis_client) 
        else:
            temp, temp_unit                         = extract_numbers_and_units(standard_step["temperature"], "temp")
            temperature_unit                        = get_unit(temp_unit[0], synthesis_client) 
        pres, pres_unit                             = extract_numbers_and_units(standard_step["pressure"], "temp")
        pressure_unit                               = get_unit(pres_unit[0], synthesis_client) 

        temperature_value                           = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/TemperatureValue_{id_hash_value}",hasNumericalValue=temp[0], hasUnit=temperature_unit, rdfs_comment=standard_step["temperature"])
        pressure_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/PressureValue_{id_hash_value}",hasNumericalValue=pres[0], hasUnit=pressure_unit, rdfs_comment=standard_step["pressure"])
        evap_temperature                            = Temperature(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/EvaporationTemperature_{id_hash_value}", hasValue=temperature_value)
        evap_pressure                               = Pressure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/EvaporationPressure_{id_hash_value}", hasValue=pressure_value)
        evaporate                                   = Evaporate(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasEvaporationTemperature=evap_temperature, hasEvaporationPressure=evap_pressure, rdfs_comment=standard_step["comment"])  
        components                                  = [duration_value, duration, vessel, temperature_value, evap_temperature, evap_pressure, evaporate, pressure_value]
        push_component_to_kg(components, synthesis_client)
        return evaporate, vessel, chemicals_list

    elif "Transfer" in standard_input:
        standard_step                           = standard_input["Transfer"]
        vessel, duration, duration_value        = steps_preupload(standard_step, synthesis_client)
        targetvessel_type                       = match_vessel(standard_step["targetVesselType"], synthesis_client)
        targetvessel                            = Vessel(rdfs_label=standard_step["targetVesselName"], hasVesselType=targetvessel_type)
        transfer                                = Transfer(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, isTransferedTo=targetvessel, rdfs_comment=standard_step["comment"])
        components                              = [duration_value, duration, transfer, vessel, targetvessel]
        push_component_to_kg(components, synthesis_client)
        return transfer, targetvessel, chemicals_list
    
    elif "Dissolve" in standard_input:
        standard_step                           = standard_input["Dissolve"]
        vessel, duration, duration_value        = steps_preupload(standard_step, synthesis_client)
        chemical_input                          = upload_inputChem(standard_step["solventName"], standard_step["solventAmount"], synthesis_client, species_client)
        dissolve                                = Dissolve(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasSolventDissolve=chemical_input, rdfs_comment=standard_step["comment"])
        components                              = [duration_value, duration, dissolve, vessel]
        chemicals_list.append(chemical_input)
        push_component_to_kg(components, synthesis_client)
        return dissolve, vessel, chemicals_list
    
    elif "Separate" in standard_input:
        standard_step                           = standard_input["Separate"]
        vessel, duration, duration_value        = steps_preupload(standard_step, synthesis_client)
        chemical_input                          = upload_inputChem(standard_step["solventName"], standard_step["solventAmount"], synthesis_client, species_client)
        separate                                = Separate(hasStepDuration=duration, hasOrder=standard_step["stepNumber"], hasVessel=vessel, hasSeparationSolvent=chemical_input , rdfs_comment=standard_step["comment"])
        components                              = [duration_value, duration, separate, duration_value, vessel]
        chemicals_list.append(chemical_input)
        push_component_to_kg(components, synthesis_client)
        return separate, vessel, chemicals_list

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

def instantiate_input(chemical_formula, species_name, client_species, client_synthesis):
    # search the ontospecies and ontosynthesis blazegraphs for existing instances
    species_iri                                             = str(uuid.uuid4())
    triples                                                 = species_querying(client_synthesis, species_name)
    print("OntoSpecies results: ", triples)
    if triples == None or triples == []:
        triples                                             = species_querying(client_species, species_name)
        if triples == None or triples == []:
            species                                         = Species(label=chemical_formula, altLabel=species_name)
            # Ontospecies uses different base IRIs for rdf type and the actual instance IRI.
            species.instance_iri                            = f"http://www.theworldavatar.com/kb/ontospecies/Species_{species_iri}"
        else:
            # when pulled from species we want to instantiate a new instance. The second time it is pulled the altlabels will be updated with new ones from the paper.
            try:
                species                                     = Species(instance_iri=triples[0]["Species"] ,label=chemical_formula, altLabel=species_name)
            except:
                # there already exists a species with the IRI but with different labels than before -> query syn kg and add label
                species                                     = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
                # update if not already saved (avoids 1000s of duplicates)
                species                                     = update_alt_label(species, species_name=species_name)
            #species                                         = update_alt_label(species, species_name=species_name)

    else:
        print("Success: ", triples[0]["Species"])
        # species                                             = Species(instance_iri=triples[0]["Species"] ,label=chemical_formula, altLabel=species_name)
        species                                             = Species.pull_from_kg(triples[0]["Species"], client_synthesis, recursive_depth=-1)[0]
        # update if not already saved (avoids 1000s of duplicates)
        species                                             = update_alt_label(species, species_name=species_name)
    return species

def instantiate_output(ccdc_number, chemical_formula, mop_names, yield_str, client_mop, client_synthesis):
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
    # give instantiated mop the queried IRI
    if mop_iri != [] and mop_iri != None:
        mop.instance_iri            = mop_iri[0]["MOPIRI"] 
    # Yield
    uuid_id                                 = str(uuid.uuid4())
    unit                                    = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/percent", client_synthesis, recursive_depth=-1)[0]
    print("yield number", yield_str)
    yield_str                               = yield_str.replace('%', '')
    # set -1 for N/A
    yield_str                               = yield_str.replace("N/A", "-1")
    yield_value                             = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/YieldValue_{uuid_id}", hasNumericalValue=float(yield_str), hasUnit=unit)
    yield_instance                          = AmountOfSubstanceFraction(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/Yield_{uuid_id}", hasValue=yield_value)
    chemical_output                         = ChemicalOutput(isRepresentedBy=mop)
    return mop, chemical_output, yield_value, yield_instance 

def doi_from_path(path:str):
    filename                                    = os.path.basename(path)
    # Split the filename into the two parts using '_'
    parts                                       = filename.split('_')
    # Remove the '.txt' extension from the second part
    number2                                     = os.path.splitext(parts[-1])[0]
    # Combine the parts in the desired format
    return f"{parts[-2]}/{number2}"


def chemicals_upload_json(input_path, output_path):
    filename_noext, subdir, client_synthesis, client_species, client_mop  = start_upload(input_path)
    subdir_name                                                 = subdir.split("_", 1)[0]
    subdir_name                                                 = subdir_name.replace("../Data/", "")
    # go through json file:
    chemicals_json                              = read_json_file(f"../Data/{subdir_name}_chemicals1/{filename_noext}.json")['synthesisProcedures']
    # general information
    doi                                         = doi_from_path(input_path)
    document                                    = Document(doi=doi)
    # go trough the different procedures described in the paper: 
    for synthesis in chemicals_json:
        print("synthesis: ", synthesis)
        chemicals                               = synthesis['steps'][0]
        print("chemcials: ", chemicals)
        # output chemical(currently only one otherwise one would need the same for loop as input):
        output_chemical                         = chemicals['outputChemical'][0]
        print("output chemical: ", output_chemical)
        # add yield to chemical output / make subclass of product!
        mop, chemical_output, yield_value, yield_instance       = instantiate_output(output_chemical["CCDCNumber"], output_chemical["chemicalFormula"], output_chemical["names"], output_chemical["yield"], client_mop, client_synthesis)
        
        # input chemicals:
        chemical_list                               = []
        for chemical in chemicals['inputChemicals']:
            print("input chemical: ", chemical)
            species_name                            = chemical["names"]
            species_name.append(chemical['chemicalFormula'])
            species                                 = instantiate_input(chemical['chemicalFormula'], species_name, client_species, client_synthesis)           
            added_amount                            = chemical['amountOfChemical']
            amount_value, amount_unit               = extract_numbers_and_units(added_amount, "add")
            # Initialize an empty list to hold `ScalarValue` instances
            scalar_values = []
            # Iterate over each pair of unit and value
            for unit, value in zip(amount_unit, amount_value):
                # Get the unit using the `get_unit` function
                print("unit and value: ", unit, value)
                unit_instance                                   = get_unit(unit, client_synthesis)
                # Create a `ScalarValue` instance and add it to the list
                scalar_value_instance                           = ScalarValue(hasNumericalValue=value, hasUnitOfMeasure=unit_instance)
                scalar_values.append(scalar_value_instance)
            # ontocape initialization
            phase_component_concentration                       = PhaseComponentConcentration(hasValue=set(scalar_values))
            phase_component                                     = PhaseComponent(representsOccurenceOf=species, hasProperty=phase_component_concentration)
            composition                                         = Composition(comprisesDirectly=phase_component_concentration)
            single_phase                                        = SinglePhase(isComposedOfSubsystem=phase_component, hasComposition=composition)     
            material                                            = Material(thermodynamicBehaviour=single_phase)
            chemical_input                                      = ChemicalInput(referencesMaterial=material)
            chemical_list.append(chemical_input)
            components_input                                    = ([phase_component_concentration, phase_component, composition, single_phase, material, chemical_input, chemical_output, yield_value, yield_instance])
            push_component_to_kg(components_input, client_synthesis)
            # putting it together as transformation and synthesis (input chemicals needed for synthesis)
            # check if product already exists:
        syn_prod                                            = transformation_querying(client_synthesis, output_chemical["names"])  
        # no entry => make a new one
        if syn_prod == []:
            chemical_synthesis                              = ChemicalSynthesis(retrievedFrom=document, hasChemicalInput=chemical_list) 
            chemical_transformation                         = ChemicalTransformation(hasChemicalOutput=chemical_output, isDescribedBy=chemical_synthesis)
        # otherwise use existing one
        else: 
            chemical_transformation                         = ChemicalTransformation.pull_from_kg(syn_prod[0]["chemicalTrans"], sparql_client=client_synthesis, recursive_depth=-1) 
            chemical_synthesis_set                          = chemical_transformation[0].isDescribedBy  
            # get IRI from set
            for synthesis in chemical_synthesis_set:
                instance_iri = synthesis.instance_iri
            chemical_synthesis                              = ChemicalSynthesis.pull_from_kg(instance_iri, sparql_client=client_synthesis, recursive_depth=-1)[0]
            for input_chem in chemical_list:
                chemical_synthesis.hasChemicalInput.add(input_chem)
        components_output                                   = [chemical_output, mop, chemical_transformation, yield_value, yield_instance, chemical_synthesis]
        push_component_to_kg(components_output, client_synthesis)


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
        
def characterisation_upload(input_path, output_path):
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = start_upload(input_path)
    subdir_name                                                 = subdir.split("_", 1)[0]
    characterisation_json                                       = read_json_file(f"../Data/{subdir_name}_characterisation/{filename_noext}.json")["Devices"][0]
    elemental_device_name                                       = characterisation_json["ElementalAnalysisDevice"]["deviceName"]
    # general information for all procedures of the paper
    elemental_device                                            = ElementalAnalysisDevice(rdfs_label=elemental_device_name)
    hnmr_device_name                                            = characterisation_json["HNMRDevice"]["deviceName"]
    nmr_device                                                  = InstrumentType(rdfs_label=hnmr_device_name)
    hnmr_frequency                                              = characterisation_json["HNMRDevice"]["frequency"]
    ir_device_name                                              = characterisation_json["InfraredSpectroscopyDevice"]["deviceName"]
    ir_device                                                   = InstrumentType(rdfs_label=ir_device_name)
    # synthesis specific information
    # http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_f6cce625-9d69-4491-bd9d-b096114db7af
    for entry in characterisation_json["Characterisation"]:
        mop_name                                                = entry["productNames"]
        mop_ccdc                                                = entry["productCCDCNumber"]
        try:
            transformation_iri                                  = transformation_querying(syn_client, mop_name=mop_name)[0]
        except:
            transformation_iri                                  = transformation_querying(syn_client, mop_name=mop_name)
        if transformation_iri == []:
            transformation_iri                                  = transformation_querying(syn_client, mop_name=mop_ccdc)[0]
        chemical_transformation                                 = ChemicalTransformation.pull_from_kg(transformation_iri["chemicalTrans"], sparql_client=syn_client, recursive_depth=-1) 
        chemical_output                                         = chemical_transformation[0].hasChemicalOutput
        # NMR
        nmr                                                     = entry['HNMR']
        solvent                                                 = Solvent(rdfs_label=nmr["solvent"])
        nmr_shifts                                              = extract_numbers_and_brackets(nmr["shifts"])
        nmr_peaks                                               = []
        for shift in nmr_shifts:
            if type(shift) != tuple:
                nmr_peak                                        = CharacteristicPeak(hasX1=shift, rdfs_comment="")
                nmr_peak.push_to_kg(syn_client, recursive_depth=-1)
            elif shift == "N/A" or shift == " N/A":
                nmr_peak                                        = CharacteristicPeak.pull_from_kg("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_920795e4-f412-4ad6-807b-8da69519a332", syn_client, -1)
            else:
                nmr_peak                                        = CharacteristicPeak(hasX1=shift[0], rdfs_comment=shift[1])
                nmr_peak.push_to_kg(syn_client, recursive_depth=-1)
            nmr_peaks.append(nmr_peak)
            
        nmr_spectra_graph                                       = SpectraGraph(hasX1Axis="ppm", hasPeak=nmr_peaks)
        hnmr                                                    = HNMRSpectra(hasSolvent=solvent, hasInstrumentType=nmr_device, hasSpectraGraph=nmr_spectra_graph)
        # elemental analysis
        elemental_analysis                                      = entry["ElementalAnalysis"]
        calc_analysis_class, chemical_output                    = elemental_analysis_upload(elemental_analysis["weightPercentageCalculated"], syn_client, chemical_output, elemental_analysis["chemicalFormula"], "")
        exp_analysis_class, chemical_output                     = elemental_analysis_upload(elemental_analysis["weightPercentageExperimental"], syn_client, chemical_output, elemental_analysis["chemicalFormula"], elemental_device)
        # IR
        ir                                                      = entry['InfraredSpectroscopy']
        bands = extract_numbers_and_brackets(ir["bands"])
        peaks                                                   = []
        for band in bands:
            if type(band) != tuple:
                peak                                            = CharacteristicPeak(hasX1=band, rdfs_comment="")
                peak.push_to_kg(syn_client, recursive_depth=-1)
            elif band == "N/A":
                peak                                            = CharacteristicPeak.pull_from_kg("http://www.theworldavatar.com/ontology/ontospecies/OntoSpecies.owl#CharacteristicPeak_f6cce625-9d69-4491-bd9d-b096114db7af", syn_client, -1)
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
        push_component_to_kg(chemical_output, syn_client)
        # upload all
        components                                              = [elemental_device, ir_device, calc_analysis_class, exp_analysis_class, spectra_graph, ft_spectra, hnmr, nmr_spectra_graph, solvent]
        push_component_to_kg(components, syn_client)
    return
    
def upload_cbu(input_path):    
    filename_noext, subdir, syn_client, sparql_client_species, sparql_client_mop  = start_upload(input_path)
    subdir_name                                                 = subdir.split("_", 1)[0]
    cbu_json                                                    = read_json_file(f"../Data/{subdir_name}_cbu/{filename_noext}.json")
    CCDC_num                                                    = cbu_json["mopFormula"]
    species_iri_1                                               = species_querying(syn_client, cbu_json["cbuSpeciesNames1"])
    species_iri_2                                               = species_querying(syn_client, cbu_json["cbuSpeciesNames2"])
    species1                                                    = Species.pull_from_kg(species_iri_1[0]["Species"] ,syn_client,1)
    species2                                                    = Species.pull_from_kg(species_iri_2[0]["Species"] ,syn_client,1)
    cbu1                                                        = ChemicalBuildingUnit(hasCBUFormula=cbu_json["cbuFormula1"], isUsedAsChemical=species1)
    cbu2                                                        = ChemicalBuildingUnit(hasCBUFormula=cbu_json["cbuFormula2"], isUsedAsChemical=species2)
    mop_iri                                                     = mop_querying(syn_client, CCDC_num, "", "")
    mop_instance                                                = MetalOrganicPolyhedron.pull_from_kg(mop_iri[0]["MOPIRI"] ,syn_client,1)
    print("mop instance:", mop_instance)
    mop_instance.hasChemicalBuildingUnit.add(cbu1)
    mop_instance.hasChemicalBuildingUnit.add(cbu2)
    print("mop instance", mop_instance)
    components                                                  = [cbu1, cbu2, mop_instance]
    #push_component_to_kg(components, syn_client)

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
        value                                                   = float(value)
        id_hash_value                                           = str(uuid.uuid4())
        unit                                                    = UnitOfMeasure.pull_from_kg("http://www.ontology-of-units-of-measure.org/resource/om-2/percent", syn_client, recursive_depth=-1)[0]
        measure                                                 = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/MassFractionValue_{id_hash_value}", hasNumericalValue=value, hasUnit=unit)
        mass_fraction                                           = MassFraction(hasValue=measure)
        element_inst                                            = match_element(element, client=syn_client)
        element_weight_precentage                               = ElementWeightPercentage(hasMassFraction=mass_fraction, isReferingToElement=element_inst)
        element_percentages.append(element_weight_precentage)
        components                                              = [measure, mass_fraction, element_weight_precentage]
        push_component_to_kg(components, syn_client)
    return element_percentages

def push_component_to_kg(instances:list, client, recursive_depth=-1):
    
    for instance in instances:
        try:
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)
        except:
            instance                                                = instance[0]
            g_to_remove, g_to_add                                   = instance.push_to_kg(client, recursive_depth)
def steps_preupload(standard_step, synthesis_client):
    step_time, time_unit                        = extract_numbers_and_units(standard_step["duration"], "add")
    id_hash_value                               = str(uuid.uuid4())
    # duration 
    duration_unit                               = get_unit(time_unit[0], synthesis_client) 
    duration_value                              = Measure(instance_iri=f"https://www.theworldavatar.com/kg/OntoSyn/StepDuration_{id_hash_value}", hasNumericalValue=step_time[0], hasUnit=duration_unit)
    duration                                    = Duration(hasValue=duration_value)

    # Vessel:
    vessel_type                                 = match_vessel(standard_step['usedVesselType'], synthesis_client)
    vessel                                      = Vessel(rdfs_label=standard_step["usedVesselName"], hasVesselType=vessel_type)
    return vessel, duration, duration_value 
def upload_steps(input_path, output_path):
    filename_noext, subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  = start_upload(input_path)
    subdir_name                                                 = subdir.split("_", 1)[0]
    print("input path: ", input_path)
    print("subdir: ", subdir_name)
    subdir_name                                                 = subdir_name.replace("../Data/", "")
    synthesis_json                                              = read_json_file(f"../Data/{subdir_name}_steps/{filename_noext}.json")["Synthesis"]
    print("actual full data: ", synthesis_json)

    for entry in synthesis_json:
        mop_name                                                = entry["productNames"]
        mop_name.append(entry["productCCDCNumber"])
        transformation_iri                                      = transformation_querying(sparql_client_synthesis, mop_name=mop_name)
        print("transformation iri: ", transformation_iri)
        if transformation_iri == []:
            print("generating new Transformation!")
            doi                                                 = doi_from_path(input_path)
            document                                            = Document(doi=doi)
            mop, chemical_output, yield_value, yield_instance   = instantiate_output(entry["productCCDCNumber"], "", entry["productNames"], "-1", sparql_client_mop, sparql_client_synthesis)
            
            chemical_synthesis                                  = ChemicalSynthesis(retrievedFrom=document) 
            chemical_transformation                             = ChemicalTransformation(hasChemicalOutput=chemical_output, isDescribedBy=chemical_synthesis)
            components                                          = [chemical_output, mop, chemical_transformation, chemical_synthesis]
            transformation_iri                                  = [{'chemicalTrans':chemical_transformation.instance_iri}]
            print("mop: ", mop)
            print(chemical_output)
            push_component_to_kg(components, sparql_client_synthesis)
        step_list                                               = []
        chemicals_list                                          = []
        filename                                                = os.path.basename(input_path)
        # Split the filename into the two parts using '_'
        number1, number2_with_extension                         = filename.split('_')
        # Remove the '.txt' extension from the second part
        number2                                                 = os.path.splitext(number2_with_extension)[0]
        # Combine the parts in the desired format
        doi                                                     = f"{number1}/{number2}"
        document                                                = Document(doi=doi)
        # instantiate empty vessel for the first iteration
        vessel                                                  = Vessel()
        for step_dat in entry["steps"]:
            print("step data: ", step_dat)
            step_class, vessel, chemicals_list                  = standard_step_upload(step_dat["Stir"], vessel, "Stir", chemicals_list, sparql_client_synthesis, sparql_client_species)
            step_list.append(step_class)
            
        print("finished steps!")
        chemical_transformation                                 = ChemicalTransformation.pull_from_kg(transformation_iri[0]["chemicalTrans"], sparql_client_synthesis, recursive_depth=1)   
        print("pulled transformation IRI")
        chemical_synthesis                                      = ChemicalSynthesis(hasSynthesisStep=step_list, retrievedFrom=document, hasChemicalInput=chemicals_list) 
        chemical_transformation[0].isDescribedBy.add(chemical_synthesis)
        components                                              = [chemical_synthesis, chemical_transformation]
        print("Started pushing synthesis and transformation")
        push_component_to_kg(components, sparql_client_synthesis, 2)
        print("Ended pushing synthesis and transformation")

def start_upload(input_path):
    sparql_client_synthesis                                 = get_client("OntoSynthesisConnection")
    sparql_client_species                                   = get_client("OntoSpeciesConnection") 
    sparql_client_mop                                       = get_client("OntoMOPConnection") 
    filename                                                = os.path.basename(input_path)
    filename_noext                                          = os.path.splitext(filename)[0]
    secondlast_subdir,last_subdir                           = os.path.split(input_path)
    return filename_noext, secondlast_subdir, sparql_client_synthesis, sparql_client_species, sparql_client_mop  

def upload(input_path, output_path):
    # predefined instances. 
    upload_predefined()
    # chemical instances 
    chemicals_upload_json(input_path, "")
    #raise Exception("stop")
    # uploadd steps
    upload_steps(input_path, "")
    # characterisation
    #characterisation_upload(input_path, "")

def main():
    #input_path                                                  = "../Data/first10_prompt54/10.1002_anie.201900519.json"
    #upload(input_path, "")
    #OntoSyn         = "http://www.theworldavatar.com/ontology/ontosyn/OntoSyn.owl"
    #upload_predefined()
    
    input_path                                              = f"../Data/fift10_chemicals1/10.1021_acsami.8b02015.json"
    #chemicals_upload_json(input_path=input_path, output_path="")
    #upload(input_path, "")
    #upload_cbu(input_path)
    upload_steps(input_path=input_path, output_path="")
    #characterisation_upload(input_path, "")


if __name__ == "__main__":
    main()

