###############################################
# Authors: Feroz Farazi (msff2@cam.ac.uk) #    
# Date: 27 Feb 2023                           #
###############################################

# The purpose of this module is to provide functions to retrieve object and
# datatype properties directly and indirectly associated with a class from
# the KG by traversing through the range relationship of the object properties

from SPARQLWrapper import SPARQLWrapper, JSON, POST, BASIC
import requests, json as js
import os
from query_templates import get_property_query, get_subclass_query, get_instance_query, get_unit_query
from json_templates import get_json_object, get_json_object_with_enum

OWL_DATATYPE_PROPERTY = "http://www.w3.org/2002/07/owl#DatatypeProperty"
OWL_OBJECT_PROPERTY = "http://www.w3.org/2002/07/owl#ObjectProperty"

COMO_ENDPOINT = ""

data = {}

def get_ontological_properties(endpoint: str, ontological_class: str):
    """
    Retrieves all datatype properties and object properties directly
    connected to the ontological class.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: The IRI, label, position and type of properties modelled as
    classes.
    """
    
    print(get_property_query(ontological_class))
    results = perform_query(endpoint, get_property_query(ontological_class))
    return results

def get_ontological_subclasses(endpoint: str, ontological_class: str):
    """
    Retrieves all ubclasses directly connected to the ontological class.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: The IRI, label, position, type, range and range label of properties
      connected to the ontological class.
    """
    
    print(get_subclass_query(ontological_class))
    results = perform_query(endpoint, get_subclass_query(ontological_class))
    return results

def get_ontological_instances(endpoint: str, ontological_class: str):
    """
    Retrieves all instances directly connected to the ontological class.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: The IRI and name of instances directly connected to the
    ontological class.
    """
    
    print(get_instance_query(ontological_class))
    results = perform_query(endpoint, get_instance_query(ontological_class))
    return results

def get_ontologically_encoded_unit(endpoint: str, ontological_class: str):
    """
    Retrieves the unit associated with an ontological class
    representing a property.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: The IRI and symbol of a unit directly connected to the
    ontological class.
    """
    
    print(get_unit_query(ontological_class))
    results = perform_query(endpoint, get_unit_query(ontological_class))
    return results


def perform_query(endpoint: str, query: str):
    """
    Performs the query against the provided endpoint.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.

    Returns:
    str: The results returned by the triple store against the query.
    """
    sparql_wrapper = SPARQLWrapper(endpoint)
    sparql_wrapper.setHTTPAuth(BASIC)
    try:
        BLAZEGRAPH_USER = os.environ.get('BLAZEGRAPH_USER')
    except KeyError:
        print("Error: BLAZEGRAPH_USER environment variable is not set.")

    try:
        BLAZEGRAPH_PASSWORD = os.environ.get('BLAZEGRAPH_PASSWORD')
    except KeyError:
        print("Error: BLAZEGRAPH_PASSWORD environment variable is not set.")

    try:
        sparql_wrapper.setCredentials(BLAZEGRAPH_USER, BLAZEGRAPH_PASSWORD)
    except ValueError:
         print("Error: either BLAZEGRAPH_USER or BLAZEGRAPH_PASSWORD is not correct.")
    sparql_wrapper.setMethod(POST)
    sparql_wrapper.setQuery(query)
    sparql_wrapper.setReturnFormat(JSON)
    return sparql_wrapper.query().convert()

def format_property_values(results: str, previous_property_values: str = None):
    property_values = dict()
    if previous_property_values is not None:
         property_values = previous_property_values
    previous_range = ""
    previous_range_label = ""
    for result in results["results"]["bindings"]:
                # print(result['property']['value'], result['label']['value'], result['position']['value'], result['type']['value'], result['range']['value'], result['rangeLabel']['value'])
                label = result['label']['value']
                range = result['range']['value']
                range_label = result['rangeLabel']['value']
                if label.lower() in property_values.keys():
                    if previous_range_label == 'int':
                        del property_values[label.lower()]
                        property_values[label.lower()] = ""
                    if property_values[label.lower()] == "":
                        property_values[label.lower()] = range_label
                    else:
                        property_values[label.lower()] = property_values[label.lower()] + ", " + range_label
                else:
                      property_values[label.lower()] = range_label
                previous_range = range
                previous_range_label = range_label
    return property_values

def format_instances(class_label:str, results: str, previous_property_values: str = None):
    instances = dict()
    for result in results["results"]["bindings"]:
                label = result['label']['value']
                if class_label.lower() in instances.keys():
                    if instances[class_label.lower()] == "":
                        instances[class_label.lower()] = label
                    else:
                        instances[class_label.lower()] = instances[class_label.lower()] + ", " + label
                else:
                      instances[class_label.lower()] = label
    return instances

def format_enumerated_list(enumerated_list):
    string_array = enumerated_list.split(", ")
    sorted_list = sorted(string_array)
    sorted_string = ", ".join(['"{}"'.format(s) for s in sorted_list])
    return sorted_string

def is_property_result_empty(results):
    if results["results"]["bindings"] == None or results["results"]["bindings"] == "" or len(results["results"]["bindings"]) == 0:
        return True
    else:
         return False
    # for result in results["results"]["bindings"]:
    #     property = result['property']['value']
    #     if property == "":
    #          return True
    #     else:
    #         return False

def is_unit_result_empty(results):
    for result in results["results"]["bindings"]:
        unit = result['unit']['value']
        if unit == "":
             return True
        else:
            return False


def traverse_through_object_property(results, property_value_processed, property_values_formated, json_string, parsed_classes):
    for result in results["results"]["bindings"]:
                # print(result['property']['value'], result['label']['value'], result['position']['value'], result['type']['value'], result['range']['value'], result['rangeLabel']['value'])
                property = result['property']['value']
                label = result['label']['value']
                position = ""
                if 'position' in result:
                    position = result['position']['value']
                type = ""
                if 'type' in result:
                    type = result['type']['value']
                range = ""
                if 'range' in result:
                    range = result['range']['value']
                range_label = ""
                if 'range_label' in result:
                    range_label = result['rangeLabel']['value']
                if label.lower() == "properties":
                     continue
                if label.lower() not in property_value_processed:
                    if "," in property_values_formated[label.lower()]:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", "string" if range_label=="string" else "string", format_enumerated_list(property_values_formated[label.lower()]))))
                    else:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object(label, "TODO", "string" if property_values_formated[label.lower()]=="string" else "string")))
                property_value_processed.append(label.lower())
                if type == OWL_OBJECT_PROPERTY:
                    if "<"+range+">" not in parsed_classes:
                        print ("COMO_ENDPOINT:", COMO_ENDPOINT)
                        results = get_ontological_properties(COMO_ENDPOINT, "<"+range+">")
                        print(len(results))
                        if is_property_result_empty(results):
                            results = get_ontological_subclasses(COMO_ENDPOINT, "<"+range+">")
                            if is_property_result_empty(results):
                                results = get_ontological_instances(COMO_ENDPOINT, "<"+range+">")
                                if is_property_result_empty(results):
                                   results = get_ontologically_encoded_unit(COMO_ENDPOINT, "<"+range+">") 
                                   if is_unit_result_empty(results):
                                        continue
                                else:
                                    property_values_formated = format_instances(label, results, property_values_formated)
                                    parsed_classes.append("<"+range+">")
                                    json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", "string" if range_label=="string" else "string", format_enumerated_list(property_values_formated[label.lower()]))))
                                    continue
                            else:
                                property_values_formated = format_property_values(results, property_values_formated)
                                parsed_classes.append("<"+range+">")
                                traverse_through_object_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
                        else:
                            property_values_formated = format_property_values(results, property_values_formated)
                            parsed_classes.append("<"+range+">")
                            traverse_through_object_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)

    return json_string

def generate_json_string():
    try:
         global COMO_ENDPOINT
         COMO_ENDPOINT = os.environ.get('COMO_ENDPOINT')
    except KeyError:
        print("Error: COMO_ENDPOINT environment variable is not set.")
    product = "<http://www.theworldavatar.com/kg/ontomatpassport#Product>"
    component = "<http://www.theworldavatar.com/kg/ontomatpassport#Component>"
    root_classes = []
    root_classes.append(product)
    root_classes.append(component)
    results = get_ontological_properties(COMO_ENDPOINT, root_classes[0])
    property_values_formated = format_property_values(results)
    # formats enum values

    property_value_processed = []
    json_string = []
    label = "Type"
    range_label = "string"
    enumerated_list_provided = "Product" + ", " + "Component"
    #

    json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", range_label, format_enumerated_list(enumerated_list_provided))))
    # 
    parsed_classes = root_classes
    json_string = traverse_through_object_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
    print(json_string)
    json_string = combined_json_dict = dict(json_string)
    print(json_string)
    return js.dumps(combined_json_dict)


if __name__== '__main__':

    print(generate_json_string().replace("[\"\\", "[").replace("\\\"","\"").replace("\"]","]"))
    # original_string = "1, 3, 2"
    # print(format_enumerated_list(original_string))

    # print(js.dumps(json_string))
    # Retrieves all datatype properties and object properties directly
    # connected to the ontological class. It is done by following
    # the algorithm provided below:
    # 1. Retrieve all directly connected datatype properties and object
    #    properties of the ontological class. If no property is available, 
    #    it returns an empty value.
    # 2. Traverse through the object properties and retrieve all datatype
    #    properties and object properties of the classes connected through
    #    the range relationship.
    # 3. 
