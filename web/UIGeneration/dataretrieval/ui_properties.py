###############################################
# Authors: Feroz Farazi (msff2@cam.ac.uk)     #    
# Date: 27 Feb 2023                           #
###############################################

# The purpose of this module is to provide functions to retrieve object and
# datatype properties directly and indirectly associated with a class from
# the KG by traversing through the range relationship of the object properties
# Retrieves all datatype properties and object properties directly
# connected to the ontological class. It is done by following
# the algorithm provided below:
# 1. Retrieve all directly connected datatype properties and object
#    properties of the ontological class. If no property is available, 
#    it returns an empty value.
# 2. Traverse through the object properties and retrieve all datatype
#    properties and object properties of the classes connected through
#    the range relationship.

from SPARQLWrapper import SPARQLWrapper, JSON, POST, BASIC
import requests, json as js
import os
from query_templates import get_property_query, get_subclass_query, get_instance_query, get_unit_query
from json_templates import get_json_object, get_json_object_with_enum, get_json_object_with_properties, get_json_object_with_unit

OWL_DATATYPE_PROPERTY = "http://www.w3.org/2002/07/owl#DatatypeProperty"
OWL_OBJECT_PROPERTY = "http://www.w3.org/2002/07/owl#ObjectProperty"
OWL_CLASS = "http://www.w3.org/2002/07/owl#Class"

JIDEP_ENDPOINT = ""

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
    """
    When multiple concepts are available in the range of an object property or
    multiple values are available in the range of a data property, this
    function combines them in a comma-separated string to make it suitable for
    representing it as an enumerated list.
    """
    property_values = dict()
    if previous_property_values is not None:
         property_values = previous_property_values
    previous_range_label = ""
    for result in results["results"]["bindings"]:
        label = result['label']['value']
        range_label = result['rangeLabel']['value']
        if label.lower().replace(" ", "_") in property_values.keys():
            if previous_range_label == 'int':
                del property_values[label.lower().replace(" ", "_")]
                property_values[label.lower().replace(" ", "_")] = ""
            if property_values[label.lower().replace(" ", "_")] == "":
                property_values[label.lower().replace(" ", "_")] = range_label
            else:
                property_values[label.lower().replace(" ", "_")] = property_values[label.lower().replace(" ", "_")] + ", " + range_label
        else:
            property_values[label.lower().replace(" ", "_")] = range_label
        previous_range_label = range_label
    return property_values

def format_subclasses(results: str, previous_property_values: str = None):
    """
    When multiple concepts are available in the range of an object property or
    multiple values are available in the range of a data property, this
    function combines them in a comma-separated string to make it suitable for
    representing it as an enumerated list.
    """
    property_values = dict()
    if previous_property_values is not None:
         property_values = previous_property_values
    for result in results["results"]["bindings"]:
        label = result['label']['value']
        property_values[label.lower().replace(" ", "_")] = label
    return property_values


def format_instances(class_label:str, results: str, previous_property_values: str = None):
    instances = dict()
    for result in results["results"]["bindings"]:
                label = result['label']['value']
                if class_label.lower().replace(" ", "_") in instances.keys():
                    if instances[class_label.lower().replace(" ", "_")] == "":
                        instances[class_label.lower().replace(" ", "_")] = label
                    else:
                        instances[class_label.lower().replace(" ", "_")] = instances[class_label.lower().replace(" ", "_")] + ", " + label
                else:
                      instances[class_label.lower().replace(" ", "_")] = label
    return instances

def format_unit(class_label:str, results: str):
    instances = dict()
    for result in results["results"]["bindings"]:
                symbol = result['label']['value']
                if class_label.lower().replace(" ", "_") in instances.keys():
                    if instances[class_label.lower().replace(" ", "_")] == "":
                        instances[class_label.lower().replace(" ", "_")] = symbol
                    else:
                        instances[class_label.lower().replace(" ", "_")] = instances[class_label.lower().replace(" ", "_")] + ", " + symbol
                else:
                      instances[class_label.lower().replace(" ", "_")] = symbol
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

def is_unit_result_empty(results):
    for result in results["results"]["bindings"]:
        unit = result['property']['value']
        if unit == "":
             return True
        else:
            return False


def traverse_through_identification_property(results, property_value_processed, property_values_formated, json_string, parsed_classes, class_of_properties: str = None, class_of_instances: str = None):
    if class_of_instances != None:
        popped_item = parsed_classes.pop()
        popped_parent_item = parsed_classes.pop()
        processed_parent_item = popped_parent_item.split("#")[1].split(">")[0] if "#" in popped_parent_item else popped_parent_item.split("/")[1].split(">")[0]
        dict(json_string)[processed_parent_item.lower().replace(" ", "_")]['properties'][class_of_instances.lower().replace(" ", "_")]['enum'] = [property_values_formated]
        parsed_classes.append(popped_parent_item)
        parsed_classes.append(popped_item)
        return

    for result in results["results"]["bindings"]:
                label = result['label']['value']
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
                    if "," in property_values_formated[label.lower().replace(" ", "_")]:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", "string" if range_label=="string" else "string", format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]))))
                    elif class_of_properties != None:
                        dict(json_string)[class_of_properties.lower()]['properties'][label.lower().replace(" ", "_")] = get_json_object(label, "TODO", "string" if range_label=="string" else "string")
                    elif type == OWL_OBJECT_PROPERTY:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_properties(label, "TODO", "object" if property_values_formated[label.lower().replace(" ", "_")]=="object" else "object"))) 
                    else:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object(label, "TODO", "string" if property_values_formated[label.lower().replace(" ", "_")]=="string" else "string")))
                property_value_processed.append(label.lower().replace(" ", "_"))
                if type == OWL_OBJECT_PROPERTY:
                    if "<"+range+">" not in parsed_classes:
                        results = get_ontological_properties(JIDEP_ENDPOINT, "<"+range+">")
                        if is_property_result_empty(results):
                            results = get_ontological_subclasses(JIDEP_ENDPOINT, "<"+range+">")
                            if is_property_result_empty(results):
                                results = get_ontological_instances(JIDEP_ENDPOINT, "<"+range+">")
                                if is_property_result_empty(results):
                                   results = get_ontologically_encoded_unit(JIDEP_ENDPOINT, "<"+range+">") 
                                   if is_unit_result_empty(results):
                                        continue
                                else:
                                    property_values_formated = format_instances(label, results, property_values_formated)
                                    parsed_classes.append("<"+range+">")
                                    traverse_through_identification_property(results, property_value_processed, format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]), json_string, parsed_classes, None, label)
                            else:
                                property_values_formated = format_property_values(results, property_values_formated)
                                parsed_classes.append("<"+range+">")
                                traverse_through_identification_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
                        else:
                            property_values_formated = format_property_values(results, property_values_formated)
                            parsed_classes.append("<"+range+">")
                            traverse_through_identification_property(results, property_value_processed, property_values_formated, json_string, parsed_classes, label)
    return json_string

def traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes, class_of_properties: str = None, class_of_instances: str = None, class_containing_unit: str = None):
    if class_of_instances != None:
        popped_item = parsed_classes.pop()
        popped_parent_item = parsed_classes.pop()
        processed_parent_item = popped_parent_item.split("#")[1].split(">")[0] if "#" in popped_parent_item else popped_parent_item.split("/")[1].split(">")[0]
        dict(json_string)[processed_parent_item.lower().replace(" ", "_")]['properties'][class_of_instances.lower().replace(" ", "_")]['enum'] = [property_values_formated]
        parsed_classes.append(popped_parent_item)
        parsed_classes.append(popped_item)
        return

    if class_containing_unit != None:
        popped_parent_item = parsed_classes.pop()
        processed_parent_item = popped_parent_item.split("#")[1].split(">")[0] if "#" in popped_parent_item else popped_parent_item.split("/")[1].split(">")[0]
        dict(json_string)[processed_parent_item.lower().replace(" ", "_")]['properties'][class_containing_unit.lower().replace(" ", "_")]['enum'] = [property_values_formated]
        parsed_classes.append(popped_parent_item)
        return


    for result in results["results"]["bindings"]:
                property = ""
                if 'property' in result:
                    property = result['property']['value']
                label = result['label']['value']
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
                    if "," in property_values_formated[label.lower().replace(" ", "_")]:
                        if type == OWL_CLASS:
                            json_string.append((label.lower().replace(" ", "_"), get_json_object_with_unit(label, "TODO", format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")])))) 
                        else:
                            json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", "string" if range_label=="string" else "string", format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]))))
                    elif class_of_properties != None:
                        dict(json_string)[class_of_properties.lower()]['properties'][label.lower().replace(" ", "_")] = get_json_object(label, "TODO", "string" if range_label=="string" else "string")
                    elif type == OWL_OBJECT_PROPERTY:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_properties(label, "TODO", "object" if property_values_formated[label.lower().replace(" ", "_")]=="object" else "object"))) 
                    elif type == OWL_CLASS:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_properties(label, "TODO", "object" if property_values_formated[label.lower().replace(" ", "_")]=="object" else "object"))) 
                    else:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object(label, "TODO", "string" if property_values_formated[label.lower().replace(" ", "_")]=="string" else "string")))
                property_value_processed.append(label.lower().replace(" ", "_"))
                if type == OWL_OBJECT_PROPERTY:
                    if "<"+range+">" not in parsed_classes:
                        results = get_ontological_properties(JIDEP_ENDPOINT, "<"+range+">")
                        if is_property_result_empty(results):
                            results = get_ontological_subclasses(JIDEP_ENDPOINT, "<"+range+">")
                            if is_property_result_empty(results):
                                results = get_ontological_instances(JIDEP_ENDPOINT, "<"+range+">")
                                if is_property_result_empty(results):
                                   results = get_ontologically_encoded_unit(JIDEP_ENDPOINT, "<"+range+">") 
                                   if is_unit_result_empty(results):
                                        continue
                                   else:
                                       property_values_formated = format_unit(label, results, property_values_formated)
                                       parsed_classes.append("<"+range+">")
                                       traverse_through_physical_property(results, property_value_processed, format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]), json_string, parsed_classes, None, None, label)
                                else:
                                    property_values_formated = format_instances(label, results, property_values_formated)
                                    parsed_classes.append("<"+range+">")
                                    traverse_through_physical_property(results, property_value_processed, format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]), json_string, parsed_classes, None, label)
                            else:
                                property_values_formated = format_property_values(results, property_values_formated)
                                parsed_classes.append("<"+range+">")
                                traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
                        else:
                            property_values_formated = format_property_values(results, property_values_formated)
                            parsed_classes.append("<"+range+">")
                            traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes, label)
                if type == OWL_CLASS:
                    if property not in parsed_classes:
                        results = get_ontological_properties(JIDEP_ENDPOINT, "<"+property+">")
                        if is_property_result_empty(results):
                            results = get_ontological_subclasses(JIDEP_ENDPOINT, "<"+property+">")
                            if is_property_result_empty(results):
                                results = get_ontological_instances(JIDEP_ENDPOINT, "<"+property+">")
                                if is_property_result_empty(results):
                                   results = get_ontologically_encoded_unit(JIDEP_ENDPOINT, "<"+property+">") 
                                   if is_unit_result_empty(results):
                                        continue
                                   else:
                                       parsed_classes.append("<"+property+">")
                                       dict(json_string)[label.lower().replace(" ", "_")]['properties'] = get_json_object_with_unit(label, "TODO", format_enumerated_list(format_unit(label, results)[label.lower().replace(" ", "_")]))
                                       continue
                                else:
                                    property_values_formated = format_instances(label, results, property_values_formated)
                                    parsed_classes.append("<"+property+">")
                                    traverse_through_physical_property(results, property_value_processed, format_enumerated_list(property_values_formated[label.lower().replace(" ", "_")]), json_string, parsed_classes, None, label)
                            else:
                                property_values_formated = format_subclasses(results, property_values_formated)
                                parsed_classes.append("<"+property+">")
                                traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
                        else:
                            property_values_formated = format_property_values(results, property_values_formated)
                            parsed_classes.append("<"+property+">")
                            traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes, label)

    return json_string


def generate_identification_properties():
    try:
         global JIDEP_ENDPOINT
         JIDEP_ENDPOINT = os.environ.get('JIDEP_ENDPOINT')
    except KeyError:
        print("Error: JIDEP_ENDPOINT environment variable is not set.")
    product = "<http://www.theworldavatar.com/kg/ontomatpassport#Product>"
    component = "<http://www.theworldavatar.com/kg/ontomatpassport#Component>"
    root_classes = []
    root_classes.append(product)
    root_classes.append(component)
    results = get_ontological_properties(JIDEP_ENDPOINT, root_classes[0])
    property_values_formated = format_property_values(results)
    property_value_processed = []
    json_string = []
    label = "Type"
    range_label = "string"
    enumerated_list_provided = "Product" + ", " + "Component"

    json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", range_label, format_enumerated_list(enumerated_list_provided))))

    parsed_classes = root_classes
    json_string = traverse_through_identification_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
    json_string = combined_json_dict = dict(json_string)
    return js.dumps(combined_json_dict).replace("[\"\\", "[").replace("\\\"","\"").replace("\"]","]")


def generate_physical_properties():
    try:
         global JIDEP_ENDPOINT
         JIDEP_ENDPOINT = os.environ.get('JIDEP_ENDPOINT')
    except KeyError:
        print("Error: COMO_ENDPOINT environment variable is not set.")
    physical_property = "<https://w3id.org/mdo/core/PhysicalProperty>"
    root_classes = []
    root_classes.append(physical_property)
    results = get_ontological_subclasses(JIDEP_ENDPOINT, root_classes[0])
    property_values_formated = format_subclasses(results)
    property_value_processed = []
    json_string = []
    # label = "Type"
    # range_label = "string"
    # enumerated_list_provided = "Product" + ", " + "Component"

    parsed_classes = root_classes
    json_string = traverse_through_physical_property(results, property_value_processed, property_values_formated, json_string, parsed_classes)
    json_string = combined_json_dict = dict(json_string)
    return js.dumps(combined_json_dict).replace("[\"\\", "[").replace("\\\"","\"").replace("\"]","]")


if __name__== '__main__':

    print(generate_physical_properties())
