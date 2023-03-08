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
from query_templates import get_property_query
from json_templates import get_json_object, get_json_object_with_enum

OWL_DATATYPE_PROPERTY = "http://www.w3.org/2002/07/owl#DatatypeProperty"
OWL_OBJECT_PROPERTY = "http://www.w3.org/2002/07/owl#ObjectProperty"

data = {}

def get_ontological_properties(endpoint: str, ontological_class: str):
    """
    Retrieves all datatype properties and object properties directly
    connected to the ontological class.

    Parameters:
    endpoint (str): The endpoint of a triple store expressed as a URL.
    ontological_class (str): The IRI of the ontological class.

    Returns:
    str: The IRI, label, position, type, range and range label of properties
      connected to the ontological class.
    """
    
    print(get_property_query(ontological_class))
    results = perform_query(endpoint, get_property_query(ontological_class))
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
    sparql_wrapper.setCredentials(os.environ.get('BLAZEGRAPH_USER'), os.environ.get('BLAZEGRAPH_PASSWORD'))
    sparql_wrapper.setMethod(POST)
    sparql_wrapper.setQuery(query)
    sparql_wrapper.setReturnFormat(JSON)
    return sparql_wrapper.query().convert()

def format_property_values(results: str):
    property_values = dict()
    previous_range = ""
    previous_range_label = ""
    for result in results["results"]["bindings"]:
                # print(result['property']['value'], result['label']['value'], result['position']['value'], result['type']['value'], result['range']['value'], result['rangeLabel']['value'])
                label = result['label']['value']
                range = result['range']['value']
                range_label = result['rangeLabel']['value']
                if label.lower() in property_values.keys():
                    if previous_range_label in previous_range.split("#")[-1]:
                        del property_values[label.lower()]
                        property_values[label.lower()] = ""
                    if property_values[label.lower()] == "":
                        property_values[label.lower()] = range_label
                    else:
                        property_values[label.lower()] = property_values[label.lower()] + "," + range_label
                else:
                      property_values[label.lower()] = range_label
                previous_range = range
                previous_range_label = range_label
    return property_values

if __name__== '__main__':
    sparql_endpoint = os.environ.get('COMO_ENDPOINT')
    product = "<http://www.theworldavatar.com/kg/ontomatpassport#Product>"
    component = "<http://www.theworldavatar.com/kg/ontomatpassport#Component>"
    form_types = []
    form_types.append(product)
    form_types.append(component)
    results = get_ontological_properties(sparql_endpoint, form_types[0])
    property_values = format_property_values(results)
    for key, value in property_values.items():
        print(key, ':', value)      

    property_value_processed = []
    json_string = []
    for result in results["results"]["bindings"]:
                # print(result['property']['value'], result['label']['value'], result['position']['value'], result['type']['value'], result['range']['value'], result['rangeLabel']['value'])
                property = result['property']['value']
                label = result['label']['value']
                position = result['position']['value']
                type = result['type']['value']
                range = result['range']['value']
                range_label = result['rangeLabel']['value']
                if label.lower() not in property_value_processed:
                    if "," in property_values[label.lower()]:
                        json_string.append((label.lower().replace(" ", "_"), get_json_object_with_enum(label, "TODO", range_label, property_values[label.lower()])))
                        # json_string.append(",")
                        # print(get_json_object_with_enum(label.lower().replace(" ", "_"), label, "TODO", range_label, property_values[label.lower()]))
                    else:
                        # print(get_json_object(label.lower().replace(" ", "_"), label, "TODO", property_values[label.lower()]))
                        json_string.append((label.lower().replace(" ", "_"), get_json_object(label, "TODO", property_values[label.lower()])))
                        # json_string.append(",")
                property_value_processed.append(label.lower())

    combined_json_dict = dict(json_string)
    combined_json_string = js.dumps(combined_json_dict)

    print(combined_json_string)
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
