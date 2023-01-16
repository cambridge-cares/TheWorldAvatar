"""
# Author: qhouyee #

This module provides helper functions to generate glTF models from the IFC file.
"""

# Third party imports
from ifcopenshell.util.selector import Selector
from py4jps import agentlogging

# Self imports
from agent.utils import find_word

# Retrieve logger
logger = agentlogging.get_logger("dev")

def verify_feature_exists(featurelist, ifc):
    """
    Verifies if the IFC feature in a list exist in the IFC file

    Arguments:
        featurelist - A list of IfcClasses to verify in string. Accepted format: '.IfcFeatureType'
        ifc - The required IFC model loaded in ifcopenshell
    Returns:
    A list containing features that exist in the format 'IfcFeatureType'
    """
    # selector is a utility tool from  ifcopenshell
    selector = Selector()
    result_list = []
    for feature in featurelist:
        if len(selector.parse(ifc, feature)) > 0:
            result_list += [feature[1:]]
    return result_list


def gendict4split(ifc):
    """
    Creates a dictionary {filename : ifc_classes or ifc_id} to split
    the IFC model into smaller IFC files

    Arguments:
        ifc - The required IFC model loaded in ifcopenshell
    Returns:
        The required dictionary for splitting the IFC model
        A hashtable to match assets to their IFC ID
    """
    # Initialise a dictionary with the IFC classes to exclude from the building output model
    # Non-exhaustive. If required, add more classes in the format 'IfcFeatureType'
    dict_elements = {"building":
                     ["IfcBuildingElementProxy", "IfcFurnishingElement",
                      "IfcFlowTerminal", "IfcSpace", "IfcOpeningElement",
                      "IfcFlowSegment"]}

    hashmapping = {}
    counter = 1
    # Store the IDs that should be generated as part of the interior furniture or solar panel model
    furniture_elements = []
    solar_panel_elements = []
    sewage_network_elements = []
    for feature in ["IfcBuildingElementProxy", "IfcFurnishingElement", "IfcFlowTerminal"]:
        for element in ifc.by_type(feature):
            # If the name contains these key words,generate individual models for them
            wordlist = ["Sensor", "Weather Station", "Meter",
                "Fume Hood", "Explosive Precursor", "Chemistry Robot",
                "Fridge"]
            if find_word(wordlist, element.Name):
                # Simplify the name of asset files, as
                # Cesium cannot load complicated or long file names
                dict_elements["asset"+str(counter)] = element.GlobalId
                # Create a hashtable dict to store the mapping values
                hashmapping[element.GlobalId] = {
                    "file": "asset"+str(counter), "name": element.Name}
                counter = counter+1
            # If the name contain a solar panel, generate a separate solar panel model
            elif find_word(["Solar Panel"], element.Name):
                solar_panel_elements.append(element.GlobalId)
            elif find_word(["Manhole"], element.Name):
                sewage_network_elements.append(element.GlobalId)
            else:
                furniture_elements.append(element.GlobalId)
    for element in ifc.by_type("IfcFlowSegment"):
        sewage_network_elements.append(element.GlobalId)
    # Add to dictionary if list is not empty
    if furniture_elements:
        dict_elements["furniture"] = furniture_elements
    if solar_panel_elements:
        dict_elements["solarpanel"] = solar_panel_elements
    if sewage_network_elements:
        dict_elements["sewagenetwork"] = sewage_network_elements
    return dict_elements, hashmapping


def append_ifcconvert_command(key, value, ifcconvert_command):
    """
    Appends the dictionary values to the commands depending on
    their key and if they are either a list or single instance

    Arguments:
        key - The dictionary key indicating the model output name
        value - The dictionary values indicating either the IFC classes
                or ID for inclusion or exclusion
        ifcconvert_command - The initialised command in List format
    Returns:
    The command with the appended values
    """
    # Add the entities arg for only building key
    if key == "building":
        ifcconvert_command += ["--exclude", "entities"]
    # Other keys will only require an attribute arg
    else:
        ifcconvert_command += ["--include", "attribute", "GlobalId"]
    # Value may be either a list or a single object
    if isinstance(value, list):
        ifcconvert_command += value
    else:
        ifcconvert_command += [value]
    return ifcconvert_command
