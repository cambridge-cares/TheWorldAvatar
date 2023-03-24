"""
# Author: qhouyee #

This module provides helper functions to generate glTF models from the IFC file.
"""

# Third party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.kgutils.const import ID_VAR

# Retrieve logger
logger = agentlogging.get_logger("dev")


def append_aggregate(aggregates_df: pd.DataFrame, dictionary: dict):
    """
    Appends the IFC id of aggregate elements into the dictionary according to file name

    Arguments:
        aggregates_df - A dataframe of assets to be aggregated into furniture and others
        dictionary - A dictionary to append their IFC id
    """
    # Initialise list containing the right assets
    furniture_elements = []
    solar_panel_elements = []
    sewage_network_elements = []
    # Check for empty dataframe
    if not aggregates_df.empty:
        # Store asset in list based on which category they belong to
        for row in range(len(aggregates_df.index)):
            file_cell = aggregates_df['file'].iloc[row]
            if file_cell == "furniture":
                furniture_elements.append(aggregates_df[ID_VAR].iloc[row])
            elif file_cell == "solarpanel":
                solar_panel_elements.append(aggregates_df[ID_VAR].iloc[row])
            elif file_cell == "sewagenetwork":
                sewage_network_elements.append(aggregates_df[ID_VAR].iloc[row])
    # Add to dictionary if list is not empty
    if furniture_elements:
        dictionary["furniture"] = furniture_elements
    if solar_panel_elements:
        dictionary["solarpanel"] = solar_panel_elements
    if sewage_network_elements:
        dictionary["sewagenetwork"] = sewage_network_elements


def append_individual_asset(asset_df, dictionary):
    """
    Appends the IFC id of individual elements into the dictionary according to file name

    Arguments:
        asset_df - A dataframe of individual assets
        dictionary - A dictionary to append their IFC id
    """
    # Check for empty dataframe
    if not asset_df.empty:
        # Store asset in list based on which category they belong to
        for row in range(len(asset_df.index)):
            dictionary[asset_df['file'].iloc[row]] = asset_df[ID_VAR].iloc[row]


def gendict4split(dataframe: pd.DataFrame):
    """
    Creates a dictionary {filename : ifc_id} to split
    the IFC model into smaller IFC files

    Arguments:
        dataframe - A dataframe containing all assets' uid and file name
    Returns:
        The required dictionary for splitting the IFC model
        A dataframe containing individual assets' metadata
    """
    # Initialise a dictionary with the IFC classes to exclude from the building output model
    # Non-exhaustive. If required, add more classes in the format 'IfcFeatureType'
    dict_elements = {"building":
                         ["IfcBuildingElementProxy", "IfcFurnishingElement",
                          "IfcFlowTerminal", "IfcSpace", "IfcOpeningElement",
                          "IfcFlowSegment"]}

    if not dataframe.empty:
        # Initialise a list of file names for aggregated assets
        aggregate_list = ["solarpanel", "sewagenetwork", "furniture"]
        logger.debug("Appending aggregate assets to dictionary...")
        append_aggregate(dataframe.loc[dataframe['file'].isin(aggregate_list)], dict_elements)

        logger.debug("Appending individual assets to dictionary...")
        asset_data = dataframe.loc[~dataframe['file'].isin(aggregate_list)]
        append_individual_asset(asset_data, dict_elements)
    else:
        asset_data = pd.DataFrame(columns=["iri", "uid", "name", "file"])
    return dict_elements, asset_data


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
