"""
# Author: qhouyee, picas9dan #

This module provides the root tile and its bounding boxes for all tilesets.
"""

# Standard library imports
from pathlib import Path
from typing import List

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.tile_helper import make_tileset, make_root_tile, compute_bbox

# Retrieve logger
logger = agentlogging.get_logger("dev")


def gen_root_content(asset_data: pd.DataFrame, building_data: list[str], root_content_parameters: List[str]):
    """Generates a tileset with building and furniture data. 

    If there are no assets, the tileset generated in this function is sufficient for visualisation.
    If there are no building, furniture and assets, returns None.
    If there is no building data, users are able to pass in root content IRI and name as parameters.

    Arguments:
        asset_data: A dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
        building_data: A list containing the data IRI and name of the building in this order.
        root_content_parameters: A list containing the IRI and name to be appended to root content in this order.

    Returns:
        A tileset with building and furniture data if either building or assets are present, otherwise None.
    """
    # Respective filepaths
    building_file_path = "./data/glb/building.glb"
    bpath = Path(building_file_path)
    furniture_file_path = "./data/glb/furniture.glb"
    fpath = Path(furniture_file_path)

    # Generate an empty list to append contents when necessary
    compute_bbox_list = []
    root_content_list = []

    # When there is a building file generated, append it to the list
    if bpath.is_file():
        root_content_list.append(state.asset_url + "building.glb")
        compute_bbox_list.append(bpath)
        # Verify if there are building_data in the knowledge graph
        if len(building_data) == 0:
            # If there are no results returned, stop the agent task
            logger.fatal(
                "Detected building geometry but the corresponding iri and name is not available in the knowledge graph!")
            raise RuntimeError(
                "Detected building geometry but the corresponding iri and name is not available in the knowledge graph!")
    else:
        # When no building file is available, reset building_data to ensure no data is populated in the tileset
        building_data = []

    # When there is a furniture file generated, append it to the list
    if fpath.is_file():
        root_content_list.append(state.asset_url + "furniture.glb")
        compute_bbox_list.append(fpath)
        # If there is no building path but there is furniture content and metadata sent through the request
        if not bpath.is_file() and len(root_content_parameters)!=0:
            # override building data so that this is appended
            building_data = root_content_parameters

    # When neither the building and furniture is generated
    if not root_content_list:
        # And if there is also no assets, ensure that no bim tileset is generated
        if asset_data.empty:
            return None
        # Otherwise, generate the root tile with a bounding box enclosing all the assets
        else:
            bbox = compute_bbox(
                [f"./data/glb/{file}.glb" for file in asset_data["file"]])
            root_tile = make_root_tile(bbox=bbox)
    # When there is either a building and/or furniture generated,
    # compute their bounding boxes and generate a root tile accordingly
    else:
        bbox = compute_bbox(compute_bbox_list)
        root_tile = make_root_tile(
            bbox=bbox, geometry_file_paths=root_content_list, root_metadata=building_data)

    return make_tileset(root_tile)