"""
# Author: qhouyee, picas9dan #

This module provides the root tile and its bounding boxes for all tilesets.
"""

# Standard library imports
from pathlib import Path

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.schema import Tileset
from agent.ifc2tileset.tile_helper import make_tileset, make_root_tile, compute_bbox

# Retrieve logger
logger = agentlogging.get_logger("dev")


def append_tileset_schema_and_metadata(tileset: Tileset, building_iri: str):
    """Appends tileset schema and metadata to tileset.

    Args:
        tileset: A tileset.
        building_iri: The data IRI of the building.
    """
    tileset["schema"] = {"classes": {
        "TilesetMetaData": {
            "name": "Tileset metadata",
            "description": "A metadata class for the tileset",
            "properties": {
                "buildingIri": {
                    "description": "Data IRI of the building",
                    "type": "STRING"
                }
            }
        }
    }}

    tileset["metadata"] = {
        "class": "TilesetMetaData",
        "properties": {
            "buildingIri": building_iri
        }
    }


def gen_root_content(building_iri: str, asset_data: pd.DataFrame):
    """Generates a tileset with building and furniture data.

    If there are no assets, the tileset generated in this function is sufficient for visualisation.
    If there are no building, furniture and assets, returns None.

    Arguments:
        building_iri: The data IRI of the building.
        asset_data: A dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
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

    # When there is a furniture file generated, append it to the list
    if fpath.is_file():
        root_content_list.append(state.asset_url + "furniture.glb")
        compute_bbox_list.append(fpath)

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
            bbox=bbox, geometry_file_paths=root_content_list)

    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, building_iri)

    return tileset
