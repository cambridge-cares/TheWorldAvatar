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
    If there are no building and assets, returns None.

    Arguments:
        building_iri: The data IRI of the building.
        asset_data: A dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
    Returns:
        A tileset with building and furniture data if either building or assets are present, otherwise None.
    """
    # Respective filepaths
    building_file_path = "./data/glb/building.glb"
    bpath = Path(building_file_path)

    if bpath.is_file():
        building_content = {"uri": state.asset_url + "building.glb"}

        furniture_file_path = "./data/glb/furniture.glb"
        fpath = Path(furniture_file_path)

        # If there are furniture, use the multiple nomenclature
        if fpath.is_file():
            bbox = compute_bbox([bpath, fpath])
            furniture_content = {"uri": state.asset_url + "furniture.glb"}

            # Tileset Nomenclature for multiple geometry files = contents:[{}]
            root_tile = make_root_tile(bbox=bbox, contents=[furniture_content, building_content])
        else:
            bbox = compute_bbox(bpath)

            # Tileset Nomenclature for 1 geometry file = content:{}
            root_tile = make_root_tile(bbox=bbox, content=building_content)
    else:
        if asset_data.empty:
            # No bim tileset should be generated if there is no builidng and no assets
            return None

        # In the scenario where there is no building, the root bbox should enclose all assets
        bbox = compute_bbox([f"./data/glb/{file}.glb" for file in asset_data["file"]])
        root_tile = make_root_tile(bbox=bbox)

    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, building_iri)

    return tileset
