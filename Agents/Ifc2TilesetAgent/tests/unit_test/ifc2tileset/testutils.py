"""
# Author: qhouyee, picas9dan #

This file contains util functions for ifc2tileset unit tests.
"""

# Standard library imports
import json
from typing import List

# Third-party imports
import pandas as pd

# Self imports
from agent.ifc2tileset.root_tile import append_tileset_schema_and_metadata
from agent.ifc2tileset.tile_helper import make_root_tile, make_tileset


def z_up_to_y_up(x_min: float, y_min: float, z_min: float, x_max: float, y_max: float, z_max: float):
    return x_min, z_min, -y_max, x_max, z_max, -y_min


def read_json(json_filepath: str):
    with open(json_filepath, "r", encoding="utf-8") as f:
        data = json.load(f)
    return data


def gen_sample_tileset(
    bbox: List[float] = [40, 0, 15, 100, 0, 0, 0, 100, 0, 0, 0, 5], 
    building_iri: str = "buildingIri"
):
    root_tile = make_root_tile(bbox)
    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, building_iri)
    return tileset


def gen_sample_asset_df(test_range: int):
    """Generates sample datafarame for asset metadata.

    Args:
        test_range: Number of assets to be generated.

    Returns:
        A dataframe with sample data.
    """
    rows = [
        {
            "uid": "uid" + str(i),
            "name": "element" + str(i),
            "iri": "iri" + str(i),
            "file": "asset" + str(i)
        } for i in range(test_range)
    ]
    return pd.DataFrame(rows)


def gen_sample_asset_contents(test_range: int):
    """Generates `contents` field of a tile populated with sample asset metadata.
    
    Args:
        test_range: Number of assets to be generated.
    
    Returns:
        A list of dict.
    """
    return [
        {
            "uri": f"./glb/asset{i}.glb",
            "metadata": {
                "class": "AssetMetaData",
                "properties": {
                    "name": "element" + str(i),
                    "uid": "uid" + str(i),
                    "iri": "iri" + str(i)
                }
            }
        } for i in range(test_range)
    ]
