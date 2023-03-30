import json
from typing import List

import pandas as pd

from agent.ifc2tileset.root_tile import append_tileset_schema_and_metadata
from agent.ifc2tileset.tile_helper import make_root_tile, make_tileset


def z_up_to_y_up(x_min: float, y_min: float, z_min: float, x_max: float, y_max: float, z_max: float):
    return x_min, z_min, -y_max, x_max, z_max, -y_min


def read_json(json_filepath: str):
    """
    A test function to read the contents of a tileset.json

    Argument:
    json_filepath - File path to the tileset.json
    Returns:
    The tileset's contents as a Python dictionary
    """
    # Read the results
    with open(json_filepath, "r", encoding="utf-8") as f:
        data = json.load(f)
    return data


def gen_sample_tileset(bbox: List[float] = [40, 0, 15, 100, 0, 0, 0, 100, 0, 0, 0, 5], building_iri: str = "buildingIri"):
    root_tile = make_root_tile(bbox)
    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, building_iri)
    return tileset


def gen_sample_asset_df(test_range: int):
    """
    A test function to generate sample datafarame for the
    test of gen_tileset_assets()

    Argument:
    test_range - number of assets to generated for testing
    Returns:
    The sample dataframe
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
    return [
        {
            "uri": f"./gltf/asset{i}.gltf",
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
