import json
from typing import List

import pandas as pd

import agent.config.config as properties
from agent.ifc2tileset.root_tile import append_tileset_schema_and_metadata
from agent.ifc2tileset.tile_helper import make_root_tile, make_tileset


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


def gen_sample_tileset(bbox: List[float] = properties.bbox_root, building_iri: str = "buildingIri"):
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