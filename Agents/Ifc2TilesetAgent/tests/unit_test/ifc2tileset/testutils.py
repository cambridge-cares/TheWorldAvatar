"""
# Author: qhouyee, picas9dan #

This file contains util functions for ifc2tileset unit tests.
"""

# Standard library imports
import json
from typing import List, Optional

# Third-party imports
import pandas as pd

# Self imports
from agent.ifc2tileset.tile_helper import make_root_tile, make_tileset


def z_up_to_y_up(x_min: float, y_min: float, z_min: float, x_max: float, y_max: float, z_max: float):
    return x_min, z_min, -y_max, x_max, z_max, -y_min


def read_json(json_filepath: str):
    with open(json_filepath, "r", encoding="utf-8") as f:
        data = json.load(f)
    return data


def gen_sample_tileset(
    bbox: List[float] = [40, 0, 15, 100, 0, 0, 0, 100, 0, 0, 0, 5]
):
    root_tile = make_root_tile(bbox)
    tileset = make_tileset(root_tile)
    return tileset


def gen_content_metadata(building_iri: str, building_name: Optional[str] = ""):
    meta_dict = {
        "class": "ContentMetaData",
        "properties": {
            "iri": building_iri
        }
    }
    if building_name:
            meta_dict["properties"]["name"] = building_name
    return meta_dict


def append_tileset_contents(expected_tileset: dict, geometry_file_paths: List[str], building_iri: Optional[str] = "", building_name: Optional[str] = ""):
    """Append the contents to the tilesets according to inputs for assertion.

    Args:
        expected_tileset: Tileset to attach these inputs.
        geometry_file_paths: A list of file_paths for the geometry generated.
        building_iri: IRI of the building. Optional.
        building_name: Name of the building. Optional.
    """
    # Generate repeated metadata if it exists
    if building_iri and building_name:
        content_metadata = gen_content_metadata(building_iri, building_name)

    # When there is only one geometry, it should have use content
    if len(geometry_file_paths) == 1:
        content = {"uri": geometry_file_paths[0]}
        if building_iri and building_name:
            content["metadata"] = content_metadata
        expected_tileset["root"]["content"] = content
    # otherwise, use contents
    else:
        content = []
        for path in geometry_file_paths:
            temp_dict = {"uri": path}
            if building_iri and building_name:
                temp_dict["metadata"] = content_metadata
            content.append(temp_dict)
        expected_tileset["root"]["contents"] = content


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
                "class": "ContentMetaData",
                "properties": {
                    "name": "element" + str(i),
                    "iri": "iri" + str(i)
                }
            }
        } for i in range(test_range)
    ]
