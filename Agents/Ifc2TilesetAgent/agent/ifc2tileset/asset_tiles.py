"""
# Author: qhouyee #

This module provides methods to generate tilesets related to assets.
"""
from typing import List

# Third party imports
import pandas as pd

# Self imports
import agent.app as state
import agent.config.config as properties
from agent.ifc2tileset.schema import Tileset, Tile, Content
from agent.kgutils.const import NAME_VAR, ID_VAR, IRI_VAR


def append_asset_metadata_schema(tileset: Tileset):
    """
    Initialise the tileset to receive asset information

    Returns:
    The tileset generated with initialised asset metadata as a python dictionary
    """
    # Add new contents to the tileset
    tileset["schema"]["classes"]["AssetMetaData"] = {
        "name": "Asset metadata",
        "description": "A metadata class for all individual assets",
        # Store all asset information here even if they are not used for specific assets
        "properties": {
            NAME_VAR: {
                "description": "Name of the asset",
                "type": "STRING"
            },
            ID_VAR: {
                "description": "Unique identifier generated in IFC",
                "type": "STRING"
            },
            IRI_VAR: {
                "description": "Data IRI of the asset",
                "type": "STRING"
            }
        }
    }


def append_assets_rec(tile: Tile, asset_list: List[Content], i: int = 6):
    """
    Recursively adds every i assets to a new child node of the required format.
    Function will only run if there are more than 6 assets.

    Arguments:
        tile - a nested dictionary starting from tileset['root']
        asset_list - a list of nested dictionary to add duplicate keys
        i - an integer denoting number of assets to add per child.
            Set to 6 as default as that is the current limit

    CAVEAT: Functionality for visualising tilesets with >10 child nodes
    in Cesium has yet to be tested. If the code fails to work for more child nodes,
    DO NOT change this code.
    Edit the asset2tileset function instead to stop when above the max limit of child nodes
    """
    if len(asset_list) > i:
        tile["children"] = [{
            "boundingVolume": {"box": properties.bbox_child},
            "geometricError": 50,
            # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
            "contents": asset_list[i:i + 6]
            # CesiumJS only supports six content by default:
            # https://github.com/CesiumGS/cesium/issues/10468
            # If we have more than six assets, do add more inner children contents here
        }]
        append_assets_rec(tile["children"][0], asset_list, i + 6)


def gen_tileset_assets(asset_df: pd.DataFrame, tileset: Tileset):
    """
    Add asset properties into the tileset.

    Arguments:
        asset_df - dataframe containing mappings for asset metadata
        tileset - tileset for adding asset metadata
    Returns:
    The tileset generated with asset metadata as a python dictionary
    """
    # Initialise tileset structure for assets
    append_asset_metadata_schema(tileset)

    # A Python list is required to handle duplicate keys in a nested dictionary
    asset_list: List[Content] = asset_df.apply(
        # Add geometry and uid for each asset
        lambda row: {
            "uri": state.asset_url + row["file"] + ".gltf",
            # Add the asset name to establish a metadata skeleton
            "metadata": {
                "class": "AssetMetaData",
                "properties": {
                    NAME_VAR: row[NAME_VAR].split(":")[0],
                    ID_VAR: row[ID_VAR],
                    IRI_VAR: row[IRI_VAR]
                }
            }
        }, axis=1
    ).tolist()

    # Add 6 assets to every nested child node of tileset
    append_assets_rec(tileset["root"], asset_list, 0)
