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


def init_asset_tiles(tileset: Tileset):
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

    tileset["root"]["children"] = [{
        "boundingVolume": {"box": properties.bbox_child},
        "geometricError": 50,
        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
        "contents": [{}]
        # CesiumJS only supports six content by default:
        # https://github.com/CesiumGS/cesium/issues/10468
        # If we have more than six assets, do add more inner children contents here
    }]
    return tileset


def appenddict_rec(tileset_root: Tile, asset_list: List[Content], i: int = 6):
    """
    Recursively adds every i assets to a new child node of the required format.
    Function will only run if there are more than 6 assets.

    Arguments:
        tileset_root - a nested dictionary starting from tileset['root']
        assetlist - a list of nested dictionary to add duplicate keys
        i - an integer denoting number of assets to add per child.
            Set to 6 as default as that is the current limit

    CAVEAT: Functionality for visualising tilesets with >10 child nodes
    in Cesium has yet to be tested. If the code fails to work for more child nodes,
    DO NOT change this code.
    Edit the asset2tileset function instead to stop when above the max limit of child nodes
    """
    if len(asset_list) > i:
        tileset_root["children"][0]["children"] = [{
            "boundingVolume": {"box": properties.bbox_child},
            "geometricError": 50,
            "contents": asset_list[i:i + 6]
        }]
        appenddict_rec(tileset_root["children"][0], asset_list, i + 6)
    else:
        return


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
    tileset = init_asset_tiles(tileset)

    # A Python list is required to handle duplicate keys in a nested dictionary
    asset_list: List[Content] = []
    # Add geometry and uid for each asset
    for row in range(len(asset_df.index)):
        asset_list.append({
            "uri": state.asset_url + asset_df['file'].iloc[row] + ".gltf",
            # Add the asset name to establish a metadata skeleton
            "metadata": {
                "class": "AssetMetaData",
                "properties": {
                    NAME_VAR: asset_df[NAME_VAR].iloc[row].split(":")[0],
                    ID_VAR: asset_df[ID_VAR].iloc[row],
                    IRI_VAR: asset_df[IRI_VAR].iloc[row]
                }
            }
        })

    # Add the first 6 assets to the first children node of the tileset
    tileset["root"]["children"][0]["contents"] = asset_list[0:6]

    # Add the remaining assets to the next nested child node of tileset
    appenddict_rec(tileset["root"], asset_list)
