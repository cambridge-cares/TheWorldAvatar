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


def append_assets_to_tile_node(tile: Tile, asset_df: pd.DataFrame):
    """
    Appends a child node containing the given assets to the given tile

    Arguments:
        asset_df - dataframe containing mappings for asset metadata
        tile - parent node to have assets added as a child node
    """
    assets: List[Content] = asset_df.apply(
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
    bbox = properties.bbox_child

    tile["children"] = [{
        "boundingVolume": {"box": bbox},
        "geometricError": 50,
        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
        "contents": assets
        # CesiumJS only supports six content by default:
        # https://github.com/CesiumGS/cesium/issues/10468
        # If we have more than six assets, do add more inner children contents here
    }]


def append_assets(tileset: Tileset, asset_df: pd.DataFrame):
    """
    Adds 6 assets to every nested child node, starting from the root tile.

    Arguments:
        asset_df - dataframe containing mappings for asset metadata
        tile - parent node to have assets added as a child node
    """
    assets_num_per_node = 6

    tile_node = tileset["root"]
    for i in range(0, len(asset_df), assets_num_per_node):
        append_assets_to_tile_node(tile_node, asset_df.iloc[i: i + assets_num_per_node])
        tile_node = tile_node["children"][0]

        # CAVEAT: Functionality for visualising tilesets with >10 child nodes in Cesium has yet to be tested.
        # If the code fails to work for more child nodes, perform early stopping when above the max limit of child nodes
        # if i >= 10:
        #     break


def gen_tileset_assets(tileset: Tileset, asset_df: pd.DataFrame):
    """
    Add asset properties into the tileset.

    Arguments:
        tileset - tileset for adding asset metadata
        asset_df - dataframe containing mappings for asset metadata
    Returns:
    The tileset generated with asset metadata as a python dictionary
    """
    # Initialise tileset structure for assets
    append_asset_metadata_schema(tileset)
    append_assets(tileset, asset_df)
