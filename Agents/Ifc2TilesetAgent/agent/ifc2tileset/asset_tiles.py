"""
# Author: qhouyee, picas9dan #

This module provides methods to generate tilesets related to assets.
"""

# Standard library imports
from typing import List, Optional

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.schema import Tileset, Tile, Content
from agent.ifc2tileset.tile_helper import append_content_metadata_schema, compute_bbox
from agent.kgutils.const import NAME_VAR, ID_VAR, IRI_VAR

logger = agentlogging.get_logger("dev")

def append_assets_to_tile(tile: Tile, asset_df: pd.DataFrame):
    """Appends a child node containing the given assets to the given tile.

    Args:
        tile: parent node to have assets added as a child node.
        asset_df: dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
    """
    def _asset_data_to_tileset_content(row: pd.Series):
        # Add geometry and uid for each asset
        return {
            "uri": state.asset_url + row["file"] + ".glb",
            # Add the asset name to establish a metadata skeleton
            "metadata": {
                "class": "ContentMetaData",
                "properties": {
                    NAME_VAR: row[NAME_VAR].split(":")[0],
                    IRI_VAR: row[IRI_VAR]
                }
            }
        }

    contents: List[Content] = asset_df.apply(
        _asset_data_to_tileset_content, axis=1).tolist()

    # Add an offset of 5m in the x- and y-directions to ensure non-negligible bounding box volume in the case of
    # small-sized assets
    bbox = compute_bbox(
        [f"./data/glb/{file}.glb" for file in asset_df["file"]], offset=5)

    tile["children"] = [{
        "boundingVolume": {"box": bbox},
        "geometricError": 50,
        # Nomenclature for 1 geometry file = content:{}, multiple files = contents:[{}]
        "contents": contents
    }]


def append_assets_to_tileset(tileset: Tileset, asset_df: pd.DataFrame):
    """Adds 6 assets to every nested child node, starting from the root tile.

    Args:
        tileset: A parent node to have assets added as a child node.
        asset_df: A dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
    """
    # CesiumJS only supports six content per child node by default:
    # https://github.com/CesiumGS/cesium/issues/10468
    assets_num_per_node = 6

    tile_node = tileset["root"]
    for i in range(0, len(asset_df), assets_num_per_node):
        # CAVEAT: Functionality for visualising tilesets with >10 child nodes in Cesium has yet to be tested.
        # If the code fails to work for more child nodes, perform early stopping when above the max limit of child nodes
        append_assets_to_tile(
            tile_node, asset_df.iloc[i: i + assets_num_per_node])
        tile_node = tile_node["children"][0]


def append_tileset_assets(tileset: Optional[Tileset], asset_df: pd.DataFrame):
    """Adds asset properties into the tileset.

    If tileset is None or asset_df is empty, no action will be performed.

    Args:
        tileset: A tileset to have asset metadata added.
        asset_df: A dataframe containing mappings for asset metadata, with headers 'file', 'name', 'uid', 'iri'.
    """
    if tileset is None or asset_df.empty:
        return

    logger.info(
        "Individual assets detected. Attaching tileset with asset metadata...")
    if "schema" not in tileset:
        append_content_metadata_schema(tileset)
    append_assets_to_tileset(tileset, asset_df)
