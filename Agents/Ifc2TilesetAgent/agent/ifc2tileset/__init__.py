"""
# Author: qhouyee, picas9dan #

This is the submodule's entry function to
generate the tilesets for the IFC file and its glTF models.
"""

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter
from agent.ifc2tileset.root_tile import gen_root_content
from agent.ifc2tileset.asset_tiles import append_tileset_assets

# Retrieve logger
logger = agentlogging.get_logger("dev")


def gen_tilesets(asset_data: pd.DataFrame, building_iri: str):
    """Generates the tileset in the json format.

    Args:
        asset_data: A dataframe containing all individual asset metadata with headers 'file', 'name', 'uid', 'iri'.
        building_iri: The data IRI of the building.
    """
    # Create and write separate tilesets if they exist
    gen_solarpanel_tileset()
    gen_sewagenetwork_tileset()

    # Generate tileset with root contents
    bim_tileset = gen_root_content(building_iri, asset_data)

    # If there are assets, append tileset with asset information
    append_tileset_assets(bim_tileset, asset_data)

    # If there are any building, furniture, or asset, bim_tileset will be not None
    if bim_tileset is not None:
        # In such case, generate the bim tileset
        jsonwriter(bim_tileset, "tileset_bim")
