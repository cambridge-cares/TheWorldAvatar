"""
# Author: qhouyee, picas9dan #

This is the submodule's entry function to
generate the tilesets for the IFC file and its geometry outputs.
"""

# Standard library imports
from typing import List, Optional

# Third-party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter
from agent.ifc2tileset.root_tile import gen_root_content
from agent.ifc2tileset.asset_tiles import append_tileset_assets

# Retrieve logger
logger = agentlogging.get_logger("dev")


def gen_tilesets(asset_data: pd.DataFrame, building_data: list[str], solar_panel_data: list[str], sewage_data: list[str], root_content_parameters: Optional[List[str]] = []):
    """Generates the tileset in the json format.

    Args:
        asset_data: A dataframe containing all individual asset metadata with headers 'file', 'name', 'uid', 'iri'.
        building_data: A list containing the data IRI and name of the building in this order.
        solar_panel_data: A list containing the data IRI and name of the solar panel in this order.
        sewage_data: A list containing the data IRI and name of the sewage network in this order.
        root_content_parameters: A list containing the IRI and name to be appended to root content in this order. Optional for non-building tilesets.
    """
    # Create and write separate tilesets if they exist
    gen_solarpanel_tileset(solar_panel_data)
    gen_sewagenetwork_tileset(sewage_data)

    # Generate tileset with root contents
    bim_tileset = gen_root_content(
        asset_data, building_data, root_content_parameters)

    # If there are assets, append tileset with asset information
    append_tileset_assets(bim_tileset, asset_data)

    # If there are any building, furniture, or asset, bim_tileset will be not None
    if bim_tileset is not None:
        # In such case, generate the bim tileset
        jsonwriter(bim_tileset, "tileset_bim")
