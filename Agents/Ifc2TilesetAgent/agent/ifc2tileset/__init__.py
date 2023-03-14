"""
# Author: qhouyee #

This is the submodule's entry function to
generate the tilesets for the IFC file and its glTF models.
"""
# Third party imports
import pandas as pd
from py4jps import agentlogging

# Self imports
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter
from agent.ifc2tileset.root_tile import gen_root_content
from agent.ifc2tileset.asset_tiles import gen_tileset_assets

# Retrieve logger
logger = agentlogging.get_logger("dev")


def gen_tilesets(asset_data: pd.DataFrame, building_iri: str):
    """
    Generates the tileset in json format

    Argument:
        asset_data - A dataframe containing all individual asset metadata
        building_iri - The data IRI of the building
    """
    # Create and write separate tilesets if they exist
    gen_solarpanel_tileset()
    gen_sewagenetwork_tileset()

    # Generate tileset with root contents
    bim_tileset = gen_root_content(building_iri)

    # If there are assets, append tileset with asset information
    if not asset_data.empty:
        logger.info("Individual glTF assets detected. Attaching tileset with asset metadata...")
        gen_tileset_assets(asset_data, bim_tileset)

    # If there are any root content (building or furniture) or asset info
    if "contents" in bim_tileset["root"] or \
            "content" in bim_tileset["root"] or \
            "children" in bim_tileset["root"]:
        # In such cases, generate the bim tileset
        jsonwriter(bim_tileset, "tileset_bim")
