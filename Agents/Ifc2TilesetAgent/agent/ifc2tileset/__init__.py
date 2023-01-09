"""
# Author: qhouyee #

This is the submodule's entry function to
generate the tilesets for the IFC file and its glTF models.
"""
# Third party imports
from py4jps import agentlogging

# Self imports
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter
from agent.ifc2tileset.root_tile import gen_root_content
from agent.ifc2tileset.asset_tiles import gen_tileset_assets

# Retrieve logger
logger = agentlogging.get_logger("dev")

def gen_tilesets(hashmapping):
    """
    Generates the tileset in json format

    Argument:
        hashmappings - A hashtable to match assets to their IFC ID
    """
    # Create and write separate tilesets if they exist
    gen_solarpanel_tileset()
    gen_sewagenetwork_tileset()

    # If there are assets, generate tilesets with asset information
    if hashmapping:
        logger.info("Individual glTF assets detected. Attaching tileset with assets...")
        jsonwriter(gen_tileset_assets(hashmapping), "tileset_bim")
    # Else, generate only the root building contents
    else:
        logger.info("No individual glTF assets detected. Generating default tileset...")
        jsonwriter(gen_root_content(), "tileset_bim")
