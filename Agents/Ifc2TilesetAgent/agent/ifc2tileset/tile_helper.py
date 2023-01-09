"""
# Author: qhouyee #

This module provides helper methods to generate separate tilesets and write to a json file.
"""

# Standard library imports
import json
from pathlib import Path

# Third party imports
from py4jps import agentlogging

# Self imports
from agent.ifc2tileset.root_tile import root_tile

# Retrieve logger
logger = agentlogging.get_logger("dev")

def gen_solarpanel_tileset():
    """
    Generates and write the tileset for solar panel into 3D Tiles Next format if it exists
    """
    solarpanel_file_path = "./data/gltf/solarpanel.gltf"
    solarpath = Path(solarpanel_file_path)
    if solarpath.is_file():
        tileset = root_tile()
        tileset["root"]["content"] = {"uri": "./gltf/solarpanel.gltf"}
        jsonwriter(tileset, "tileset_solarpanel")


def gen_sewagenetwork_tileset():
    """
    Generates and write the tileset for sewage network into 3D Tiles Next format if it exists
    """
    sewage_file_path = "./data/gltf/sewagenetwork.gltf"
    sewagepath = Path(sewage_file_path)
    if sewagepath.is_file():
        tileset = root_tile()
        tileset["root"]["content"] = {"uri": "./gltf/sewagenetwork.gltf"}
        jsonwriter(tileset, "tileset_sewage")


def jsonwriter(tileset, tileset_string):
    """
    Writes the python dictionary into 3D Tiles Next in JSON format

    Arguments:
            tileset - a python dictionary formatted for 3D Tiles
            tileset_string - a string containing the output tileset's name
    """
    json_string = json.dumps(tileset)
    with open('data/'+tileset_string+'.json', 'w', encoding="utf-8") as outfile:
        outfile.write(json_string)
    logger.info(tileset_string+".json have been generated")
