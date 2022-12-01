"""
# Author: qhouyee #

This module generates the tilesets for the IFC file and its glTF models.
"""

# Standard library imports
import json
from pathlib import Path

# Self imports
from ifc2tileset.root_tile import root_tile

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
    print(tileset_string+".json have been generated")
