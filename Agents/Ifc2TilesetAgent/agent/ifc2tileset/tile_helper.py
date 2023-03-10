"""
# Author: qhouyee #

This module provides helper methods to generate separate tilesets and write to a json file.
"""

# Standard library imports
import json
from pathlib import Path

# Third party imports
import numpy as np
import trimesh
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.root_tile import root_tile

# Retrieve logger
logger = agentlogging.get_logger("dev")


def get_bbox(gltf_file):
    """
    Computes bbox for a given glTF/GLB file
    """
    m = trimesh.load(gltf_file, force="mesh")
    vertices = np.array(m.vertices)

    mins = vertices.min(axis=0)
    maxs = vertices.max(axis=0)
    center = (mins + maxs) / 2
    diffs = maxs - mins

    return [
        center[0], center[1], center[2],
        diffs[0] / 2, 0, 0,
        0, diffs[1] / 2, 0,
        0, 0, diffs[2] / 2
    ]


def gen_solarpanel_tileset():
    """
    Generates and write the tileset for solar panel into 3D Tiles Next format if it exists
    """
    solarpanel_file_path = "./data/gltf/solarpanel.gltf"
    solarpath = Path(solarpanel_file_path)
    if solarpath.is_file():
        bbox = get_bbox("./data/glb/solarpanel.glb")
        tileset = root_tile(bbox)
        tileset["root"]["content"] = {"uri": state.asset_url + "solarpanel.gltf"}
        jsonwriter(tileset, "tileset_solarpanel")


def gen_sewagenetwork_tileset():
    """
    Generates and write the tileset for sewage network into 3D Tiles Next format if it exists
    """
    sewage_file_path = "./data/gltf/sewagenetwork.gltf"
    sewagepath = Path(sewage_file_path)
    if sewagepath.is_file():
        bbox = get_bbox("./data/glb/sewagenetwork.glb")
        tileset = root_tile(bbox)
        tileset["root"]["content"] = {"uri": state.asset_url + "sewagenetwork.gltf"}
        jsonwriter(tileset, "tileset_sewage")


def jsonwriter(tileset: dict, tileset_string: str):
    """
    Writes the python dictionary into 3D Tiles Next in JSON format

    Arguments:
            tileset - a python dictionary formatted for 3D Tiles
            tileset_string - a string containing the output tileset's name
    """
    with open('data/' + tileset_string + '.json', 'w', encoding="utf-8") as outfile:
        json.dump(tileset, outfile)
    logger.info(tileset_string + ".json have been generated")
