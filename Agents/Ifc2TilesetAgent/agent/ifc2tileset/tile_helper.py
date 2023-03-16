"""
# Author: qhouyee #

This module provides helper methods to generate separate tilesets and write to a json file.
"""

# Standard library imports
import json
from pathlib import Path
from typing import List, Union, Optional
import os

# Third party imports
import numpy as np
import trimesh
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.schema import Tileset, Tile

# Retrieve logger
logger = agentlogging.get_logger("dev")


def make_root_tile(bbox: Optional[List[float]] = None, **kwargs):
    bounding_volume = {"box": bbox} if bbox is not None else {}
    root_tile = Tile(
        boundingVolume=bounding_volume,
        geometricError=512,
    )

    for key, value in kwargs.items():
        root_tile[key] = value

    return root_tile


def make_tileset(root_tile: Tile):
    """
    Defines a skeleton template for all tilesets as a dictionary
    to write into the required json format

    Returns:
    The root tileset generated as a python dictionary
    """
    return Tileset(
        asset={"version": "1.1"},
        geometricError=1024,
        root=root_tile
    )


PathLike = Union[str, bytes, os.PathLike]


def compute_bbox(gltf: Union[PathLike, List[PathLike]]):
    """
    Computes bbox for a given glTF/GLB file(s)
    """
    if not isinstance(gltf, list):
        gltf = [gltf]

    meshes = [trimesh.load(file, force="mesh") for file in gltf]
    vertices = np.vstack([mesh.vertices for mesh in meshes])

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
        bbox = compute_bbox("./data/glb/solarpanel.glb")
        root_tile = make_root_tile(bbox=bbox, content={"uri": state.asset_url + "solarpanel.gltf"})
        tileset = make_tileset(root_tile)

        jsonwriter(tileset, "tileset_solarpanel")


def gen_sewagenetwork_tileset():
    """
    Generates and write the tileset for sewage network into 3D Tiles Next format if it exists
    """
    sewage_file_path = "./data/gltf/sewagenetwork.gltf"
    sewagepath = Path(sewage_file_path)

    if sewagepath.is_file():
        bbox = compute_bbox("./data/glb/sewagenetwork.glb")
        root_tile = make_root_tile(bbox=bbox, content={"uri": state.asset_url + "sewagenetwork.gltf"})
        tileset = make_tileset(root_tile)

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
