"""
# Author: qhouyee, picas9dan #

This module provides helper methods to generate separate tilesets and write to a json file.
"""

# Standard library imports
import json
from pathlib import Path
from typing import List, Union, Optional, Iterable
import os

# Third-party imports
import numpy as np
import trimesh
from py4jps import agentlogging

# Self imports
import agent.app as state
from agent.ifc2tileset.schema import Tileset, Tile
from agent.kgutils.const import IRI_VAR, NAME_VAR

# Retrieve logger
logger = agentlogging.get_logger("dev")


def append_content_metadata_schema(tileset: Tileset):
    """Appends the schema of content metadata for building and asset to the tileset."""

    tileset["schema"] = {"classes": {
        "ContentMetaData": {
            "name": "Content metadata",
            "description": "A metadata class for all content including building and individual assets",
            # Store all content and asset information here even if they are not used
            "properties": {
                NAME_VAR: {
                    "description": "Name of the asset/building",
                    "type": "STRING"
                },
                IRI_VAR: {
                    "description": "Data IRI of the asset/building",
                    "type": "STRING"
                }
            }
        }
    }}


def make_root_tile(bbox: Optional[List[float]] = None, geometry_file_paths: Optional[List[str]] = [], root_metadata: Optional[List[str]] = []):
    """Generates a root tile with the provided arguments and default values. The root tile will only
    include geometry content for non-asset elements like the building, furniture, and solar panels.

    Args:
        bbox (optional): A 12-element list that represents Next tileset's boundingVolume.box property. Defaults to None.
        geometry_file_paths (optional): A list of geometry file paths if available to be appended. Defaults to an empty list.
        root_metadata (optional): A list containing the data IRI and name of the root contents in this order. Defaults to an empty list.
    Returns:
        A root tile.
    """
    bounding_volume = {"box": bbox} if bbox is not None else {}
    root_tile = Tile(
        boundingVolume=bounding_volume,
        geometricError=512,
        refine="ADD",
    )
    # If there is building data, generate a placeholder dictionary content
    if len(root_metadata) > 0:
        building_dict = {
            "class": "ContentMetaData",
            "properties": {
                IRI_VAR: root_metadata[0]
            }
        }
        # If there is a building name, add it to the tileset
        if root_metadata[1]:
            building_dict["properties"][NAME_VAR] = root_metadata[1]

    # If there are geometry contents available
    if geometry_file_paths:
        # And if there is only one item in the list, use the "content" nomenclature
        if len(geometry_file_paths) == 1:
            root_tile["content"] = {"uri": geometry_file_paths[0]}
            # If there is building data, append the metadata
            if len(root_metadata) > 0:
                root_tile["content"]["metadata"] = building_dict
        else:
            # If there are more than one item, use the "contents" nomenclature with a list
            # Sample format: {"contents": [{"uri":"path1"}, {"uri":"path2"}]}
            root_tile["contents"] = []
            for path in geometry_file_paths:
                temp_dict = {"uri": path}
                # If there is building data, append the metadata to all geometries
                if len(root_metadata) > 0:
                    temp_dict["metadata"] = building_dict
                root_tile["contents"].append(temp_dict)

    return root_tile


def make_tileset(root_tile: Tile):
    """Generates a tileset that wraps around the provided root tile, with default properties.

    Args:
        root_tile: A tile.

    Returns:
        A tileset.
    """
    tileset = Tileset(
        asset={"version": "1.1"},
        geometricError=1024,
        root=root_tile
    )
    # If there is building data,
    if ("content" in root_tile and "metadata" in root_tile["content"]) \
            or ("contents" in root_tile and "metadata" in root_tile["contents"][0]):
        # Attach the metadata schema
        append_content_metadata_schema(tileset)
    return tileset


def y_up_to_z_up(x_min: float, y_min: float, z_min: float, x_max: float, y_max: float, z_max: float):
    """Transforms a bounding box's extreme coordinates in the y-up system to the z-up system.
    """
    return x_min, -z_max, y_min, x_max, -z_min, y_max


PathLike = Union[str, bytes, os.PathLike]


def compute_bbox(gltf: Union[PathLike, Iterable[PathLike]], offset: float = 0):
    """Computes Next tileset bbox for a given glTF/glb file(s).

    The y-up coordinate system of glTF will be transformed to the z-up system of Next tileset.

    Args:
        gltf: A path or a list of paths to glTF/glb file(s).
        offset: Amount of x- and y-offsets.

    Returns:
        A 12-element list that represents Next tileset's boundingVolume.box property.
    """
    if not isinstance(gltf, list):
        gltf = [gltf]

    meshes = [trimesh.load(file, force="mesh") for file in gltf]
    vertices = np.vstack([mesh.vertices for mesh in meshes])

    mins = vertices.min(axis=0)
    maxs = vertices.max(axis=0)

    # Converts the y-up coordinate system of glTF to the z-up coordinate of Next tileset
    x_min, y_min, z_min, x_max, y_max, z_max = y_up_to_z_up(*mins, *maxs)

    x_min -= offset
    y_min -= offset
    x_max += offset
    y_max += offset

    return [
        (x_min + x_max) / 2, (y_min + y_max) / 2, (z_min + z_max) / 2,
        (x_max - x_min) / 2, 0, 0,
        0, (y_max - y_min) / 2, 0,
        0, 0, (z_max - z_min) / 2
    ]


def gen_solarpanel_tileset(solar_panel_data: list[str]):
    """Generates and write the tileset for solar panel into 3D Tiles Next format if it exists.

    Args:
        solar_panel_data: A list containing the data IRI and name of the solar panel in this order.
    """
    solarpanel_file_path = "./data/glb/solarpanel.glb"
    solarpath = Path(solarpanel_file_path)

    if not solarpath.is_file():
        return

    bbox = compute_bbox(solarpath)
    # No building data should be created
    root_tile = make_root_tile(
        bbox=bbox, geometry_file_paths=[state.asset_url + "solarpanel.glb"], root_metadata=solar_panel_data)
    tileset = make_tileset(root_tile)

    jsonwriter(tileset, "tileset_solarpanel")


def gen_sewagenetwork_tileset(sewage_data: list[str]):
    """Generates and writes the tileset for sewage network into 3D Tiles Next format if it exists.

    Args:
        sewage_data: A list containing the data IRI and name of the sewage network in this order.
    """
    sewage_file_path = "./data/glb/sewagenetwork.glb"
    sewagepath = Path(sewage_file_path)

    if not sewagepath.is_file():
        return

    bbox = compute_bbox(sewagepath)
    # No building data should be created
    root_tile = make_root_tile(
        bbox=bbox,  geometry_file_paths=[state.asset_url + "sewagenetwork.glb"], root_metadata=sewage_data)
    tileset = make_tileset(root_tile)

    jsonwriter(tileset, "tileset_sewage")


def jsonwriter(tileset: Tileset, filename: str):
    """Writes a tileset object into 3D Tiles Next file in JSON format.

    Args:
        tileset: A tileset object.
        filename: The output tileset's filename.
    """
    filepath = os.path.join("data", filename + ".json")

    with open(filepath, 'w', encoding="utf-8") as outfile:
        json.dump(tileset, outfile)

    logger.info(filename + ".json have been generated")
