"""
# Author: qhouyee #

This module provides the root tile and its bounding boxes for all tilesets.
"""

# Standard library imports
from pathlib import Path

# Self imports
import agent.app as state
import agent.config.config as properties


def root_tile():
    """
    Defines a skeleton template for all tilesets as a dictionary
    to write into the required json format

    Returns:
    The root tileset generated as a python dictionary
    """
    tileset = {'asset': {'version': '1.1'},
               'geometricError': 1024,
               'root': {"boundingVolume": {"box": properties.bbox_root},
                        "geometricError": 512,
                        "refine": "ADD",
                        }
               }
    return tileset


def gen_root_content():
    """
    Add the root content of building and background furniture to tileset
    If there are no assets, the tileset generated in this function is sufficient for visualisation

    Returns:
    The tileset generated as a python dictionary
    """
    # Generate a minimal tileset
    tileset = root_tile()

    # Respective filepaths
    building_file_path = "./data/gltf/building.gltf"
    bpath = Path(building_file_path)
    furniture_file_path = "./data/gltf/furniture.gltf"
    fpath = Path(furniture_file_path)

    # In a special case where there is no building and furniture, no root content is added
    if bpath.is_file():
        rootlist = []
        if fpath.is_file():
            rootlist += [{"uri": state.asset_url + "furniture.gltf"}]
        # If there are furniture, use the multiple nomenclature
        if rootlist:
            rootlist += [{"uri": state.asset_url + "building.gltf"}]
            # Tileset Nomenclature for multiple geometry files = contents:[{}]
            tileset["root"]["contents"] = rootlist
        else:
            # Tileset Nomenclature for 1 geometry file = content:{}
            tileset["root"]["content"] = {"uri": state.asset_url + "building.gltf"}
    return tileset
