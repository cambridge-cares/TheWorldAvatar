"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.root_tile submodule.
"""

# Standard library imports
import os

# Self import
import agent.config.config as properties
from agent.ifc2tileset.root_tile import root_tile, gen_root_content


def test_root_tile():
    """
    Tests root_tile()
    """
    properties.bbox_root = []
    expected_tileset = {
        'asset': {'version': '1.1'},
        'geometricError': 1024,
        'root': {
            "boundingVolume": {"box": []},
            "geometricError": 512,
            "refine": "ADD",
        }
    }
    assert expected_tileset == root_tile()


def test_gen_root_content_no_building():
    """
    Tests gen_root_content() when there is no building and furniture.gltf detected
    """
    assert gen_root_content() == root_tile()


def test_gen_root_content_only_building():
    """
    Tests gen_root_content() when there is only building.gltf
    """
    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    open(building, "x", encoding="utf-8")
    tileset = gen_root_content()
    os.remove(building)
    # Ensure that tileset contains this dictionary
    assert tileset["root"]["content"] == {"uri": "./gltf/building.gltf"}


def test_gen_root_content_building_furniture():
    """
    Tests gen_root_content() when there is both building and furniture.gltf available
    """
    # Create both glTF files for testing
    building = os.path.join("data", "gltf", "building.gltf")
    furniture = os.path.join("data", "gltf", "furniture.gltf")
    open(building, "x", encoding="utf-8")
    open(furniture, "x", encoding="utf-8")
    tileset = gen_root_content()
    os.remove(building)
    os.remove(furniture)
    # Ensure that tileset contains this dictionary
    assert tileset["root"]["contents"] == [
        {"uri": "./gltf/furniture.gltf"}, {"uri": "./gltf/building.gltf"}]
