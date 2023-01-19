"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.root_tile submodule.
"""

# Standard library imports
import os

# Self import
import agent.config.config as properties
from agent.ifc2tileset.root_tile import root_tile, append_tileset_schema, gen_root_content

ENDPOINT = "http://www.example.org/sparql"

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


def test_append_tileset_schema():
    """
    Tests append_tileset_schema()
    """
    # Initialise test cases and expected result
    endpoint = "http://www.example.org/sparql"
    result = {}
    expected_tileset = {
        'schema': {'classes': {
                    'TilesetMetaData': {
                        'name': "Tileset metadata",
                        'description': "A metadata class for the tileset",
                        'properties': {
                            'queryEndpoint': {
                                "description": "SPARQL query endpoint",
                                "type": "STRING"
                            },
                            'updateEndpoint': {
                                "description": "SPARQL update endpoint",
                                "type": "STRING"
                            }
                        }
                    }}},
        'metadata': {
            'class': 'TilesetMetaData',
            'properties': {
                'queryEndpoint': endpoint,
                'updateEndpoint': endpoint,
            }
        }
    }
    # Execute method
    append_tileset_schema(result, endpoint, endpoint)
    # Test assertion
    assert expected_tileset == result


def test_gen_root_content_no_building():
    """
    Tests gen_root_content() when there is no building and furniture.gltf detected
    """
    # Initialise test cases and expected result
    expected = root_tile()
    append_tileset_schema(expected, ENDPOINT, ENDPOINT)
    assert expected == gen_root_content(ENDPOINT, ENDPOINT)


def test_gen_root_content_only_building():
    """
    Tests gen_root_content() when there is only building.gltf
    """
    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    try:
        open(building, "x", encoding="utf-8")
        tileset = gen_root_content(ENDPOINT, ENDPOINT)
    finally:
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
    try:
        open(building, "x", encoding="utf-8")
        open(furniture, "x", encoding="utf-8")
        tileset = gen_root_content(ENDPOINT, ENDPOINT)
    finally:
        os.remove(building)
        os.remove(furniture)
    # Ensure that tileset contains this dictionary
    assert tileset["root"]["contents"] == [
        {"uri": "./gltf/furniture.gltf"}, {"uri": "./gltf/building.gltf"}]
