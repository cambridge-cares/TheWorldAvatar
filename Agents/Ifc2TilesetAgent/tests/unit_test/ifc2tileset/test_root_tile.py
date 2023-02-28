"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.root_tile submodule.
"""

# Standard library imports
import os

# Third party import
from pytest_mock import MockFixture

# Self import
import agent.config.config as properties
from agent.ifc2tileset.root_tile import root_tile, append_tileset_schema_and_metadata, gen_root_content

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
    building_iri = "http://www.theworldavatar.com/ifc/building/Building_5a9f7641-2d12-11b2-8040-cdbcaabc8e65"
    result = {}
    expected_tileset = {
        'schema': {'classes': {
            'TilesetMetaData': {
                'name': "Tileset metadata",
                'description': "A metadata class for the tileset",
                'properties': {
                    'buildingIri': {
                        "description": "Data IRI of the building",
                        "type": "STRING"
                    }
                }
            }}},
        'metadata': {
            'class': 'TilesetMetaData',
            'properties': {
                "buildingIri": building_iri
            }
        }
    }
    # Execute method
    append_tileset_schema_and_metadata(result, building_iri)
    # Test assertion
    assert expected_tileset == result


def get_minimal_tileset(building_iri: str):
    tile = root_tile()
    append_tileset_schema_and_metadata(tile, building_iri)
    return tile


def test_gen_root_content_no_building_no_furniture(mocker: MockFixture):
    """
    Tests gen_root_content() when there is no building and furniture.gltf detected
    """
    # arrange
    # expected tileset is minimal
    building_iri = "test_iri"
    expected = get_minimal_tileset(building_iri)
    # mock get_building_iri
    mocker.patch("agent.ifc2tileset.root_tile.get_building_iri", return_value=building_iri)

    # act
    actual = gen_root_content(ENDPOINT, ENDPOINT)

    # assert
    assert actual == expected


def test_gen_root_content_only_building(mocker: MockFixture):
    """
    Tests gen_root_content() when there is only building.gltf
    """
    # arrange
    # expected tileset contains a single building directory
    building_iri = "test_iri"
    expected = get_minimal_tileset(building_iri)
    expected["root"]["content"] = {"uri": "./gltf/building.gltf"}

    # mock get_building_iri
    mocker.patch("agent.ifc2tileset.root_tile.get_building_iri", return_value=building_iri)

    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    open(building, "x", encoding="utf-8")

    try:
        # act
        actual = gen_root_content(ENDPOINT, ENDPOINT)
    finally:
        os.remove(building)

    # assert
    assert actual == expected


def test_gen_root_content_with_building_and_furniture(mocker: MockFixture):
    """
    Tests gen_root_content() when there are both building and furniture.gltf available
    """
    # arrange
    # expected tileset contains directories of furniture and building
    building_iri = "test_iri"
    expected = get_minimal_tileset(building_iri)
    expected["root"]["contents"] = [
        {"uri": "./gltf/furniture.gltf"}, {"uri": "./gltf/building.gltf"}]

    # mock get_building_iri
    mocker.patch("agent.ifc2tileset.root_tile.get_building_iri", return_value=building_iri)

    # Create both glTF files for testing
    building = os.path.join("data", "gltf", "building.gltf")
    furniture = os.path.join("data", "gltf", "furniture.gltf")

    try:
        open(building, "x", encoding="utf-8")
        open(furniture, "x", encoding="utf-8")

        # act
        actual = gen_root_content(ENDPOINT, ENDPOINT)
    finally:
        os.remove(building)
        os.remove(furniture)

    # Ensure that tileset contains this dictionary
    assert actual == expected
