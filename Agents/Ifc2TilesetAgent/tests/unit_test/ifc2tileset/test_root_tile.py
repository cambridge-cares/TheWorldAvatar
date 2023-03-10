"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.root_tile submodule.
"""

# Standard library imports
import os
from typing import List

# Self import
import agent.config.config as properties
from agent.ifc2tileset.root_tile import append_tileset_schema_and_metadata, gen_root_content
from agent.ifc2tileset.tile_helper import make_tileset, make_root_tile
from . import testconsts as C

ENDPOINT = "http://www.example.org/sparql"


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


def make_bim_tileset(bbox: List[str], building_iri: str):
    root_tile = make_root_tile(bbox=bbox)
    tile = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tile, building_iri)
    return tile


def test_gen_root_content_no_building_no_furniture():
    """
    Tests gen_root_content() when there is no building and furniture.gltf detected
    """
    # arrange
    # expected tileset is minimal
    building_iri = "test_iri"
    expected = make_bim_tileset(properties.bbox_root, building_iri)

    # act
    actual = gen_root_content("test_iri")

    # assert
    assert actual == expected


def test_gen_root_content_only_building():
    """
    Tests gen_root_content() when there is only building.gltf
    """
    # arrange
    # expected tileset contains a single building directory
    building_iri = "test_iri"
    expected = make_bim_tileset(C.sample_box_bbox, building_iri)
    expected["root"]["content"] = {"uri": "./gltf/building.gltf"}

    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    open(building, "x", encoding="utf-8")

    # Create sample glb file
    building_glb = os.path.join("data", "glb", "building.glb")
    m = C.sample_box_gen()
    m.export(building_glb)

    try:
        # act
        actual = gen_root_content("test_iri")
    finally:
        os.remove(building)

    # assert
    assert actual == expected


def test_gen_root_content_with_building_and_furniture():
    """
    Tests gen_root_content() when there are both building and furniture.gltf available
    """
    # arrange
    # expected tileset contains directories of furniture and building
    building_iri = "test_iri"
    expected = make_bim_tileset(C.combined_bbox, building_iri)
    expected["root"]["contents"] = [
        {"uri": "./gltf/furniture.gltf"}, {"uri": "./gltf/building.gltf"}]

    # Create both glTF files for testing
    building = os.path.join("data", "gltf", "building.gltf")
    furniture = os.path.join("data", "gltf", "furniture.gltf")
    open(building, "x", encoding="utf-8")
    open(furniture, "x", encoding="utf-8")

    # Create GLB files for testing
    building_glb = os.path.join("data", "glb", "building.glb")
    furniture_glb = os.path.join("data", "glb", "furniture.glb")
    building_mesh = C.sample_box_gen()
    furniture_mesh = C.sample_cone_gen()
    building_mesh.export(building_glb)
    furniture_mesh.export(furniture_glb)

    try:
        # act
        actual = gen_root_content("test_iri")

        # Ensure that tileset contains this dictionary
        assert actual == expected

    finally:
        os.remove(building)
        os.remove(furniture)
