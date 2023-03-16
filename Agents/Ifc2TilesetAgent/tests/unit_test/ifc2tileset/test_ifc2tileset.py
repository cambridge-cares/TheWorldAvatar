"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset submodule.
"""

# Standard import
import os

# Third party import
import numpy as np
import pandas as pd

# Self import
from agent.ifc2tileset import gen_tilesets
from . import testconsts as C
from .testutils import read_json, gen_sample_asset_df


def test_gen_tilesets_solarpanel():
    """
    Tests gen_tilesets() for generating only solarpanel tileset
    """
    # Create a solarpanel.gltf for testing
    solarpanel_gltf = os.path.join("data", "gltf", "solarpanel.gltf")
    open(solarpanel_gltf, "x", encoding="utf-8")

    # Create sample glb file
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "building_iri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(solar_json_filepath)
        assert not os.path.exists(bim_json_filepath)
        assert not os.path.exists(sewage_json_filepath)

        # Read the tileset
        tileset_content = read_json(solar_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/solarpanel.gltf"}
        # Test that the bbox is correctly computed
        assert np.allclose(tileset_content["root"]["boundingVolume"]["box"], C.sample_box_bbox)
    finally:
        # Remove files
        os.remove(solarpanel_gltf)
        os.remove(solarpanel_glb)
        os.remove(solar_json_filepath)


def test_gen_tilesets_sewage():
    """
    Tests gen_tilesets() for generating only sewage tileset
    """
    # Create a sewagenetwork.gltf for testing
    sewage_gltf = os.path.join("data", "gltf", "sewagenetwork.gltf")
    open(sewage_gltf, "x", encoding="utf-8")

    # Create sample glb file
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "building_iri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(sewage_json_filepath)
        assert not os.path.exists(bim_json_filepath)
        assert not os.path.exists(solar_json_filepath)

        # Read the tileset
        tileset_content = read_json(sewage_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/sewagenetwork.gltf"}
        # Test that the bbox is correctly computed
        assert np.allclose(tileset_content["root"]["boundingVolume"]["box"], C.sample_cone_bbox)
    finally:
        # Remove files
        os.remove(sewage_gltf)
        os.remove(sewage_glb)
        os.remove(sewage_json_filepath)


def assert_tileset_metadata(tileset_content: dict):
    assert "schema" in tileset_content
    assert "classes" in tileset_content["schema"]
    assert "TilesetMetaData" in tileset_content["schema"]["classes"]
    assert tileset_content["schema"]["classes"]["TilesetMetaData"] == {
        "name": "Tileset metadata",
        "description": "A metadata class for the tileset",
        "properties": {
            "buildingIri": {
                "description": "Data IRI of the building",
                "type": "STRING"
            }
        }
    }

    assert "metadata" in tileset_content
    assert tileset_content["metadata"] == {
        "class": "TilesetMetaData",
        "properties": {
            "buildingIri": "building_iri"
        }
    }


def test_gen_tilesets_building():
    """
    Tests gen_tilesets() for generating only the bim tileset without asset data
    """
    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    open(building, "x", encoding="utf-8")

    # Create sample glb file
    building_glb = os.path.join("data", "glb", "building.glb")
    m = C.sample_box_gen()
    m.export(building_glb)

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "building_iri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(bim_json_filepath)
        assert not os.path.exists(sewage_json_filepath)
        assert not os.path.exists(solar_json_filepath)

        # Read the tileset
        tileset_content = read_json(bim_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/building.gltf"
        }
        assert np.allclose(tileset_content["root"]["boundingVolume"]["box"], C.sample_box_bbox)
        assert "children" not in tileset_content["root"]
        assert_tileset_metadata(tileset_content)
    finally:
        # Remove files
        os.remove(building)
        os.remove(bim_json_filepath)


def test_gen_tilesets_asset():
    """
    Tests gen_tilesets() for generating the bim tileset with only asset data
    """
    # Initialise test case
    sampledf = gen_sample_asset_df(3)

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")

    try:
        # Execute method
        gen_tilesets(sampledf, "building_iri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(bim_json_filepath)

        # Read the tileset
        tileset_content = read_json(bim_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert "children" in tileset_content["root"]
        assert "content" not in tileset_content["root"]
        assert "contents" not in tileset_content["root"]
        assert_tileset_metadata(tileset_content)
    finally:
        # Remove files
        os.remove(bim_json_filepath)
