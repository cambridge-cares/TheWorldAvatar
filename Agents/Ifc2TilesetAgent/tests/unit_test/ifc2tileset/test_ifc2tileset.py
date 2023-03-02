"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset submodule.
"""

# Standard import
import os

# Third party import
import pandas as pd

# Self import
from agent.ifc2tileset import gen_tilesets
from tests.unit_test.ifc2tileset.test_tile_helper import retrieve_tileset_contents
from tests.unit_test.ifc2tileset.test_asset_tiles import gen_sample_df


def test_gen_tilesets_solarpanel():
    """
    Tests gen_tilesets() for generating only solarpanel tileset
    """
    # Create a solarpanel.gltf for testing
    solarpanel = os.path.join("data", "gltf", "solarpanel.gltf")
    open(solarpanel, "x", encoding="utf-8")

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "buildingIri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(solar_json_filepath)
        assert not os.path.exists(bim_json_filepath)
        assert not os.path.exists(sewage_json_filepath)

        # Read the tileset
        tileset_content = retrieve_tileset_contents(solar_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/solarpanel.gltf"}
    finally:
        # Remove files
        os.remove(solarpanel)
        os.remove(solar_json_filepath)


def test_gen_tilesets_sewage():
    """
    Tests gen_tilesets() for generating only sewage tileset
    """
    # Create a sewagenetwork.gltf for testing
    sewage = os.path.join("data", "gltf", "sewagenetwork.gltf")
    open(sewage, "x", encoding="utf-8")

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "buildingIri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(sewage_json_filepath)
        assert not os.path.exists(bim_json_filepath)
        assert not os.path.exists(solar_json_filepath)

        # Read the tileset
        tileset_content = retrieve_tileset_contents(sewage_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/sewagenetwork.gltf"}
    finally:
        # Remove files
        os.remove(sewage)
        os.remove(sewage_json_filepath)


def test_gen_tilesets_building():
    """
    Tests gen_tilesets() for generating only the bim tileset without asset data
    """
    # Create a building.gltf for testing
    building = os.path.join("data", "gltf", "building.gltf")
    open(building, "x", encoding="utf-8")

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    try:
        # Execute method
        gen_tilesets(pd.DataFrame(), "buildingIri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(bim_json_filepath)
        assert not os.path.exists(sewage_json_filepath)
        assert not os.path.exists(solar_json_filepath)

        # Read the tileset
        tileset_content = retrieve_tileset_contents(bim_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/building.gltf"}
        assert "children" not in tileset_content["root"]
    finally:
        # Remove files
        os.remove(building)
        os.remove(bim_json_filepath)


def test_gen_tilesets_asset():
    """
    Tests gen_tilesets() for generating the bim tileset with only asset data
    """
    # Initialise test case
    sampledf = gen_sample_df(3)

    # JSON output path
    bim_json_filepath = os.path.join("data", "tileset_bim.json")

    try:
        # Execute method
        gen_tilesets(sampledf, "buildingIri")

        # Test that only relevant tileset.json are created
        assert os.path.exists(bim_json_filepath)

        # Read the tileset
        tileset_content = retrieve_tileset_contents(bim_json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert "children" in tileset_content["root"]
        assert "content" not in tileset_content["root"]
        assert "contents" not in tileset_content["root"]
    finally:
        # Remove files
        os.remove(bim_json_filepath)
