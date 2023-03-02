"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.tile_helper submodule.
"""

# Standard library imports
import os
import json

# Self import
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter


def retrieve_tileset_contents(json_filepath: str):
    """
    A test function to read the contents of a tileset.json

    Argument:
    json_filepath - File path to the tileset.json
    Returns:
    The tileset's contents as a Python dictionary
    """
    # Read the results
    json_output = open(json_filepath, "r", encoding="utf-8")
    contents = json_output.read()  # Store as string
    tileset_content = json.loads(contents)  # Convert to dictionary
    json_output.close()
    return tileset_content


def test_jsonwriter():
    """
    Tests jsonwriter()
    """
    sample_tileset = {"testkey": "testvalue"}
    sample_name = "jsontest"
    # JSON output path
    test_json = os.path.join("data", sample_name + ".json")
    try:
        # Execute method
        jsonwriter(sample_tileset, sample_name)
        # Test that the tileset.json has been created
        assert os.path.exists(test_json)
        # Test that the tileset contents are equivalent to the dictionary
        assert retrieve_tileset_contents(test_json) == sample_tileset
    finally:
        os.remove(test_json)  # Remove tileset


def test_gen_solarpanel_tileset_no_solarpanel():
    """
    Tests gen_solarpanel_tileset() when there is no solarpanel.gltf detected
    """
    # Execute method
    gen_solarpanel_tileset()
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert not os.path.exists(json_filepath)


def test_gen_solarpanel_tileset():
    """
    Tests gen_solarpanel_tileset() when there is a solarpanel.gltf
    """
    # Create a solarpanel.gltf for testing
    solarpanel = os.path.join("data", "gltf", "solarpanel.gltf")
    open(solarpanel, "x", encoding="utf-8")
    try:
        # Execute method
        gen_solarpanel_tileset()
        # JSON output path
        json_filepath = os.path.join("data", "tileset_solarpanel.json")
        # Test that the tileset.json has been created
        assert os.path.exists(json_filepath)
        # Read the tileset
        tileset_content = retrieve_tileset_contents(json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/solarpanel.gltf"}
    finally:
        os.remove(solarpanel)  # Remove glTF
        os.remove(json_filepath)  # Remove tileset


def test_gen_sewagenetwork_tileset_no_sewage():
    """
    Tests gen_sewagenetwork_tileset() when there is no sewagenetwork.gltf detected
    """
    # Execute method
    gen_sewagenetwork_tileset()
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert not os.path.exists(json_filepath)


def test_gen_sewagenetwork_tileset():
    """
    Tests gen_sewagenetwork_tileset() when there is a sewagenetwork.gltf
    """
    # Create a sewagenetwork.gltf for testing
    sewage = os.path.join("data", "gltf", "sewagenetwork.gltf")
    open(sewage, "x", encoding="utf-8")
    try:
        # Execute method
        gen_sewagenetwork_tileset()
        # JSON output path
        json_filepath = os.path.join("data", "tileset_sewage.json")
        # Test that the tileset.json has been created
        assert os.path.exists(json_filepath)
        # Read the tileset
        tileset_content = retrieve_tileset_contents(json_filepath)
        # Test that the tileset contents are equivalent to the dictionary
        assert tileset_content["root"]["content"] == {
            "uri": "./gltf/sewagenetwork.gltf"}
    finally:
        os.remove(sewage)  # Remove glTF
        os.remove(json_filepath)  # Remove tileset
