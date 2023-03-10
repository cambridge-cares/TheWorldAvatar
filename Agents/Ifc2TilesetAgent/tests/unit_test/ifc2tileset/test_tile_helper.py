"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.tile_helper submodule.
"""

# Standard library imports
import os
import json

# Third party import
import numpy as np
import pytest

# Self import
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter, compute_bbox, \
    make_tileset
from . import testconsts as C


def test_make_tileset():
    # arrange
    root_tile = {
        "boundingVolume": {"box": []},
        "geometricError": 512,
        "refine": "ADD",
    }
    expected = {
        "asset": {"version": "1.1"},
        "geometricError": 1024,
        "root": {
            "boundingVolume": {"box": []},
            "geometricError": 512,
            "refine": "ADD",
        }
    }

    # act
    actual = make_tileset(root_tile)

    # assert
    assert expected == actual

@pytest.mark.parametrize(
    "mesh_gen, expected",
    [(C.sample_box_gen, C.sample_box_bbox),
     (C.sample_cone_gen, C.sample_cone_bbox)]
)
def test_compute_bbox_single_mesh(mesh_gen, expected, tmp_path):
    # arrange
    glb_path = tmp_path / "sample.glb"
    m = mesh_gen()
    m.export(glb_path)

    # act
    actual = compute_bbox(glb_path)

    # assert
    assert np.allclose(expected, actual)


def test_compute_bbox_multi_mesh(tmp_path):
    # arrange
    glb_path1 = tmp_path / "sample1.glb"
    glb_path2 = tmp_path / "sample2.glb"
    m1 = C.sample_box_gen()
    m2 = C.sample_cone_gen()
    m1.export(glb_path1)
    m2.export(glb_path2)

    # act
    actual = compute_bbox([glb_path1, glb_path2])

    # assert
    assert np.allclose(C.combined_bbox, actual)


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
    solarpanel_gltf = os.path.join("data", "gltf", "solarpanel.gltf")
    open(solarpanel_gltf, "x", encoding="utf-8")

    # Create sample glb file
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

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
        # Test that the bbox is correctly computed
        assert np.allclose(tileset_content["root"]["boundingVolume"]["box"], C.sample_box_bbox)
    finally:
        os.remove(solarpanel_gltf)  # Remove glTF
        os.remove(solarpanel_glb)   # Remove glb
        os.remove(json_filepath)    # Remove tileset


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
    sewage_gltf = os.path.join("data", "gltf", "sewagenetwork.gltf")
    open(sewage_gltf, "x", encoding="utf-8")

    # Create sample glb file
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

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
        # Test that the bbox is correctly computed
        assert np.allclose(tileset_content["root"]["boundingVolume"]["box"], C.sample_cone_bbox)
    finally:
        os.remove(sewage_gltf)    # Remove glTF
        os.remove(sewage_glb)     # Remove glb
        os.remove(json_filepath)  # Remove tileset
