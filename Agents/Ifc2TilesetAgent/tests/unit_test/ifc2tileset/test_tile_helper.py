"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.tile_helper submodule.
"""

# Standard library imports
import os

# Third party import
import numpy as np
import pytest

# Self import
from agent.ifc2tileset.tile_helper import gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter, compute_bbox, \
    make_tileset, make_root_tile, y_up_to_z_up
from . import testconsts as C
from .testutils import read_json


@pytest.mark.parametrize(
    "bbox, kwargs",
    [
        (list(range(12)), dict()),
        (list(range(12)), dict(content={"uri": "./data/gltf/building.gltf"})),
        (None, dict()),
        (None, dict(contents=[{"uri": "./data/gltf/building.gltf"}, {"uri": "./data/gltf/furniture.gltf"}]))
    ]
)
def test_make_root_tile(bbox, kwargs):
    # Arrange
    expected = {
        **{
            "boundingVolume": {"box": bbox} if bbox is not None else {},
            "geometricError": 512,
            "refine": "ADD"
        },
        **kwargs
    }

    # Act
    actual = make_root_tile(bbox, **kwargs)

    # Assert
    assert actual == expected


def test_make_tileset():
    # Arrange
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

    # Act
    actual = make_tileset(root_tile)

    # Assert
    assert expected == actual


@pytest.mark.parametrize(
    "gltf_extreme_coordinates, expected",
    [((0, 0, -1, 1, 2, 0), (0, 0, 0, 1, 1, 2)),
     ((0, 0, -2, 1, 1, 0), (0, 0, 0, 1, 2, 1)),
     ((0, 0, -1, 2, 1, 0), (0, 0, 0, 2, 1, 1)),
     ((0, 2, -1, 1, 4, 0), (0, 0, 2, 1, 1, 4)),
     ((0, 0, -4, 1, 1, -2), (0, 2, 0, 1, 4, 1)),
     ((2, 0, -1, 4, 1, 0), (2, 0, 0, 4, 1, 1))]
)
def test_y_up_to_z_up(gltf_extreme_coordinates, expected):
    # Act
    actual = y_up_to_z_up(*gltf_extreme_coordinates)

    # Assert
    assert actual == expected


@pytest.mark.parametrize(
    "mesh_gen, expected",
    [(C.sample_box_gen, C.sample_box_bbox),
     (C.sample_cone_gen, C.sample_cone_bbox)]
)
def test_compute_bbox_single_mesh(mesh_gen, expected, tmp_path):
    # Arrange
    glb_path = tmp_path / "sample.glb"
    m = mesh_gen()
    m.export(glb_path)

    # Act
    actual = compute_bbox(glb_path)

    # Assert
    assert np.allclose(expected, actual)


def test_compute_bbox_multi_mesh(tmp_path):
    # Arrange
    glb_path1 = tmp_path / "sample1.glb"
    glb_path2 = tmp_path / "sample2.glb"
    m1 = C.sample_box_gen()
    m2 = C.sample_cone_gen()
    m1.export(glb_path1)
    m2.export(glb_path2)

    # Act
    actual = compute_bbox([glb_path1, glb_path2])

    # Assert
    assert np.allclose(C.combined_bbox, actual)


def test_jsonwriter():
    # Arrange
    sample_tileset = {"testkey": "testvalue"}
    sample_name = "jsontest"
    
    # Act
    jsonwriter(sample_tileset, sample_name)

    # Assert
    expected_filepath = os.path.join("data", sample_name + ".json")
    assert os.path.exists(expected_filepath)
    assert read_json(expected_filepath) == sample_tileset


def test_gen_solarpanel_tileset_no_solarpanel():
    """Tests gen_solarpanel_tileset() when there is no solarpanel glTF detected."""
    # Act
    gen_solarpanel_tileset()

    # Assert
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert not os.path.exists(json_filepath)


def test_gen_solarpanel_tileset():
    """Tests gen_solarpanel_tileset() when there is a solarpanel glTF."""
    # Arrange
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    # Act
    gen_solarpanel_tileset()

    # Assert
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {"uri": "./glb/solarpanel.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)


def test_gen_sewagenetwork_tileset_no_sewage():
    """
    Tests gen_sewagenetwork_tileset() when there is no sewagenetwork.gltf detected
    """
    # Execute method
    gen_sewagenetwork_tileset()
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert not os.path.exists(json_filepath)


def test_gen_sewagenetwork_tileset():
    """Tests gen_sewagenetwork_tileset() when there is a sewagenetwork glTF."""
    # Arrange
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    # Act
    gen_sewagenetwork_tileset()

    # Assert
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {"uri": "./glb/sewagenetwork.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_cone_bbox)