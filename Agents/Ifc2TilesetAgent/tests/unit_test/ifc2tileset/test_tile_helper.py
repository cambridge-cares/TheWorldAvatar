"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2tileset.tile_helper submodule.
"""

# Standard library imports
import os

# Third-party import
import numpy as np
import pytest

# Self imports
from agent.ifc2tileset.tile_helper import append_content_metadata_schema, gen_solarpanel_tileset, gen_sewagenetwork_tileset, jsonwriter, compute_bbox, \
    make_tileset, make_root_tile, y_up_to_z_up
from . import testconsts as C
from .testutils import gen_content_metadata, read_json

TEST_BUILDING_IRI = "iri_test"
TEST_BUILDING_NAME = "Building A"


def test_append_content_metadata_schema():
    # Arrange
    root_tile = make_root_tile([])
    tileset = make_tileset(root_tile)

    # Act
    append_content_metadata_schema(tileset)

    # Assert
    assert "asset" in tileset and tileset["asset"] == {"version": "1.1"}
    assert "geometricError" in tileset and tileset["geometricError"] == 1024
    assert "root" in tileset
    assert "schema" in tileset

    root = tileset["root"]
    assert "geometricError" in root and root["geometricError"] == 512
    assert "content" not in root
    assert "contents" not in root

    assert tileset["schema"] == C.expected_content_metadata_schema


@pytest.mark.parametrize(
    "bbox, geometry_file_paths, building_data, kwargs",
    [
        # When there is only a bounding box and nothing else
        (list(range(12)), [], [], dict()),
        # When there is only a building with no name, ensure content is used with metadata attached
        (list(range(12)), ["./data/glb/building.glb"], [TEST_BUILDING_IRI, ""],
         dict(content={"uri": "./data/glb/building.glb", "metadata": gen_content_metadata(TEST_BUILDING_IRI)})),
        # When there is only a building, ensure content is used with metadata attached
        (list(range(12)), ["./data/glb/building.glb"], [TEST_BUILDING_IRI, TEST_BUILDING_NAME],
         dict(content={"uri": "./data/glb/building.glb", "metadata": gen_content_metadata(TEST_BUILDING_IRI, TEST_BUILDING_NAME)})),
        # When there is only a furniture, ensure content is used and no metadata is attached
        (list(range(12)), ["./data/glb/furniture.glb"], [],
         dict(content={"uri": "./data/glb/furniture.glb"})),
        # When there is nothing
        (None, None, [], dict()),
        # When there are multiple contents including building, ensure contents is used
        (None, ["./data/glb/building.glb", "./data/glb/furniture.glb"], [TEST_BUILDING_IRI, TEST_BUILDING_NAME],
         dict(contents=[{"uri": "./data/glb/building.glb", "metadata": gen_content_metadata(TEST_BUILDING_IRI, TEST_BUILDING_NAME)},
                        {"uri": "./data/glb/furniture.glb", "metadata": gen_content_metadata(TEST_BUILDING_IRI, TEST_BUILDING_NAME)}]))
    ]
)
def test_make_root_tile(bbox, geometry_file_paths, building_data, kwargs):
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
    actual = make_root_tile(bbox, geometry_file_paths, building_data)

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
        "geometricError": 1024
    }
    expected["root"] = root_tile

    # Act
    actual = make_tileset(root_tile)

    # Assert
    assert expected == actual


@pytest.mark.parametrize(
    "contents, content_dict",
    [
        # When there is a content containing metadata
        ("content", {"metadata": ""}),
        # When there is contents containing a list of metadata
        ("contents", [{"metadata": ""}]),
    ]
)
def test_make_tileset_has_building_metadata(contents, content_dict):
    # Arrange
    root_tile = {
        "boundingVolume": {"box": []},
        "geometricError": 512,
        "refine": "ADD",
    }
    root_tile[contents] = content_dict
    expected = {
        "asset": {"version": "1.1"},
        "geometricError": 1024
    }
    expected["root"] = root_tile
    # Both test cases should add the schema
    expected["schema"] = C.expected_content_metadata_schema

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


@pytest.mark.parametrize("offset", [0, 5])
def test_compute_bbox_multi_mesh(offset, tmp_path):
    # Arrange
    glb_path1 = tmp_path / "sample1.glb"
    glb_path2 = tmp_path / "sample2.glb"
    m1 = C.sample_box_gen()
    m2 = C.sample_cone_gen()
    m1.export(glb_path1)
    m2.export(glb_path2)

    expected = list(C.combined_bbox)
    expected[4] += offset
    expected[8] += offset

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
    """Tests gen_solarpanel_tileset() when there is no solarpanel geometry output detected."""
    # Act
    gen_solarpanel_tileset([])

    # Assert
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert not os.path.exists(json_filepath)


def test_gen_solarpanel_tileset_with_metadata():
    """Tests gen_solarpanel_tileset() when there is a solarpanel geometry output and metadata inputs."""
    # Arrange
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    # Act
    gen_solarpanel_tileset([C.sample_solar_iri, C.sample_solar_name])

    # Assert
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset
    assert tileset["schema"] == C.expected_content_metadata_schema

    root_tile = tileset["root"]
    assert "content" in root_tile
    assert root_tile["content"] == {"metadata": gen_content_metadata(C.sample_solar_iri, C.sample_solar_name),
                                    "uri": "./glb/solarpanel.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)


def test_gen_solarpanel_tileset():
    """Tests gen_solarpanel_tileset() when there is a solarpanel geometry output."""
    # Arrange
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    # Act
    gen_solarpanel_tileset([])

    # Assert
    json_filepath = os.path.join("data", "tileset_solarpanel.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {
        "uri": "./glb/solarpanel.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)


def test_gen_sewagenetwork_tileset_no_sewage():
    """
    Tests gen_sewagenetwork_tileset() when there is no sewagenetwork geometry output detected
    """
    # Execute method
    gen_sewagenetwork_tileset([])
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert not os.path.exists(json_filepath)


def test_gen_sewagenetwork_tileset_with_metadata():
    """Tests gen_sewagenetwork_tileset() when there is a sewagenetwork geometry output and metadata input."""
    # Arrange
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    # Act
    gen_sewagenetwork_tileset([C.sample_sewage_iri, C.sample_sewage_name])

    # Assert
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset
    assert tileset["schema"] == C.expected_content_metadata_schema

    root_tile = tileset["root"]
    assert "content" in root_tile
    assert root_tile["content"] == {"metadata": gen_content_metadata(C.sample_sewage_iri, C.sample_sewage_name),
                                    "uri": "./glb/sewagenetwork.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_cone_bbox)


def test_gen_sewagenetwork_tileset():
    """Tests gen_sewagenetwork_tileset() when there is a sewagenetwork geometry output."""
    # Arrange
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    # Act
    gen_sewagenetwork_tileset([])

    # Assert
    json_filepath = os.path.join("data", "tileset_sewage.json")
    assert os.path.exists(json_filepath)

    tileset = read_json(json_filepath)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {
        "uri": "./glb/sewagenetwork.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_cone_bbox)
