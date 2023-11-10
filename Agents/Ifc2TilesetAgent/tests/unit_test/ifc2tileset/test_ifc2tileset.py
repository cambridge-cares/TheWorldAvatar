"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2tileset submodule.
"""

# Standard library imports
import os

# Third-party imports
import pytest
import numpy as np
import pandas as pd
import trimesh

# Self imports
from agent.ifc2tileset import gen_tilesets
from . import testconsts as C
from .testutils import read_json, gen_content_metadata, gen_sample_asset_df, gen_sample_asset_contents, z_up_to_y_up

TEST_BUILDING_IRI = "iri_test"
TEST_BUILDING_NAME = "Building A"
TEST_ROOT_IRI = "http://www.example.org/sample/Test_21471"
TEST_ROOT_NAME = "Shelter"


@pytest.mark.parametrize(
    ("root_iri", "root_name", "solar_iri", "solar_name"),
    [("", "", "", ""),
     # Ensure that input parameters will not override in solar panel tileset
     (TEST_ROOT_IRI, TEST_ROOT_NAME, "", ""),
     # Verify that solar iri and name are generated
     ("", "", C.sample_solar_iri, C.sample_solar_name),
     (TEST_ROOT_IRI, TEST_ROOT_NAME, C.sample_solar_iri, C.sample_solar_name)]
)
def test_gen_tilesets_solarpanel(root_iri, root_name, solar_iri, solar_name):
    """Asserts gen_tilesets() for generating only solarpanel tileset."""
    # Arrange
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    if root_iri and root_name:
        root_data = [root_iri, root_name]
    else:
        root_data = []

    if solar_iri and solar_name:
        solar_data = [solar_iri, solar_name]
    else:
        solar_data = []

    # Act
    gen_tilesets(pd.DataFrame(), [], solar_data, [], root_data)

    # Assert
    # Assert that only tileset for solar panel is generated
    assert os.path.exists(solar_json_filepath)
    assert not os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)

    # Test content of tileset
    solar_tileset = read_json(solar_json_filepath)
    assert "root" in solar_tileset

    root_tile = solar_tileset["root"]
    assert "content" in root_tile
    if solar_iri and solar_name:
        assert solar_tileset["schema"] == C.expected_content_metadata_schema
        assert root_tile["content"] == {"metadata": gen_content_metadata(C.sample_solar_iri, C.sample_solar_name),
                                        "uri": "./glb/solarpanel.glb"}
    else:
        assert "schema" not in solar_tileset
        assert root_tile["content"] == {
            "uri": "./glb/solarpanel.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)


@pytest.mark.parametrize(
    ("root_iri", "root_name", "sewage_iri", "sewage_name"),
    [("", "", "", ""),
     # Ensure that input parameters will not override in sewage tileset
     (TEST_ROOT_IRI, TEST_ROOT_NAME, "", ""),
     # Verify that solar iri and name are generated
     ("", "", C.sample_sewage_iri, C.sample_sewage_name),
     (TEST_ROOT_IRI, TEST_ROOT_NAME, C.sample_sewage_iri, C.sample_sewage_name)]
)
def test_gen_tilesets_sewage(root_iri, root_name, sewage_iri, sewage_name):
    """Asserts gen_tilesets() for generating only sewage tileset."""
    # Arrange
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    if root_iri and root_name:
        root_data = [root_iri, root_name]
    else:
        root_data = []

    if sewage_iri and sewage_name:
        sewage_data = [sewage_iri, sewage_name]
    else:
        sewage_data = []

    # Act
    gen_tilesets(pd.DataFrame(), [], [], sewage_data, root_data)

    # Assert
    # Assert that only sewage tileset is generated
    assert os.path.exists(sewage_json_filepath)
    assert not os.path.exists(bim_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    sewage_tileset = read_json(sewage_json_filepath)
    assert "root" in sewage_tileset

    root_tile = sewage_tileset["root"]
    assert "content" in root_tile
    if sewage_iri and sewage_name:
        assert sewage_tileset["schema"] == C.expected_content_metadata_schema
        assert root_tile["content"] == {"metadata": gen_content_metadata(C.sample_sewage_iri, C.sample_sewage_name),
                                        "uri": "./glb/sewagenetwork.glb"}
    else:
        assert "schema" not in sewage_tileset
        assert root_tile["content"] == {
            "uri": "./glb/sewagenetwork.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_cone_bbox)


@pytest.mark.parametrize(
    "geometry, root_iri, root_name, expected_iri, expected_name",
    [("building.glb", "", "", TEST_BUILDING_IRI, TEST_BUILDING_NAME),
     ("building.glb", TEST_ROOT_IRI, TEST_ROOT_NAME,
      TEST_BUILDING_IRI, TEST_BUILDING_NAME),
     # Ensure that input parameters will not override in sewage tileset
     ("furniture.glb", "", "", "", ""),
     ("furniture.glb", TEST_ROOT_IRI, TEST_ROOT_NAME, TEST_ROOT_IRI, TEST_ROOT_NAME)
     ]
)
def test_gen_tilesets_bim_no_assets(geometry, root_iri, root_name, expected_iri, expected_name):
    """Asserts gen_tilesets() for generating only the bim tileset without asset data including building or furniture."""
    # Arrange
    building_glb = os.path.join("data", "glb", geometry)
    m = C.sample_box_gen()
    m.export(building_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    if root_iri and root_name:
        root_data = [root_iri, root_name]
    else:
        root_data = []
    expected_content = {"uri": "./glb/" + geometry}

    # Act
    gen_tilesets(pd.DataFrame(), [TEST_BUILDING_IRI,
                 TEST_BUILDING_NAME], [], [], root_data)

    # Assert
    # Assert that only bim tileset is generated
    assert os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    tileset = read_json(bim_json_filepath)
    # When we expect metadata
    if expected_iri and expected_name:
        # verify that schema exists
        assert tileset["schema"] == C.expected_content_metadata_schema
        # add the metadata to the expected content
        expected_content["metadata"] = gen_content_metadata(
            expected_iri, expected_name)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == expected_content
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)
    assert "children" not in root_tile


def test_gen_tilesets_3_assets():
    """Asserts gen_tilesets() for generating the bim tileset with only 3 assets."""
    # Arrange
    test_range = 3
    sample_asset_df = gen_sample_asset_df(test_range)

    glb_files = [os.path.join(
        "data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    expected_child_node = {
        "boundingVolume": {"box": [0, 0, 1.5, 15, 0, 0, 0, 15, 0, 0, 0, 1.5]},
        "geometricError": 50,
        "contents": gen_sample_asset_contents(test_range)
    }

    # Act
    gen_tilesets(sample_asset_df, [], [], [])

    # Assert
    # Assert that only bim tileset is generated
    assert os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    tileset = read_json(bim_json_filepath)
    # Test schema exists
    assert tileset["schema"] == C.expected_content_metadata_schema

    # Test root tile
    assert "root" in tileset
    root_tile = tileset["root"]
    assert "content" not in root_tile
    assert "contents" not in root_tile

    # Test child node
    assert "children" in root_tile and isinstance(root_tile["children"], list) \
        and len(root_tile["children"]) == 1
    assert root_tile["children"][0] == expected_child_node
