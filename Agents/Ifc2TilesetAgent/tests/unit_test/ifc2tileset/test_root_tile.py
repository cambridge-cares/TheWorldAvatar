"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2tileset.root_tile submodule.
"""

# Standard library imports
import os
from typing import List

# Third-party imports
import pytest
import pandas as pd
import trimesh

# Self imports
from agent.ifc2tileset.root_tile import gen_root_content
from . import testconsts as C
from .testutils import append_tileset_contents, gen_sample_asset_df, z_up_to_y_up

ENDPOINT = "http://www.example.org/sparql"
TEST_BUILDING_IRI = "iri_test"
TEST_BUILDING_NAME = "name here"


def make_bim_tileset(bbox: List[str]):
    return {
        "asset": {"version": "1.1"},
        "geometricError": 1024,
        "root": {
            "boundingVolume": {"box": bbox},
            "geometricError": 512,
            "refine": "ADD",
        }
    }


def test_gen_root_content_no_building_no_furniture_with_assets():
    """Tests gen_root_content() when there are geometry files for assets but not building or furniture."""
    # Arrange
    test_range = 6
    asset_df = gen_sample_asset_df(test_range)

    glb_files = [os.path.join(
        "data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    expected = make_bim_tileset([0, 0, 3,  10, 0, 0, 0, 10, 0, 0, 0, 3])

    # Act
    actual = gen_root_content(asset_df, [], [])

    # Assert
    assert actual == expected


def test_gen_root_content_no_building_no_furniture_no_assets():
    """Tests gen_root_content() when there are no geometry files."""
    # Act
    actual = gen_root_content(
        pd.DataFrame(), [TEST_BUILDING_IRI, TEST_BUILDING_NAME], [])

    # Assert
    assert actual is None


def test_gen_root_content_only_building():
    """Tests gen_root_content() when there is only geometry file for building."""
    # Arrange
    expected = make_bim_tileset(C.sample_box_bbox)
    append_tileset_contents(
        expected, ["./glb/building.glb"], TEST_BUILDING_IRI, TEST_BUILDING_NAME)
    expected["schema"] = C.expected_content_metadata_schema

    # Create sample glb file
    building_glb = os.path.join("data", "glb", "building.glb")
    m = C.sample_box_gen()
    m.export(building_glb)

    # Act
    actual = gen_root_content(
        pd.DataFrame(), [TEST_BUILDING_IRI, TEST_BUILDING_NAME], [])

    # Assert
    assert actual == expected

@pytest.mark.parametrize(
    "root_iri, root_name",
    [("", ""),
     # Check if input parameters will override
     ("iri", "content")]
)
def test_gen_root_content_only_furniture(root_iri, root_name):
    """Tests gen_root_content() when there is only geometry file for furniture."""
    # Arrange
    expected = make_bim_tileset(C.sample_box_bbox) 
    append_tileset_contents(expected, ["./glb/furniture.glb"], root_iri, root_name)
    if root_iri and root_name:
        root_data = [root_iri, root_name]
        expected["schema"] = C.expected_content_metadata_schema
    else:
        root_data =[]

    # Create sample glb file
    furniture_glb = os.path.join("data", "glb", "furniture.glb")
    m = C.sample_box_gen()
    m.export(furniture_glb)

    # Act
    actual = gen_root_content(pd.DataFrame(), [], root_data)

    # Assert
    assert actual == expected

@pytest.mark.parametrize(
    "root_iri, root_name",
    [("", ""),
     # Ensure that input parameters will not override default building content
     ("iri", "content")]
)
def test_gen_root_content_with_building_and_furniture(root_iri, root_name):
    """Tests gen_root_content() when there are geometry files for building and furniture."""
    # Arrange
    expected = make_bim_tileset(C.combined_bbox)
    append_tileset_contents(expected, [
                            "./glb/building.glb", "./glb/furniture.glb"], TEST_BUILDING_IRI, TEST_BUILDING_NAME)
    expected["schema"] = C.expected_content_metadata_schema
    if root_iri and root_name:
            root_data = [root_iri, root_name]
    else:
        root_data =[]

    # Create GLB files for testing
    building_glb = os.path.join("data", "glb", "building.glb")
    furniture_glb = os.path.join("data", "glb", "furniture.glb")

    C.sample_box_gen().export(building_glb)
    C.sample_cone_gen().export(furniture_glb)

    # Act
    actual = gen_root_content(
        pd.DataFrame(), [TEST_BUILDING_IRI, TEST_BUILDING_NAME], root_data)

    # Assert
    assert actual == expected
