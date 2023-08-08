"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2tileset submodule.
"""

# Standard library imports
import os

# Third-party imports
import numpy as np
import pandas as pd
import trimesh

# Self imports
from agent.ifc2tileset import gen_tilesets
from . import testconsts as C
from .testutils import read_json, gen_sample_asset_df, gen_sample_asset_contents, z_up_to_y_up


def test_gen_tilesets_solarpanel():
    """Asserts gen_tilesets() for generating only solarpanel tileset."""
    # Arrange
    solarpanel_glb = os.path.join("data", "glb", "solarpanel.glb")
    m = C.sample_box_gen()
    m.export(solarpanel_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    # Act
    gen_tilesets(pd.DataFrame(), "building_iri")

    # Assert
    # Assert that only tileset for solar panel is generated
    assert os.path.exists(solar_json_filepath)
    assert not os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)

    # Test content of tileset
    solar_tileset = read_json(solar_json_filepath)
    assert "root" in solar_tileset

    root_tile = solar_tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {"uri": "./glb/solarpanel.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)


def test_gen_tilesets_sewage():
    """Asserts gen_tilesets() for generating only sewage tileset."""
    # Arrange
    sewage_glb = os.path.join("data", "glb", "sewagenetwork.glb")
    m = C.sample_cone_gen()
    m.export(sewage_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    # Act
    gen_tilesets(pd.DataFrame(), "building_iri")

    # Assert
    # Assert that only sewage tileset is generated
    assert os.path.exists(sewage_json_filepath)
    assert not os.path.exists(bim_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    sewage_tileset = read_json(sewage_json_filepath)
    assert "root" in sewage_tileset

    root_tile = sewage_tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {"uri": "./glb/sewagenetwork.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_cone_bbox)


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
    """Asserts gen_tilesets() for generating only the bim tileset without asset data."""
    # Arrange
    building_glb = os.path.join("data", "glb", "building.glb")
    m = C.sample_box_gen()
    m.export(building_glb)

    bim_json_filepath = os.path.join("data", "tileset_bim.json")
    solar_json_filepath = os.path.join("data", "tileset_solarpanel.json")
    sewage_json_filepath = os.path.join("data", "tileset_sewage.json")

    # Act
    gen_tilesets(pd.DataFrame(), "building_iri")

    # Assert
    # Assert that only bim tileset is generated
    assert os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    tileset = read_json(bim_json_filepath)
    assert "root" in tileset

    root_tile = tileset["root"]
    assert "content" in root_tile and root_tile["content"] == {"uri": "./glb/building.glb"}
    assert "boundingVolume" in root_tile and "box" in root_tile["boundingVolume"] \
        and np.allclose(root_tile["boundingVolume"]["box"], C.sample_box_bbox)
    assert "children" not in root_tile
    assert_tileset_metadata(tileset)


def test_gen_tilesets_3_assets():
    """Asserts gen_tilesets() for generating the bim tileset with only 3 assets."""
    # Arrange
    test_range = 3
    sample_asset_df = gen_sample_asset_df(test_range)

    glb_files = [os.path.join("data", "glb", f"asset{i}.glb") for i in range(test_range)]
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
    gen_tilesets(sample_asset_df, "building_iri")

    # Assert
    # Assert that only bim tileset is generated
    assert os.path.exists(bim_json_filepath)
    assert not os.path.exists(sewage_json_filepath)
    assert not os.path.exists(solar_json_filepath)

    # Test content of tileset
    tileset = read_json(bim_json_filepath)
    assert_tileset_metadata(tileset)

    # Test root tile
    assert "root" in tileset
    root_tile = tileset["root"]
    assert "content" not in root_tile
    assert "contents" not in root_tile

    # Test child node
    assert "children" in root_tile and isinstance(root_tile["children"], list) \
        and len(root_tile["children"]) == 1
    assert root_tile["children"][0] == expected_child_node
