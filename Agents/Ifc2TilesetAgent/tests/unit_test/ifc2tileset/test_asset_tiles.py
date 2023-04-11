"""
# Author: qhouyee, picas9dan #

A test suite for the agent.ifc2tileset.asset_tiles submodule.
"""

# Standard library imports
import os

# Third-party imports
import trimesh

# Self imports
from agent.ifc2tileset.asset_tiles import append_asset_metadata_schema, append_tileset_assets, append_assets_to_tileset, \
    append_assets_to_tile
from agent.ifc2tileset.root_tile import make_tileset, append_tileset_schema_and_metadata
from agent.ifc2tileset.tile_helper import make_root_tile
from tests.unit_test.ifc2tileset.testutils import gen_sample_asset_df, gen_sample_tileset, gen_sample_asset_contents, \
    z_up_to_y_up


def test_append_asset_metadata_schema():
    # Arrange
    root_tile = make_root_tile([])
    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, "buildingIri")

    expected_asset_metadata_schema = {
        "description": "A metadata class for all individual assets",
        "name": "Asset metadata",
        "properties": {
            "name": {
                "description": "Name of the asset",
                "type": "STRING"
            },
            "uid": {
                "description": "Unique identifier generated in IFC",
                "type": "STRING"
            },
            "iri": {
                "description": "Data IRI of the asset",
                "type": "STRING"
            }
        }
    }

    # Act
    append_asset_metadata_schema(tileset)

    # Assert
    assert "asset" in tileset and tileset["asset"] == {"version": "1.1"}
    assert "geometricError" in tileset and tileset["geometricError"] == 1024
    assert "root" in tileset
    assert "schema" in tileset

    root = tileset["root"]
    assert "geometricError" in root and root["geometricError"] == 512
    assert "content" not in root
    assert "contents" not in root

    schema = tileset["schema"]
    assert "classes" in schema and "AssetMetaData" in schema["classes"] \
           and schema["classes"]["AssetMetaData"] == expected_asset_metadata_schema


def test_append_assets_to_tile_node():
    # Arrange
    tile = {
        "boundingVolume": {"box": []},
        "geometricError": 50
    }
    test_range = 6
    asset_df = gen_sample_asset_df(test_range)

    # Create sample asset files
    glb_files = [os.path.join("data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    expected_child_node = {
        "boundingVolume": {"box": [0, 0, 3,  10, 0, 0, 0, 10, 0, 0, 0, 3]},
        "geometricError": 50,
        "contents": gen_sample_asset_contents(test_range)
    }

    # Act
    append_assets_to_tile(tile, asset_df)

    # Assert
    assert "children" in tile
    assert len(tile["children"]) == 1
    assert tile["children"][0] == expected_child_node


def flatten_child_nodes(tile: dict):
    """Flattens the child node hierarchy of a tile."""
    child_nodes = []
    while "children" in tile and isinstance(tile["children"], list) and len(tile["children"]) > 0:
        child_node = tile["children"][0]
        child_nodes.append(child_node)
        tile = child_node
    return child_nodes


def test_append_assets_less_than_six_assets():
    # Arrange
    test_range = 4
    sample_asset_df = gen_sample_asset_df(test_range)

    # Create sample asset files
    glb_files = [os.path.join("data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    expected_child_node = {
        "boundingVolume": {"box": [0, 0, 2, 10, 0, 0, 0, 10, 0, 0, 0, 2]},
        "geometricError": 50,
        "contents": gen_sample_asset_contents(test_range)
    }

    # Act
    tileset = gen_sample_tileset()
    append_assets_to_tileset(tileset, sample_asset_df)

    # Assert
    # Test that the root tile has one child node
    assert "root" in tileset
    root_tile = tileset["root"]
    assert "children" in root_tile
    assert len(root_tile["children"]) == 1
    assert root_tile["children"][0] == expected_child_node


def test_append_assets_more_than_six_assets():
    # Arrange
    test_range = 14
    sample_asset_df = gen_sample_asset_df(test_range)

    # Create sample asset files
    glb_files = [os.path.join("data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    asset_contents = gen_sample_asset_contents(test_range)
    expected_fields_of_child_nodes = [
        {
            "boundingVolume": {"box": [0, 0, 3, 10, 0, 0, 0, 10, 0, 0, 0, 3]},
            "contents": asset_contents[: 6]
        },
        {
            "boundingVolume": {"box": [0, 0, 9, 10, 0, 0, 0, 10, 0, 0, 0, 3]},
            "contents": asset_contents[6: 12]
        },
        {
            "boundingVolume": {"box": [0, 0, 13, 10, 0, 0, 0, 10, 0, 0, 0, 1]},
            "contents": asset_contents[12:]
        }
    ]

    # Act
    tileset = gen_sample_tileset()
    append_assets_to_tileset(tileset, sample_asset_df)

    # Assert
    assert "root" in tileset
    child_nodes = flatten_child_nodes(tileset["root"])
    for i, (child_node, expected_fields) in enumerate(zip(child_nodes, expected_fields_of_child_nodes)):
        assert "geometricError" in child_node and child_node["geometricError"] == 50
        if i < len(child_nodes) - 1:
            assert "children" in child_node and isinstance(child_node["children"], list) \
                and len(child_node["children"]) == 1
        else:
            assert "children" not in child_node
        assert expected_fields.items() <= child_node.items()


def test_append_tileset_assets():
    """
    Tests gen_tileset_assets()
    """
    # Generate sample parameters
    test_range = 8
    sample_df = gen_sample_asset_df(test_range)

    glb_files = [os.path.join("data", "glb", f"asset{i}.glb") for i in range(test_range)]
    for i, file in enumerate(glb_files):
        z_up_coords = -10, -10, i, 10, 10, i + 1
        y_up_coords = z_up_to_y_up(*z_up_coords)
        m = trimesh.creation.box(bounds=[y_up_coords[:3], y_up_coords[3:]])
        m.export(file)

    asset_contents = gen_sample_asset_contents(test_range)
    expected_fields_of_child_nodes = [
        {
            "boundingVolume": {"box": [0, 0, 3, 10, 0, 0, 0, 10, 0, 0, 0, 3]},
            "contents": asset_contents[: 6]
        },
        {
            "boundingVolume": {"box": [0, 0, 7, 10, 0, 0, 0, 10, 0, 0, 0, 1]},
            "contents": asset_contents[6:]
        }
    ]

    # Act
    tileset = gen_sample_tileset()
    append_tileset_assets(tileset, sample_df)

    # Assert
    assert "root" in tileset
    child_nodes = flatten_child_nodes(tileset["root"])
    assert "children" not in child_nodes[-1]
    for i, (child_node, expected_fields) in enumerate(zip(child_nodes, expected_fields_of_child_nodes)):
        assert "geometricError" in child_node and child_node["geometricError"] == 50
        if i < len(child_nodes) - 1:
            assert "children" in child_node and isinstance(child_node["children"], list) \
                and len(child_node["children"]) == 1
        else:
            assert "children" not in child_node
        assert expected_fields.items() <= child_node.items()
