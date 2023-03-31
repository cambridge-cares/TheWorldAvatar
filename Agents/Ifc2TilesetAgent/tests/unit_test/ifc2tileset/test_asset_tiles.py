"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.asset_tiles submodule.
"""

# Standard import
import os
from typing import List

# Third-party import
import trimesh

# Self import
from agent.ifc2tileset.asset_tiles import append_asset_metadata_schema, append_tileset_assets, append_assets_to_tileset, \
    append_assets_to_tile
from agent.ifc2tileset.root_tile import make_tileset, append_tileset_schema_and_metadata
from agent.ifc2tileset.schema import Tile
from agent.ifc2tileset.tile_helper import make_root_tile
from tests.unit_test.ifc2tileset.testutils import gen_sample_asset_df, gen_sample_tileset, gen_sample_asset_contents, \
    z_up_to_y_up


def test_append_asset_metadata_schema():
    """
    Tests init_asset_tiles()
    """
    # Initialise test parameters
    building_iri = "buildingIri"

    # Init root tile for input
    root_tile = make_root_tile([])
    tileset = make_tileset(root_tile)
    append_tileset_schema_and_metadata(tileset, building_iri)

    # Execute method
    append_asset_metadata_schema(tileset)

    # Test that root tile is generated with no glTF content
    assert tileset["asset"]["version"] == "1.1"
    assert tileset["geometricError"] == 1024

    assert tileset["root"]["geometricError"] == 512
    assert "content" not in tileset["root"]
    assert "contents" not in tileset["root"]

    # Test schema key is generated
    metadata = tileset["schema"]["classes"]["AssetMetaData"]
    assert metadata["name"] == "Asset metadata"
    assert metadata["properties"]["name"]["description"] == "Name of the asset"
    assert metadata["properties"]["name"]["type"] == "STRING"
    assert metadata["properties"]["uid"]["description"] == "Unique identifier generated in IFC"
    assert metadata["properties"]["uid"]["type"] == "STRING"
    assert metadata["properties"]["iri"]["description"] == "Data IRI of the asset"
    assert metadata["properties"]["iri"]["type"] == "STRING"


def test_append_assets_to_tile_node():
    # arrange
    tile = {
        "boundingVolume": {"box": []},
        "geometricError": 50
    }
    test_range = 6
    asset_df = gen_sample_asset_df(test_range)

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

    # act
    append_assets_to_tile(tile, asset_df)

    # assert
    assert "children" in tile
    assert len(tile["children"]) == 1
    assert tile["children"][0] == expected_child_node


def gen_nested_dicts_for_test(sample_tileset: Tile, test_dict_list: List[Tile]):
    """
    A test function to recursively retrieve and append
    nested dictionary['children'][0] to a list for the tests of appenddict_rec()

    Argument:
    sample_tileset - initial dictionary to start parsing,
            ideally one depth above starting children node
    test_dict_list - the list to append nested dictionary to
    """
    # If there are still children nodes, continue the recursion
    if "children" in sample_tileset:
        nested_dict = sample_tileset["children"][0]
        test_dict_list.append(nested_dict)
        gen_nested_dicts_for_test(nested_dict, test_dict_list)
    else:
        return


# TODO: incorporate the assertions for bbox into this function
def assert_nested_node_content(nested_node_list: List[dict], test_range: int, current_asset_num: int = 6):
    """
    A test function to assert nested contents

    Arguments:
    nested_node_list - a list containing the dictionary's nested node by depth
    test_range - number of assets generated for testing
    current_asset_num - starts the assertion from this number. \
            Only accepts 0 or 6, which provide tests for \
            gen_tileset_assets() or appenddict_rec() respectively
    """
    # Assign test cases according to input
    if current_asset_num == 6:
        test_case = 1  # Test case for appenddict_rec()
    elif current_asset_num == 0:
        test_case = 0  # Test case for gen_tileset_assets()
    else:
        raise ValueError(
            "Invalid current_asset_num value. Only 0 or 6 is accepted!" +
            " Please read function description for more details")

    for nested_dict in nested_node_list:
        # Test these key, values are present
        assert "boundingVolume" in nested_dict
        assert nested_dict["geometricError"] == 50
        # Max content in one children node should be 6 unless test_range is not divisible by 6
        max_range = current_asset_num + 6 \
            if test_range - current_asset_num > 6 \
            else test_range
        i = -1  # initialise list index in 'contents' node
        # Test that the assets' contents are properly appended
        for asset_no in range(current_asset_num, max_range):
            i = -1 if i > 4 else i + 1
            if test_case == 0:
                metadata = nested_dict["contents"][i]["metadata"]
                assert nested_dict["contents"][i]["uri"] == "./glb/asset" + str(asset_no) + ".glb"
                assert metadata["class"] == "AssetMetaData"
                assert metadata["properties"]["name"] == "element" + str(asset_no)
                assert metadata["properties"]["uid"] == "uid" + str(asset_no)
                assert metadata["properties"]["iri"] == "iri" + str(asset_no)

            else:
                assert nested_dict["contents"][i]["uri"] == "./glb/asset" + str(asset_no) + ".glb"
        # 6 assets should be cleared for each nested dict
        current_asset_num = current_asset_num + 6


def test_append_assets_less_than_six_assets():
    """
    Tests append_assets() for less than 6 assets
    """
    # Generate sample parameters
    test_range = 4
    sample_asset_df = gen_sample_asset_df(test_range)

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

    tileset = gen_sample_tileset()

    # Execute method
    append_assets_to_tileset(tileset, sample_asset_df)

    # Test that the root tile has one child node
    root_tile = tileset["root"]
    assert "children" in root_tile
    assert len(root_tile["children"]) == 1
    assert root_tile["children"][0] == expected_child_node

    # Test that there is no nested children key
    assert "children" not in tileset["root"]["children"][0]


def test_append_assets_more_than_six_assets():
    """
    Tests append_assets() for more than 6 assets
    """
    # Generate sample parameters
    test_range = 14

    sample_asset_df = gen_sample_asset_df(test_range)

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

    tileset = gen_sample_tileset()

    # Execute method
    append_assets_to_tileset(tileset, sample_asset_df)

    # Generate list of nested dict
    test_dict_list = []
    gen_nested_dicts_for_test(tileset["root"], test_dict_list)

    # Test that there is no children in deepest layer
    assert "children" not in test_dict_list[-1]

    # Test the nested children nodes are attached with assets
    assert_nested_node_content(test_dict_list, test_range, 0)

    for i, tile in enumerate(test_dict_list):
        assert expected_fields_of_child_nodes[i].items() <= tile.items()


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

    tileset = gen_sample_tileset()

    # Execute test method
    append_tileset_assets(tileset, sample_df)

    # Process result for programmatic testing
    test_dict_list = []
    gen_nested_dicts_for_test(tileset["root"], test_dict_list)

    # Test the nested children nodes are attached with assets
    assert_nested_node_content(test_dict_list, test_range, 0)
    for i, tile in enumerate(test_dict_list):
        assert expected_fields_of_child_nodes[i].items() <= tile.items()
