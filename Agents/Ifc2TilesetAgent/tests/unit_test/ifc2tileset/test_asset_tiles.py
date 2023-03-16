"""
# Author: qhouyee #

A test suite for the agent.ifc2tileset.asset_tiles submodule.
"""
from typing import List

# Self import
import agent.config.config as properties
from agent.ifc2tileset.asset_tiles import append_asset_metadata_schema, gen_tileset_assets, append_assets
from agent.ifc2tileset.root_tile import make_tileset, append_tileset_schema_and_metadata
from agent.ifc2tileset.schema import Tile, Content
from agent.ifc2tileset.tile_helper import make_root_tile
from tests.unit_test.ifc2tileset.testutils import gen_sample_asset_df, gen_sample_tileset


def test_append_asset_metadata_schema():
    """
    Tests init_asset_tiles()
    """
    # Initialise test parameters
    properties.bbox_child = []
    building_iri = "buildingIri"

    # Init root tile for input
    root_tile = make_root_tile(properties.bbox_root)
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


def gen_nested_dicts_for_test(sample_tileset: Tile, test_dict_list: List[Content]):
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
                assert nested_dict["contents"][i]["uri"] == "./gltf/asset" + str(asset_no) + ".gltf"
                assert metadata["class"] == "AssetMetaData"
                assert metadata["properties"]["name"] == "element" + str(asset_no)
                assert metadata["properties"]["uid"] == "uid" + str(asset_no)
                assert metadata["properties"]["iri"] == "iri" + str(asset_no)

            else:
                assert nested_dict["contents"][i]["uri"] == "./gltf/asset" + str(asset_no) + ".gltf"
        # 6 assets should be cleared for each nested dict
        current_asset_num = current_asset_num + 6


def test_append_assets_less_than_six_assets():
    """
    Tests append_assets() for less than 6 assets
    """
    # Generate sample parameters
    test_range = 4

    sample_asset_df = gen_sample_asset_df(test_range)
    sample_tileset = gen_sample_tileset()

    # Execute method
    append_assets(sample_tileset, sample_asset_df)

    # Test that there is no nested children key
    assert "children" in sample_tileset["root"]
    assert "children" not in sample_tileset["root"]["children"][0]


def test_append_assets_more_than_six_assets():
    """
    Tests append_assets() for more than 6 assets
    """
    # Generate sample parameters
    test_range = 14

    sample_asset_df = gen_sample_asset_df(test_range)
    sample_tileset = gen_sample_tileset()

    # Execute method
    append_assets(sample_tileset, sample_asset_df)

    # Generate list of nested dict
    test_dict_list = []
    gen_nested_dicts_for_test(sample_tileset["root"], test_dict_list)

    # Test that there is no children in deepest layer
    assert "children" not in test_dict_list[-1]

    # Test the nested children nodes are attached with assets
    assert_nested_node_content(test_dict_list, test_range, 0)


def test_gen_tileset_assets():
    """
    Tests gen_tileset_assets()
    """
    # Generate sample parameters
    test_range = 8
    sample_df = gen_sample_asset_df(test_range)
    tileset = gen_sample_tileset()

    # Execute test method
    gen_tileset_assets(tileset, sample_df)

    # Process result for programmatic testing
    test_dict_list = []
    gen_nested_dicts_for_test(tileset["root"], test_dict_list)

    # Test the nested children nodes are attached with assets
    assert_nested_node_content(test_dict_list, test_range, 0)
