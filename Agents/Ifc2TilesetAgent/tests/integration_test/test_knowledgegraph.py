"""
# Author: qhouyee, picas9dan #

An integration test suite for the knowledge graph interactions.
"""

# Third-party imports
import pandas as pd
import pytest

# Self imports
from . import testconsts as C
from agent.ifc2gltf.kghelper import retrieve_metadata, get_building_iri_name
from agent.ifc2gltf import conv2gltf
from .testutils import init_kg_client, assert_df_equal, assert_assets_present


@pytest.mark.parametrize(
    "select_query, update_query, expected",
    [(C.select_element_query, C.insert_element_query, C.expected_select_element_result)]
)
def test_execute_query(select_query, update_query, expected, kg_client):
    # Arrange
    kg_client.execute_update(update_query)

    # Act
    actual = kg_client.execute_query(select_query)

    # Assert
    assert actual == expected


@pytest.mark.parametrize(
    "init_assets, expected",
    [(
        # when only wall is present, returns empty
        ["wall"],
        pd.DataFrame(columns=["iri", "uid", "name", "file"])
    ), (
        # when wall and assets are present, returns only assets
        # filename of assets should be appended with an integer incrementing from 1
        ["wall", "water_meter", "fridge"],
        pd.DataFrame(data=dict(
            iri=[C.base_namespace + e.iri for e in (C.sample_water_meter, C.sample_fridge)],
            uid=[e.ifc_id for e in (C.sample_water_meter, C.sample_fridge)],
            name=[e.label for e in (C.sample_water_meter, C.sample_fridge)],
            file=[f"asset{i + 1}" for i in range(2)]
        ))
    ), (
        # when wall, assets, and furniture are present, returns assets and furniture
        ["wall", "chair", "table", "water_meter", "fridge"],
        pd.DataFrame(data=dict(
            iri=[C.base_namespace + e.iri
                 for e in (C.sample_water_meter, C.sample_fridge, C.sample_chair, C.sample_table)],
            uid=[e.ifc_id for e in (C.sample_water_meter, C.sample_fridge, C.sample_chair, C.sample_table)],
            name=[e.label for e in (C.sample_water_meter, C.sample_fridge, C.sample_chair, C.sample_table)],
            file=[f"asset{i + 1}" for i in range(2)] +
                 ["furniture" for _ in range(2)]
        ))
    )]
)
def test_retrieve_metadata(init_assets, expected, kg_client):
    # Arrange
    init_kg_client(kg_client, init_assets)

    # Act
    actual = retrieve_metadata(C.KG_ENDPOINT, C.KG_ENDPOINT)

    # Assert
    assert_df_equal(actual, expected)


@pytest.mark.parametrize(
    "init_assets, expected_building_iri, expected_building_name",
    [(
        # when building exists, ensure that an iri and name can be retrieved
        ["building"],
        C.SAMPLE_BUILDING_IRI,
        C.SAMPLE_BUILDING_NAME
    ), (
        # when building exists but no name is available, ensure that only an iri is retrieved
        ["nameless_building"],
        C.SAMPLE_BUILDING_IRI,
        ""
    ), (
        # when there is no building, no values should be returned
        [],
        "",
        ""
    )]
)
def test_get_building_iri_name(init_assets, expected_building_iri, expected_building_name, endpoint, kg_client):
    # Arrange
    init_kg_client(kg_client, init_assets)

    # Act
    actual = get_building_iri_name(endpoint, endpoint)

    # Assert
    assert actual[0] == expected_building_iri
    assert actual[1] == expected_building_name


@pytest.mark.parametrize(
    "init_assets, expected_gltf, expected_asset_data, expected_building_iri, expected_building_name",
    [(
        # when only building and wall are present, building gltf should be generated and asset data should be empty
        ["building", "wall"],
        ["building"],
        pd.DataFrame(columns=["iri", "uid", "name", "file"]),
        C.SAMPLE_BUILDING_IRI,
        C.SAMPLE_BUILDING_NAME
    ), (
        # when only building, wall, and furniture are present, building and furniture gltf should be generated and
        # asset data should be empty
        ["building", "wall", "chair", "table"],
        ["building", "furniture"],
        pd.DataFrame(columns=["iri", "uid", "name", "file"]),
        C.SAMPLE_BUILDING_IRI,
        C.SAMPLE_BUILDING_NAME
    ), (
        # when assets, furniture, solar panel are present, gltf files should be generated and asset data present
        ["building", "wall", "water_meter", "fridge", "chair", "table", "solar_panel"],
        ["building", "asset1", "asset2", "furniture", "solarpanel"],
        pd.DataFrame(data=dict(
            iri=[C.base_namespace + e.iri for e in (C.sample_water_meter, C.sample_fridge)],
            uid=[e.ifc_id for e in (C.sample_water_meter, C.sample_fridge)],
            name=[e.label for e in (C.sample_water_meter, C.sample_fridge)],
            file=[f"asset{i + 1}" for i in range(2)]
        )),
        C.SAMPLE_BUILDING_IRI,
        C.SAMPLE_BUILDING_NAME
    )]
)
def test_conv2gltf(init_assets, expected_gltf, expected_asset_data, expected_building_iri, expected_building_name,
                   endpoint, kg_client, gen_sample_ifc_file):
    # Arrange
    init_kg_client(kg_client, init_assets)
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc", assets=init_assets)

    # Act
    actual_asset_data, actual_building_data = conv2gltf(ifcpath, endpoint, endpoint)

    # Assert
    assert_df_equal(actual_asset_data, expected_asset_data)
    assert_assets_present(expected_gltf)
    assert actual_building_data[0] == expected_building_iri
    assert actual_building_data[1] == expected_building_name
