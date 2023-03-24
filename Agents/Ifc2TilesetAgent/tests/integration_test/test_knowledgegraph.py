"""
# Author: qhouyee #

An integration test suite for the knowledge graph interactions.
"""
# Standard import
import os

import pandas as pd
from pandas.testing import assert_frame_equal
import pytest

# Self import
from . import testconsts as C
from agent.ifc2gltf.kghelper import retrieve_metadata, get_building_iri
from agent.ifc2gltf import conv2gltf


@pytest.mark.parametrize(
    "select_query, update_query, expected",
    [(C.select_element_query, C.insert_element_query, C.expected_select_element_result)]
)
def test_execute_query(select_query, update_query, expected, initialise_client):
    """
    Tests that the KG client can execute queries and update values with the endpoint
    """
    # Get KG client from fixture
    kg_client = initialise_client

    # Update the test triples into the KG
    kg_client.execute_update(update_query)

    # Query for the triples
    actual = kg_client.execute_query(select_query)

    # Assert if triples have been updated and queried properly
    assert actual == expected


def sort_and_reset_index(df: pd.DataFrame):
    return df.sort_values(by=df.columns.tolist()).reset_index(drop=True)


@pytest.mark.parametrize(
    "init_queries, expected",
    [(
        # when only building structure components are present, returns empty
        [C.insert_bsc_query],
        pd.DataFrame(columns=["iri", "uid", "name", "file"])
    ), (
        # when building structure components and assets are present, returns only assets
        # filename of assets should be appended with an integer incrementing from 1
        [C.insert_bsc_query, C.insert_assets_query],
        pd.DataFrame(data=dict(
            iri=[C.base_namespace + e.iri for e in C.sample_assets],
            uid=[e.ifc_id for e in C.sample_assets],
            name=[e.label for e in C.sample_assets],
            file=[f"asset{i + 1}" for i in range(len(C.sample_assets))]
        ))
    ), (
        # when building structure components, assets, and furniture are present, returns assets and furniture
        [C.insert_bsc_query, C.insert_assets_query, C.insert_furniture_query],
        pd.DataFrame(data=dict(
            iri=[C.base_namespace + e.iri for e in C.sample_assets + C.sample_furniture],
            uid=[e.ifc_id for e in C.sample_assets + C.sample_furniture],
            name=[e.label for e in C.sample_assets + C.sample_furniture],
            file=[f"asset{i + 1}" for i in range(len(C.sample_assets))] +
                 ["furniture" for _ in range(len(C.sample_furniture))]
        ))
    )]
)
def test_retrieve_metadata(init_queries, expected, initialise_client):
    # arrange
    kg_client = initialise_client
    for query in init_queries:
        kg_client.execute_update(query)

    # act
    actual = retrieve_metadata(C.KG_ENDPOINT, C.KG_ENDPOINT)

    # assert
    assert_frame_equal(sort_and_reset_index(actual), sort_and_reset_index(expected), check_dtype=False)


@pytest.mark.parametrize(
    "endpoint, expected",
    [(C.KG_ENDPOINT, C.sample_building_iri)]
)
def test_get_building_iri(endpoint, expected, initialise_client):
    # arrange
    kg_client = initialise_client
    kg_client.execute_update(C.insert_building_query)

    # act
    actual = get_building_iri(endpoint, endpoint)

    # assert
    assert actual == expected


@pytest.mark.parametrize(
    "init_queries, endpoint, expected_assets, expected_building_iri",
    [([C.insert_building_query, C.insert_bsc_query], C.KG_ENDPOINT, ["building"], C.sample_building_iri)]
)
def test_conv2gltf_simple(init_queries, endpoint, expected_assets, expected_building_iri, initialise_client,
                          gen_sample_ifc_file, assert_asset_geometries):
    """
    Tests that the conv2gltf() in agent.ifc2gltf submodule runs and generates only one gltf file
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    for query in init_queries:
        kg_client.execute_update(query)

    # Generate sample ifc files and file paths
    ifcpath = gen_sample_ifc_file("./data/ifc/wall.ifc", False)

    # Execute method to convert a IFC model to gltf
    asset_data, building_iri = conv2gltf(ifcpath, endpoint, endpoint)

    try:
        assert asset_data.empty
        # Assert that the geometry files are generated
        assert_asset_geometries(expected_assets)

        assert building_iri == expected_building_iri
    finally:
        os.remove(ifcpath)


@pytest.mark.parametrize(
    "init_queries, endpoint, expected_assets, expected_building_iri",
    [(
        [C.insert_building_query, C.insert_assets_query, C.insert_furniture_query, C.insert_solar_panel_query],
        C.KG_ENDPOINT, ["building", "asset1", "asset2", "furniture", "solarpanel"], C.sample_building_iri
    )]
)
def test_conv2gltf_complex(init_queries, endpoint, expected_assets, expected_building_iri, initialise_client,
                           gen_sample_ifc_file, assert_asset_geometries):
    """
    Tests that the conv2gltf() in agent.ifc2gltf submodule runs and generates required geometry file
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    for query in init_queries:
        print("")
        kg_client.execute_update(query)

    # Generate sample ifc files and file paths
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc", True)

    expected_asset_data = pd.DataFrame(data=dict(
        iri=[C.base_namespace + e.iri for e in C.sample_assets],
        uid=[e.ifc_id for e in C.sample_assets],
        name=[e.label for e in C.sample_assets],
        file=[f"asset{i + 1}" for i in range(len(C.sample_assets))]
    ))

    # Execute method to convert a IFC model to gltf
    actual_asset_data, actual_building_iri = conv2gltf(ifcpath, endpoint, endpoint)

    try:
        # Assert that asset data is precisely the assets without furniture, solar panel, sewage network
        assert_frame_equal(sort_and_reset_index(actual_asset_data), sort_and_reset_index(expected_asset_data))

        # Assert that the geometry files are generated
        assert_asset_geometries(expected_assets)

        assert actual_building_iri == expected_building_iri
    finally:
        os.remove(ifcpath)
