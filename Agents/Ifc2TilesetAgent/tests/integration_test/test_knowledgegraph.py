"""
# Author: qhouyee #

An integration test suite for the knowledge graph interactions.
"""
# Standard import
import os

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


@pytest.mark.parametrize(
    "update_query, endpoint",
    [(C.insert_wall_query, C.KG_ENDPOINT)]
)
def test_retrieve_metadata_none(update_query, endpoint, initialise_client):
    """
    Tests that agent.ifc2gltf.kghelper does not retrieve metadata from non-asset types
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(update_query)

    # Execute method to retrieve metadata
    result = retrieve_metadata(endpoint, endpoint)

    # Assert that there is no result in the dataframe
    assert result.empty


@pytest.mark.parametrize(
    "update_query, endpoint",
    [(C.insert_assets_query, C.KG_ENDPOINT)]
)
def test_retrieve_metadata(update_query, endpoint, initialise_client):
    """
    Tests that agent.ifc2gltf.kghelper retrieves and classifies the metadata of assets accurately
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(update_query)

    # Execute method to retrieve metadata
    result = retrieve_metadata(endpoint, endpoint)

    # Assert if the row correspond with the right classification
    # First retrieve the row under conditions. If any row exist and is true, return true
    assert result.loc[(result["name"] == "Electric Wire Box") & (result["file"] == "furniture")].any().all()
    assert result.loc[(result["name"] == "Water Meter") & (result["file"] == "asset1")].any().all()
    assert result.loc[(result["name"] == "Solar Panel") & (result["file"] == "solarpanel")].any().all()


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
    "update_query, endpoint, expected_assets, expected_building_iri",
    [(C.insert_wall_query, C.KG_ENDPOINT, C.expected_assets1, C.sample_building_iri)]
)
def test_conv2gltf_simple(update_query, endpoint, expected_assets, expected_building_iri, initialise_client,
                          gen_sample_ifc_file, assert_asset_geometries):
    """
    Tests that the conv2gltf() in agent.ifc2gltf submodule runs and generates only one gltf file
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(update_query)
    kg_client.execute_update(C.insert_building_query)

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
    "update_query, endpoint, expected_assets, expected_building_iri",
    [(C.insert_assets_query, C.KG_ENDPOINT, C.expected_assets2, C.sample_building_iri)]
)
def test_conv2gltf_complex(update_query, endpoint, expected_assets, expected_building_iri, initialise_client,
                           gen_sample_ifc_file, assert_asset_geometries):
    """
    Tests that the conv2gltf() in agent.ifc2gltf submodule runs and generates required geometry file
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(update_query)
    kg_client.execute_update(C.insert_building_query)

    # Generate sample ifc files and file paths
    ifcpath = gen_sample_ifc_file("./data/ifc/sample.ifc", True)

    # Execute method to convert a IFC model to gltf
    asset_data, building_iri = conv2gltf(ifcpath, endpoint, endpoint)

    try:
        # Assert that there is only 1 result row returned for asset1
        assert len(asset_data) == 1
        # Assert the row is as follows
        assert asset_data.loc[(asset_data["name"] == "Water Meter") & (asset_data["file"] == "asset1")].any().all()
        # Assert that the geometry files are generated
        assert_asset_geometries(expected_assets)

        assert building_iri == expected_building_iri
    finally:
        os.remove(ifcpath)
