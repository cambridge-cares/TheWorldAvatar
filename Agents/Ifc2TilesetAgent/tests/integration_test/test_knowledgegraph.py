"""
# Author: qhouyee #

An integration test suite for the knowledge graph interactions.
"""
# Third party import
import pytest
import pandas as pd

# Self import
from . import testconsts
from agent.ifc2gltf.kghelper import retrieve_metadata

@pytest.mark.parametrize(
    "selectquery, updatequery, expected",
    [
        (testconsts.selectquery1, testconsts.insertquery1, testconsts.expected1),
    ]
)
def test_execute_query(selectquery, updatequery, expected, initialise_client):
    """
    Tests that the KG client can execute queries and update values with the endpoint
    """
    # Get KG client from fixture
    kg_client = initialise_client
    # Update the test triples into the KG
    kg_client.execute_update(updatequery)
    # Query for the triples
    result = kg_client.execute_query(selectquery)
    # Assert if triples have been updated and queried properly
    assert len(result) ==1
    assert expected == result[0]

@pytest.mark.parametrize(
    "updatequery, endpoints",
    [
        (testconsts.insertquery2, testconsts.KG_ENDPOINT),
    ]
)
def test_retrieve_metadata_none(updatequery, endpoints, initialise_client):
    """
    Tests that agent.ifc2gltf.kghelper does not retrieve metadata from non-asset types
    """
    # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(updatequery)
    # Execute method to retrieve metadata
    result = retrieve_metadata(endpoints, endpoints)
    # Assert that there is no result in the dataframe
    assert result.empty

@pytest.mark.parametrize(
    "updatequery, endpoints",
    [
        (testconsts.insertquery3, testconsts.KG_ENDPOINT),
    ]
)
def test_retrieve_metadata(updatequery, endpoints, initialise_client):
    """
    Tests that agent.ifc2gltf.kghelper retrieves and classifies the metadata of assets accurately
    """
     # Generate the test IFC triples
    kg_client = initialise_client
    kg_client.execute_update(updatequery)
    # Execute method to retrieve metadata
    result = retrieve_metadata(endpoints, endpoints)
    # The column names of the result dataframe
    name_col = "name"
    file_col = "file"
    # Assert if the row correspond with the right classification
    # First retrieve the row under conditions. If any row exist and is true, return true
    assert result.loc[(result[name_col]=="Electric Wire Box") & (result[file_col]=="furniture")].any().all()
    assert result.loc[(result[name_col]=="Water Meter") & (result[file_col]=="asset1")].any().all()
    assert result.loc[(result[name_col]=="Solar Panel") & (result[file_col]=="solarpanel")].any().all()
