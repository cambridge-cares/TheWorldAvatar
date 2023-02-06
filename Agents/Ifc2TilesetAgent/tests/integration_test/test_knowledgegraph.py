"""
# Author: qhouyee #

A test suite for the agent.kgutils.kgclient submodule.
"""
# Third party import
import pytest

# Self import
from . import testconsts

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
